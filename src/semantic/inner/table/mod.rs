pub mod execution;

use std::cell::{Cell, RefCell};
use std::ops::ControlFlow;
use std::rc::{Rc, Weak};

use crate::semantic::pattern::PatternLen;
use crate::semantic::table::{ExecutionError, TableErrorSub, ToTableError};
use crate::semantic::{self, GlobalElement, GlobalReference, TableError};
use crate::syntax::block;
use crate::Span;

use super::disassembly;
use super::display::Display;
use super::execution::{Execution, ExecutionBuilder, ExecutionExport};
use super::pattern::{Pattern, PatternConstraint};
use super::{
    FieldSize, FieldSizeMut, GlobalConvert, GlobalScope, Sleigh, SolverStatus,
    WithBlockCurrent,
};

#[derive(Clone)]
pub struct Table {
    me: Weak<Self>,
    is_root: bool,
    pub constructors: RefCell<Vec<Constructor>>,
    /* TODO:
     * export need to be identified by type, such Varnode, Const, Value, etc.
     * and should by identified by context of its use.
     * eg: in `ARMinstructions.sinc` the `rn` table returns a value or a
     * register depending if the token `c1619=15` or not.
     * With this diference being clear on the `addrmode2`.
     * In this case, if the context is `c1619=15`, `rn` returns a value,
     * otherwise it returns a varnode. */
    //None means no constructors is able to define the return type, because
    //this table is empty or contructors are all `unimpl`.
    //Some(ExecutionExport::None) mean that this table doesn't export
    export: RefCell<Option<ExecutionExport>>,

    //HACK: pattern indirect recursion helper
    pattern_recursion_checked: RefCell<bool>,

    pattern_len: Cell<Option<PatternLen>>,

    //used to differ empty tables and ones that got converted
    converted: RefCell<bool>,
    result: Rc<semantic::table::Table>,
}

impl std::fmt::Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Table returning: {:?}", self.export)
    }
}

impl Table {
    pub fn new_empty(is_root: bool) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            is_root,
            constructors: RefCell::new(vec![]),
            result: Rc::new(semantic::table::Table::new_dummy(is_root)),
            converted: RefCell::new(false),
            //it does not export, unless a exporting constructor is added
            export: RefCell::default(),
            //HACK
            pattern_recursion_checked: RefCell::new(false),
            pattern_len: Cell::default(),
            //export_time: RefCell::default(),
            me: Weak::clone(me),
        })
    }
    pub fn is_root(&self) -> bool {
        self.is_root
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    pub fn add_constructor(
        &self,
        constructor: Constructor,
    ) -> Result<(), TableError> {
        //all the constructor need to export or none can export
        //if this constructor is not `unimpl` update/verify the return type
        if let Some(execution) = constructor.execution() {
            let mut export = self.export.borrow_mut();
            if let Some(export) = export.as_mut() {
                //if this table exports, the new constructor need to be
                //compatible
                *export = export.combine(*execution.return_type()).ok_or_else(
                    || {
                        ExecutionError::InvalidExport
                            .to_table(constructor.src.clone())
                    },
                )?;
            } else {
                //first constructor will define the table export type
                *export = Some(execution.return_type().clone());
            }
        }
        self.constructors.borrow_mut().push(constructor);
        Ok(())
    }
    pub fn export(&self) -> &RefCell<Option<ExecutionExport>> {
        &self.export
    }
    pub fn reference(&self) -> Rc<semantic::table::Table> {
        Rc::clone(&self.result)
    }
    //HACK
    pub fn pattern_indirect_recursion(&self) -> ControlFlow<Vec<Rc<Table>>> {
        use semantic::inner::pattern::PatternWalker;
        struct FindIndirectRecursion(*const Table);
        impl PatternWalker<Vec<Rc<Table>>> for FindIndirectRecursion {
            fn table(
                &mut self,
                table: &GlobalReference<Table>,
            ) -> ControlFlow<Vec<Rc<Table>>> {
                if table.element_ptr() == self.0 {
                    //we don't care about self reference, only indirect ones
                    return ControlFlow::Continue(());
                }
                table.element().pattern_indirect_recursion()
            }
        }
        //This borrow will detect the recursion, we hold the lock, while the
        //sub patterns are checked, we are only unable to borrow, if the lock
        //is hold by a previous interation.
        let mut checked = if let Ok(checked) =
            self.pattern_recursion_checked.try_borrow_mut()
        {
            //if this table was already checked, then we don't need to check
            //again
            if *checked {
                return ControlFlow::Continue(());
            }
            checked
        } else {
            //unable to lock, this means that we got here though some kind of
            //recursion
            return ControlFlow::Break(vec![self.me()]);
        };
        let mut find = FindIndirectRecursion(Weak::as_ptr(&self.me));
        let constructors = self.constructors.borrow();
        for constructor in constructors.iter() {
            find.pattern(&constructor.pattern)?;
        }
        //table verified all the leading branches, they never call recursives
        *checked = true;
        ControlFlow::Continue(())
    }
    pub fn pattern_len(&self) -> Option<PatternLen> {
        self.pattern_len.get()
    }
    fn solve_pattern_len<T>(&self, solved: &mut T) -> Result<(), TableError>
    where
        T: SolverStatus + Default,
    {
        //update all constructors
        self.constructors
            .borrow_mut()
            .iter_mut()
            .try_for_each(|constructor| constructor.solve_pattern(solved))?;

        //if already solved, do nothing
        if self.pattern_len.get().is_some() {
            return Ok(());
        };

        //TODO improve this to not require collect
        //all the lens from constructors
        let constructors = self.constructors.borrow();
        let lens: Result<Vec<_>, &Span> = constructors
            .iter()
            //get all the lens, returning none if is undefined len,
            //in this case abort the whole len calculation
            //indexs are 1/1
            .map(|constructor| {
                constructor
                    .pattern
                    .len()
                    .clone()
                    .ok_or_else(|| constructor.src())
            })
            .collect();
        let lens: Vec<_> = match lens {
            Ok(lens) => lens,
            Err(src) => {
                //found one undefined len, unable to calculate the table pattern
                //len range for now
                solved.iam_not_finished_location(src, file!(), line!());
                return Ok(());
            }
        };
        //the smallest possible table pattern_len, except from recursives
        let min = lens.iter().filter_map(|len| len.min()).min();

        //the biggest possible table pattern_len
        //FUTURE use try_reduce instead
        let mut iter = lens.iter().map(|len| len.max());
        let max = iter.next().unwrap().and_then(|first| {
            iter.try_fold(first, |acc, len| len.map(|len| len.max(acc)))
        });

        match (min, max) {
            //impossible to happen, only invalid logic can generate that
            (None, Some(_)) => unreachable!(),
            //no smallest, means that all constructors are recursives, what is
            //invalid, because the pattern will never end to match
            (None, None) => {
                //TODO error here
                panic!("Table is composed exclusivelly of recursive patterns");
            }
            //if there is no biggest, means at least one constructor
            //will always result in a Min (recursive).
            //if always recursive, len is Min(smallest_len)
            (Some(min), None) => {
                self.pattern_len.set(Some(PatternLen::Min(min)));
                solved.i_did_a_thing();
            }

            //if both are Some, we can return a range
            (Some(min), Some(max)) => {
                let len = PatternLen::new(min, max);
                self.pattern_len.set(Some(len));
                solved.i_did_a_thing();
            }
        }

        //TODO not really, review this check
        ////with this table len solved, all constructors shold also be able to
        ////immediatelly solve all the sub-patterns, unable to do so can only
        ////be caused by an logic error.
        //let mut solved = super::Solved::default();
        //self.constructors.borrow_mut().iter_mut().try_for_each(
        //    |constructor| constructor.solve_pattern(&mut solved),
        //)?;
        //if !solved.we_finished() {
        //    unreachable!("Table pattern len solver have a logical error")
        //}
        Ok(())
    }
    pub fn solve<T>(&self, solved: &mut T) -> Result<(), TableError>
    where
        T: SolverStatus + Default,
    {
        //empty table can't be solved
        if self.constructors.borrow().is_empty() {
            return Ok(());
        }

        //TODO only solve, if necessary
        //if self.fully_solved_already() {
        //  return Ok(());
        //}

        self.solve_pattern_len(solved)?;

        //TODO move this into solve_execution
        //update all constructors
        self.constructors
            .borrow_mut()
            .iter_mut()
            .try_for_each(|constructor| constructor.solve_execution(solved))?;
        //TODO update the FieldSize::all_same_size to use iterators and use it
        //here
        //update all the constructors return size, if none/undefined return just
        //finish solving
        let mut modified = false;
        let mut export = self.export.borrow_mut();
        let new_size: &mut FieldSize = if let Some(export) =
            export.as_mut().map(|x| x.size_mut()).flatten()
        {
            export
        } else {
            return Ok(());
        };
        //find the sizes of all contructors
        for con in self.constructors.borrow().iter() {
            //if the execution is unimpl, ignore this constructor
            let (src, size) = if let Some(exe) = con.execution() {
                (&con.src, exe.return_type().size().unwrap(/*unreachable*/))
            } else {
                continue;
            };
            modified |= new_size
                .update_action(|new_size| new_size.intersection(*size))
                .ok_or_else(|| {
                    TableErrorSub::TableConstructorExportSizeInvalid
                        .to_table(src.clone())
                })?;
        }

        //update all the constructors
        for con in self.constructors.borrow_mut().iter_mut() {
            let src = con.src.clone();
            //if the execution is unimpl, ignore this constructor
            let (src, size) = if let Some(exe) = con.execution_mut() {
                let size =
                    exe.return_type_mut().size_mut().unwrap(/*unreachable*/);
                (src, size)
            } else {
                continue;
            };
            modified |= size
                .update_action(|size| size.intersection(*new_size))
                .unwrap();
            if size.is_undefined() {
                solved.iam_not_finished_location(&src, file!(), line!());
            }
        }
        //update the export size
        if modified {
            solved.i_did_a_thing();
        }
        Ok(())
    }
}
impl GlobalConvert for Table {
    type FinalType = crate::semantic::Table;
    fn convert(&self) -> Rc<Self::FinalType> {
        let converted = *self.converted.borrow();
        if converted {
            return Rc::clone(&self.result);
        }
        //TODO better sorting method
        //put the constructors in the correct order
        let constructors: Vec<_> =
            std::mem::take(self.constructors.borrow_mut().as_mut());
        let patterns: Vec<PatternConstraint> = constructors
            .iter()
            .map(|constructor| constructor.pattern.constraint())
            .collect();
        let mut new_constructors: Vec<(Constructor, PatternConstraint)> =
            vec![];
        for (constructor, pattern) in
            constructors.into_iter().zip(patterns.into_iter())
        {
            //TODO detect conflicting instead of just looking for contains
            let pos = new_constructors.iter().enumerate().find_map(
                |(i, (_con, pat))| {
                    //TODO how to handle inter-intersections? such:
                    //first variant of self contains the second variant of other
                    //AND
                    //second variant of self contains the first variant of other
                    let ord = pattern.ordering(&pat);
                    use super::pattern::MultiplePatternOrdering as Ord;
                    match ord {
                        //new pattern is contained at least once, just skip it
                        Ord { contained: 1.., .. } => None,
                        //new pattern contains at least once, insert it first
                        Ord { contains: 1.., .. } => Some(i),
                        Ord { .. } => None,
                    }
                },
            );
            //insert constructors in the correct order accordingly with the
            //rules of `7.8.1. Matching`
            if let Some(pos) = pos {
                new_constructors.insert(pos, (constructor, pattern));
            } else {
                new_constructors.push((constructor, pattern));
            }
        }

        *self.converted.borrow_mut() = true;
        let export =
            self.export.borrow().unwrap_or_default().convert().unwrap();
        let constructors = new_constructors
            .into_iter()
            .map(|(x, _)| Rc::new(x.convert()))
            .collect();

        //TODO is this really safe?
        //There are strong pointers to table, but there are no references to the
        //internal data, so modify `cosntructors/export/pattern_len` should be
        //safe
        unsafe {
            let ptr = Rc::as_ptr(&self.result);
            let ptr: *mut semantic::table::Table = std::mem::transmute(ptr);
            (*ptr).constructors = constructors;
            (*ptr).export = export;
        }

        Rc::clone(&self.result)
    }
}

impl FieldSizeMut for GlobalElement<Table> {
    fn get(&self) -> FieldSize {
        //TODO expect
        *self.export().borrow().as_ref().unwrap().size().unwrap()
    }

    fn set(&mut self, size: FieldSize) -> Option<bool> {
        let mut self_export = self.export().borrow_mut();
        let self_ref = self_export.as_mut().unwrap().size_mut().unwrap();
        let modify = *self_ref != size;
        if modify {
            let _ = std::mem::replace(self_ref, size);
        }
        Some(modify)
    }
}

pub type FinalConstructor = crate::semantic::table::Constructor;
#[derive(Clone, Debug)]
pub struct Constructor {
    //pub table: Weak<Table>,
    pub display: Display,
    pub pattern: Pattern,
    pub execution: Option<Execution>,
    src: Span,
}
impl Constructor {
    pub fn new(
        display: Display,
        pattern: Pattern,
        execution: Option<Execution>,
        src: Span,
    ) -> Self {
        Self {
            display,
            pattern,
            execution,
            src,
        }
    }
    pub fn execution(&self) -> Option<&Execution> {
        self.execution.as_ref()
    }
    pub fn execution_mut(&mut self) -> Option<&mut Execution> {
        self.execution.as_mut()
    }
    pub fn src(&self) -> &Span {
        &self.src
    }
    pub fn solve_pattern<T>(&mut self, solved: &mut T) -> Result<(), TableError>
    where
        T: SolverStatus + Default,
    {
        self.pattern.solve(solved).to_table(self.src.clone())
    }
    pub fn solve_execution<T>(
        &mut self,
        solved: &mut T,
    ) -> Result<(), TableError>
    where
        T: SolverStatus + Default,
    {
        if let Some(execution) = &mut self.execution {
            execution.solve(solved).to_table(self.src.clone())?;
        }
        Ok(())
    }
    pub fn convert(self) -> FinalConstructor {
        let display = self.display.into();
        let pattern = self.pattern.convert();
        let execution = self.execution.map(|x| x.convert());
        let src = self.src;

        semantic::table::Constructor {
            pattern,
            display,
            execution,
            src,
        }
    }
}

impl<'a> From<Constructor> for FinalConstructor {
    fn from(value: Constructor) -> Self {
        value.convert()
    }
}

impl Sleigh {
    pub(crate) fn insert_table_constructor(
        &mut self,
        with_block_current: &mut WithBlockCurrent,
        constructor: block::table::Constructor,
    ) -> Result<(), TableError> {
        let table_name =
            with_block_current.table_name(constructor.table_name());
        let table = self
            .get_table_or_create_empty(table_name)
            .ok_or(
                TableErrorSub::TableNameInvalid
                    .to_table(constructor.src.clone()),
            )?
            .clone();

        let pattern = with_block_current.pattern(&constructor.pattern);
        let mut pattern = Pattern::new(self, pattern, table.element_ptr())
            .to_table(constructor.src.clone())?;
        pattern.unresolved_token_fields().into_iter().try_for_each(
            |(_key, token_field)| {
                if pattern
                    .produce_token_field(&token_field)
                    .map(|block_num| block_num.is_none())
                    .to_table(constructor.src.clone())?
                {
                    return Err(semantic::table::PatternError::MissingRef(
                        token_field.src.clone(),
                    ))
                    .to_table(constructor.src.clone());
                }
                Ok(())
            },
        )?;

        let disassembly_raw =
            with_block_current.disassembly(constructor.disassembly);
        if let Some(disassembly_raw) = disassembly_raw {
            disassembly::Builder::new(self, &mut pattern)
                .build(disassembly_raw)
                .to_table(constructor.src.clone())?
        }

        let is_root = table.is_root();
        let display =
            Display::new(constructor.display, self, &mut pattern, is_root)
                .to_table(constructor.src.clone())?;

        let execution = constructor
            .execution
            .map(|x| -> Result<Execution, ExecutionError> {
                let mut execution = execution::Builder::new(
                    self,
                    &mut pattern,
                    &constructor.src,
                );
                execution.extend(x)?;
                Ok(execution.into())
            })
            .transpose()
            .to_table(constructor.src.clone())?;

        let constructor =
            Constructor::new(display, pattern, execution, constructor.src);
        table.add_constructor(constructor)?;
        Ok(())
    }
    pub fn get_table_or_create_empty(
        &mut self,
        name: &str,
    ) -> Option<&GlobalElement<Table>> {
        //TODO check if creating the table is always required, or we can create
        //it only if a contructor is added to it. Or just remove empty tables
        //and check the Rc counts before converting into the final sleigh struct
        let name = Rc::from(name);
        self.idents
            .entry(Rc::clone(&name))
            .or_insert_with(|| {
                let table = Table::new_empty(false);
                GlobalScope::Table(GlobalElement::new(name, table))
            })
            .table_or(())
            .ok()
    }
}
