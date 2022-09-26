pub mod disassembly;
pub mod execution;

use std::cell::{Cell, RefCell};
use std::ops::ControlFlow;
use std::rc::{Rc, Weak};

use crate::semantic::table::{ExecutionError, TableErrorSub, ToTableError};
use crate::semantic::{self, TableError};
use crate::syntax::block;
use crate::{InputSource, PatternLen, IDENT_INSTRUCTION};

use super::disassembly::Disassembly;
use super::display::Display;
use super::execution::ExecutionExport;
use super::execution::{Execution, ExecutionBuilder};
use super::pattern::{Pattern, PatternConstraint};
use super::{FieldSize, GlobalScope, Sleigh, SolverStatus, WithBlockCurrent};

#[derive(Clone)]
pub struct Table {
    me: Weak<Self>,
    pub name: Rc<str>,
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
        write!(f, "Table {}, returning: {:?}", self.name, self.export)
    }
}

impl Table {
    pub fn new_empty(name: Rc<str>) -> Rc<Self> {
        let name = Rc::from(name);
        Rc::new_cyclic(|me| Self {
            name: Rc::clone(&name),
            constructors: RefCell::new(vec![]),
            result: Rc::new(semantic::table::Table::new_dummy(name)),
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
        self.name.as_ref() == IDENT_INSTRUCTION
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    pub fn name(&self) -> &Rc<str> {
        &self.name
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
                table: &Rc<Table>,
            ) -> ControlFlow<Vec<Rc<Table>>> {
                if Rc::as_ptr(table) == self.0 {
                    //we don't care about self reference, only indirect ones
                    return ControlFlow::Continue(());
                }
                table.pattern_indirect_recursion()
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
        //if already solved, do nothing
        if self.pattern_len.get().is_some() {
            return Ok(());
        };

        //update all constructors
        self.constructors
            .borrow_mut()
            .iter_mut()
            .try_for_each(|constructor| constructor.solve_pattern(solved))?;

        //TODO improve this to not require collect
        //all the lens from constructors
        let lens: Vec<_> = {
            let constructors = self.constructors.borrow();
            let lens: Result<Vec<_>, &InputSource> = constructors
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
            match lens {
                Ok(lens) => lens,
                Err(src) => {
                    //found one undefined len, unable to calculate the table pattern len
                    //range for now
                    solved.iam_not_finished_location(src, file!(), line!());
                    return Ok(());
                }
            }
        };
        //the smallest possible table pattern_len, except from recursives
        let min = lens.iter().filter_map(|len| len.min()).min();

        //the biggest possible table pattern_len
        //FUTURE use try_reduce instead
        let max = {
            let mut iter = lens.iter().map(|len| len.max());
            if let Some(first) = iter.next().unwrap() {
                iter.try_fold(first, |acc, len| len.map(|len| len.max(acc)))
            } else {
                None
            }
        };

        match (min, max) {
            //impossible to happen, only invalid logic can generate that
            (None, Some(_)) => unreachable!(),
            //no smallest, means that all constructors are recursives, what is
            //invalid, because the pattern will never end to match
            (None, None) => {
                //TODO error here
                panic!(
                    "Table is composed exclusivelly of recursive patterns: {}",
                    self.name()
                );
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
                let len = PatternLen::new_range(min, max);
                self.pattern_len.set(Some(len));
                solved.i_did_a_thing();
            }
        }

        //with this table len solved, all constructors shold also be able to
        //immediatelly solve all the sub-patterns, unable to do so can only
        //be caused by an logic error.
        let mut solved = super::Solved::default();
        self.constructors.borrow_mut().iter_mut().try_for_each(
            |constructor| constructor.solve_pattern(&mut solved),
        )?;
        if !solved.we_finished() {
            unreachable!("Table pattern len solver have a logical error")
        }
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
    pub fn convert(&self) -> Rc<semantic::table::Table> {
        let converted = *self.converted.borrow();
        if converted {
            return self.reference();
        }
        //TODO better sorting method
        //put the constructors in the correct order
        let mut constructors = self.constructors.borrow_mut();
        let mut constructors: Vec<(Constructor, PatternConstraint)> =
            constructors
                .drain(..)
                .map(|constructor| {
                    let patt = constructor.pattern.pattern_constrait();
                    (constructor, patt)
                })
                .collect();
        let mut new_constructors: Vec<(Constructor, PatternConstraint)> =
            vec![];
        for (constructor, pattern) in constructors.drain(..) {
            let pos = new_constructors.iter().enumerate().find_map(
                |(i, (_con, pat))| pat.contains(&pattern).then_some(i),
            );
            //insert constructors in the correct order accordingly with the rules of
            //`7.8.1. Matching`
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
            .drain(..)
            .map(|(x, _)| x.convert())
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

        self.reference()
    }
}

#[derive(Clone, Debug)]
pub struct Constructor {
    //pub table: Weak<Table>,
    pub display: Display,
    pub pattern: Pattern,
    pub disassembly: Disassembly,
    pub execution: Option<Execution>,
    src: InputSource,
}
impl Constructor {
    pub fn new(
        display: Display,
        pattern: Pattern,
        disassembly: Disassembly,
        execution: Option<Execution>,
        src: InputSource,
    ) -> Self {
        Self {
            display,
            pattern,
            disassembly,
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
    pub fn src(&self) -> &InputSource {
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
    pub fn convert(self) -> semantic::table::Constructor {
        let display = self.display.into();
        let pattern = self.pattern.try_into().unwrap();
        let disassembly = self.disassembly.convert();
        let execution = self.execution.map(|x| x.convert());
        let src = self.src;

        semantic::table::Constructor {
            pattern,
            display,
            disassembly,
            execution,
            src,
        }
    }
}

impl<'a> Sleigh<'a> {
    pub(crate) fn insert_table_constructor(
        &mut self,
        with_block_current: &mut WithBlockCurrent<'a>,
        constructor: block::table::Constructor<'a>,
    ) -> Result<(), TableError> {
        let table_pos = self.input_src(constructor.src);
        let table_name =
            with_block_current.table_name(constructor.table_name());
        let table = self.get_table_or_create_empty(table_name).ok_or(
            TableErrorSub::TableNameInvalid.to_table(table_pos.clone()),
        )?;

        let pattern = with_block_current
            .pattern(constructor.pattern)
            .to_table(table_pos.clone())?;
        let pattern =
            Pattern::new(self, pattern, &table).to_table(table_pos.clone())?;

        let disassembly =
            with_block_current.disassembly(constructor.dissasembly);
        let disassembly = disassembly
            .map(|disassembly| self.table_disassembly(disassembly))
            .transpose()
            .to_table(table_pos.clone())?
            .unwrap_or_default();

        let is_root = table.is_root();
        let display =
            Display::new(constructor.display, self, &disassembly, is_root)
                .to_table(table_pos.clone())?;

        let src_table = self.input_src(constructor.src);
        let execution = constructor
            .execution
            .map(|x| -> Result<Execution, ExecutionError> {
                let mut execution =
                    execution::Builder::new(self, &disassembly, &src_table);
                execution.extend(x)?;
                Ok(execution.into())
            })
            .transpose()
            .to_table(table_pos.clone())?;

        let constructor = Constructor::new(
            display,
            pattern,
            disassembly,
            execution,
            table_pos,
        );
        table.add_constructor(constructor)?;
        Ok(())
    }
    pub fn get_table_or_create_empty(
        &mut self,
        name: &str,
    ) -> Option<Rc<Table>> {
        //TODO check if creating the table is always required, or we can create
        //it only if a contructor is added to it. Or just remove empty tables
        //and check the Rc counts before converting into the final sleigh struct
        let name = Rc::from(name);
        self.idents
            .entry(Rc::clone(&name))
            .or_insert(GlobalScope::Table(Table::new_empty(name)))
            .table_or(())
            .ok()
    }
}
