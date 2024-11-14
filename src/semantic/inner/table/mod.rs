use std::cell::{Cell, RefCell};
use std::ops::ControlFlow;

use crate::pattern::BitConstraint;
use crate::semantic::display::Display;
use crate::semantic::pattern::PatternLen;
use crate::semantic::table::Constructor as FinalConstructor;
use crate::semantic::{Sleigh as FinalSleigh, Table as FinalTable, TableId};
use crate::table::{ConstructorId, Matcher, VariantId};
use crate::{
    syntax, ExecutionError, PatternError, SleighError, Span, TableError,
};

use super::disassembly;
use super::execution::{
    Execution, ExecutionBuilder, FieldSize, FieldSizeMut, TableExportType,
};
use super::pattern::{Pattern, PatternWalker};
use super::with_block::WithBlockCurrent;
use super::{GlobalScope, Sleigh, SolverStatus};

pub mod execution;

#[derive(Clone)]
pub struct Table {
    pub is_root: bool,
    pub name: String,
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
    pub export: RefCell<Option<TableExportType>>,

    //HACK: pattern indirect recursion helper
    pub pattern_recursion_checked: RefCell<bool>,

    pub pattern_len: Cell<Option<PatternLen>>,
}

impl std::fmt::Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Table returning: {:?}", self.export)
    }
}

//7.8.1. Matching
//one pattern contains the other if all the cases that match the contained,
//also match the pattern.
//eg: `a` contains `b` if all cases that match `b` also match `a`. In other
//words `a` is a special case of `b`.
//NOTE the opose don't need to be true.
fn is_first_then(
    constructors: &[FinalConstructor],
    matcher_a: Matcher,
    matcher_b: Matcher,
) -> bool {
    let constructor_a = &constructors[matcher_a.constructor.0];
    let constructor_b = &constructors[matcher_b.constructor.0];
    let pattern_len_a = constructor_a.pattern.bits_produced();
    let pattern_len_b = constructor_a.pattern.bits_produced();
    let pattern_len_max = pattern_len_a.max(pattern_len_b);
    let extend_len_a = pattern_len_max - pattern_len_a;
    let extend_len_b = pattern_len_max - pattern_len_b;

    fn produce_iter(
        constructor: &FinalConstructor,
        variant: VariantId,
        extend: usize,
    ) -> impl Iterator<Item = BitConstraint> + '_ {
        let context_bits = &constructor.variants_bits[variant.0].1;
        let token_bits = &constructor.variants_bits[variant.0].2;
        context_bits
            .iter()
            .chain(token_bits.iter())
            .cloned()
            .chain((0..extend).map(|_| BitConstraint::Unrestrained))
    }
    let bits_a =
        produce_iter(constructor_a, matcher_a.variant_id, extend_len_a);
    let bits_b =
        produce_iter(constructor_b, matcher_b.variant_id, extend_len_b);
    use BitConstraint::*;
    bits_a.zip(bits_b).all(|(x, y)| {
        match (x, y) {
            // matcher_a is equal to b, or is tighter, so continue testing
            (Defined(x), Defined(y)) if x == y => true,
            (Unrestrained, Unrestrained)
            | (Defined(_), Unrestrained)
            | (Restrained, Unrestrained) => true,
            // diferent, a can't contain b
            (Defined(_), Defined(_)) => false,
            // verify that this complex restraint is equal in both
            // TODO: deep test, checking if the complex constraint can be
            // compatible. For now, just assume they are different.
            (Defined(_), Restrained)
            | (Restrained, Defined(_))
            | (Restrained, Restrained) => false,
            // matcher_a don't constraint something in b.
            (Unrestrained, Restrained) | (Unrestrained, Defined(_)) => false,
        }
    })
}

impl Table {
    pub fn new_empty(is_root: bool, name: String) -> Self {
        Self {
            is_root,
            name,
            constructors: RefCell::new(vec![]),
            //it does not export, unless a exporting constructor is added
            export: RefCell::new(None),
            //HACK
            pattern_recursion_checked: RefCell::new(false),
            pattern_len: Cell::default(),
            //export_time: RefCell::default(),
        }
    }
    pub fn is_root(&self) -> bool {
        self.is_root
    }
    pub fn add_constructor(
        &mut self,
        constructor: Constructor,
    ) -> Result<(), Box<SleighError>> {
        //all the constructor need to export or none can export
        //if this constructor is not `unimpl` update/verify the return type
        if let Some(execution) = constructor.execution() {
            let mut export = self.export.borrow_mut();
            if let Some(export) = export.as_mut() {
                //if this table exports, the new constructor need to be
                //compatible
                *export = export.combine(execution.return_value).ok_or_else(
                    || {
                        Box::new(SleighError::new_table(
                            constructor.src.clone(),
                            ExecutionError::InvalidExport,
                        ))
                    },
                )?;
            } else {
                //first constructor will define the table export type
                *export = Some(execution.return_value);
            }
        }
        self.constructors.borrow_mut().push(constructor);
        Ok(())
    }
    //HACK
    pub fn pattern_indirect_recursion(
        &self,
        sleigh: &Sleigh,
        me: TableId,
    ) -> ControlFlow<Vec<TableId>> {
        struct FindIndirectRecursion<'a>(&'a Sleigh, TableId);
        impl<'a> PatternWalker<Vec<TableId>> for FindIndirectRecursion<'a> {
            fn table(&mut self, table: TableId) -> ControlFlow<Vec<TableId>> {
                if table == self.1 {
                    //we don't care about self reference, only indirect ones
                    return ControlFlow::Continue(());
                }
                self.0
                    .table(table)
                    .pattern_indirect_recursion(self.0, table)
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
            return ControlFlow::Break(vec![me]);
        };
        let mut find = FindIndirectRecursion(sleigh, me);
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
    fn solve_pattern_len<T>(
        &self,
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<(), Box<SleighError>>
    where
        T: SolverStatus + Default,
    {
        //update all constructors
        self.constructors.borrow_mut().iter_mut().try_for_each(
            |constructor| constructor.solve_pattern(sleigh, solved).map(|_| ()),
        )?;

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
                constructor.pattern.len.ok_or_else(|| constructor.src())
            })
            .collect();
        let lens: Vec<_> = match lens {
            Ok(lens) => lens,
            Err(src) => {
                //found one undefined len, unable to calculate the table pattern
                //len range for now
                solved.iam_not_finished(src, file!(), line!());
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

        Ok(())
    }
    pub fn solve<T>(
        &self,
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<(), Box<SleighError>>
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

        self.solve_pattern_len(sleigh, solved)?;

        //TODO move this into solve_execution
        //update all constructors
        self.constructors.borrow_mut().iter_mut().try_for_each(
            |constructor| constructor.solve_execution(sleigh, solved),
        )?;
        //TODO update the FieldSize::all_same_size to use iterators and use it
        //here
        //update all the constructors return size, if none/undefined return just
        //finish solving
        let mut modified = false;
        let mut export = self.export.borrow_mut();
        let Some(new_size) = export.as_mut().and_then(|x| x.size_mut()) else {
            return Ok(());
        };
        //find the sizes of all contructors
        for con in self.constructors.borrow().iter() {
            //if the execution is unimpl, ignore this constructor
            let (src, size) = if let Some(exe) = con.execution() {
                (&con.src, exe.return_value.size().unwrap(/*unreachable*/))
            } else {
                continue;
            };
            modified |= new_size
                .update_action(|new_size| new_size.intersection(*size))
                .ok_or_else(|| {
                    SleighError::new_table(
                        src.clone(),
                        TableError::TableConstructorExportSizeInvalid,
                    )
                })?;
        }

        //update all the constructors
        for con in self.constructors.borrow_mut().iter_mut() {
            let src = con.src.clone();
            //if the execution is unimpl, ignore this constructor
            let (src, size) = if let Some(exe) = con.execution_mut() {
                let size = exe.return_value.size_mut().unwrap(/*unreachable*/);
                (src, size)
            } else {
                continue;
            };
            modified |= size
                .update_action(|size| size.intersection(*new_size))
                .unwrap();
            if size.is_undefined() {
                solved.iam_not_finished(&src, file!(), line!());
            }
        }
        //update the export size
        if modified {
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(self, sleigh: &FinalSleigh) -> FinalTable {
        let constructors: Box<[_]> = self
            .constructors
            .take()
            .into_iter()
            .map(|constructor| constructor.convert(sleigh))
            .collect();
        let matchers_num =
            constructors.iter().map(|c| c.variants_bits.len()).sum();
        let mut matcher_order = Vec::with_capacity(matchers_num);

        let matcher_a_iter =
            constructors.iter().enumerate().flat_map(|(i, c)| {
                c.variants().map(move |x| Matcher {
                    constructor: ConstructorId(i),
                    variant_id: x.0,
                })
            });
        for matcher_a in matcher_a_iter {
            //TODO detect conflicting instead of just looking for contains
            let pos = matcher_order.iter().position(|matcher_b| {
                is_first_then(&constructors, matcher_a, *matcher_b)
            });
            //insert constructors in the correct order accordingly with the
            //rules of `7.8.1. Matching`
            if let Some(pos) = pos {
                matcher_order.insert(pos, matcher_a);
            } else {
                matcher_order.push(matcher_a);
            }
        }

        FinalTable {
            is_root: self.is_root,
            constructors,
            export: self
                .export
                .borrow()
                .unwrap_or(TableExportType::None)
                .convert(),
            pattern_len: self.pattern_len.get().unwrap(),
            name: self.name.into(),
            matcher_order: matcher_order.into(),
        }
    }
}

impl FieldSizeMut for &Table {
    fn get(&self) -> FieldSize {
        //TODO expect
        *self.export.borrow().as_ref().unwrap().size().unwrap()
    }

    fn set(&mut self, size: FieldSize) -> Option<bool> {
        let mut self_export = self.export.borrow_mut();
        let self_ref = self_export.as_mut().unwrap().size_mut().unwrap();
        let modify = *self_ref != size;
        if modify {
            let _ = std::mem::replace(self_ref, size);
        }
        Some(modify)
    }
}

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

    pub fn solve_pattern<T>(
        &mut self,
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<bool, Box<SleighError>>
    where
        T: SolverStatus + Default,
    {
        self.pattern
            .calculate_len(sleigh, solved)
            .map_err(|e| Box::new(SleighError::new_table(self.src.clone(), *e)))
    }

    pub fn solve_execution<T>(
        &mut self,
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<(), Box<SleighError>>
    where
        T: SolverStatus + Default,
    {
        if let Some(execution) = &mut self.execution {
            execution
                .solve(sleigh, solved)
                .map_err(|e| SleighError::new_table(self.src.clone(), *e))?
        }
        Ok(())
    }

    pub fn convert(mut self, sleigh: &FinalSleigh) -> FinalConstructor {
        self.pattern.calculate_bits(0);
        let pattern = self.pattern.convert();
        let execution = self.execution.map(|x| x.convert());
        let src = self.src;

        //TODO detect export type and apply to the disassembly, if the case

        let variants_bits = pattern
            .pattern_bits_variants(sleigh)
            .map(|(i, c, v)| (VariantId(i), c.into(), v.into()))
            .collect();

        FinalConstructor {
            pattern,
            display: self.display,
            execution,
            location: src,
            variants_bits,
        }
    }
}

impl Sleigh {
    pub(crate) fn insert_table_constructor(
        &mut self,
        with_block_current: &mut WithBlockCurrent,
        constructor: syntax::block::table::Constructor,
    ) -> Result<(), Box<SleighError>> {
        let table_name =
            with_block_current.table_name(constructor.table_name());
        let table_id =
            self.get_table_or_create_empty(table_name, &constructor.src)?;

        let pattern = with_block_current.pattern(&constructor.pattern);
        let mut pattern =
            Pattern::new(self, pattern, table_id).map_err(|e| {
                Box::new(SleighError::new_table(constructor.src.clone(), *e))
            })?;
        //TODO move this into the Pattern::new function
        pattern.unresolved_token_fields(self).try_for_each(
            |(token_field, location)| {
                let token_produced = pattern
                    .produce_token_field(self, token_field)
                    .map_err(|e| {
                        Box::new(SleighError::new_table(
                            constructor.src.clone(),
                            *e,
                        ))
                    })?;
                if token_produced.is_none() {
                    //TODO error with the list of unresolved fields instead of
                    //only the first
                    return Err(Box::new(SleighError::new_table(
                        constructor.src.clone(),
                        PatternError::MissingRef(location),
                    )));
                }
                Ok(())
            },
        )?;

        let disassembly_raw =
            with_block_current.disassembly(constructor.disassembly);
        if let Some(disassembly_raw) = disassembly_raw {
            disassembly::Builder::new(self, &mut pattern)
                .build(disassembly_raw)
                .map_err(|e| {
                    Box::new(SleighError::new_table(
                        constructor.src.clone(),
                        *e,
                    ))
                })?;
        }

        let is_root = self.instruction_table == table_id;
        let display = self
            .new_display(constructor.display, &mut pattern, is_root)
            .map_err(|e| {
                Box::new(SleighError::new_table(constructor.src.clone(), *e))
            })?;

        let execution = constructor
            .execution
            .map(|x| -> Result<Execution, Box<ExecutionError>> {
                let mut execution = execution::Builder::new(
                    self,
                    &mut pattern,
                    constructor.src.clone(),
                );
                execution.extend(x)?;
                Ok(execution.into())
            })
            .transpose()
            .map_err(|e| {
                Box::new(SleighError::new_table(constructor.src.clone(), *e))
            })?;

        let constructor =
            Constructor::new(display, pattern, execution, constructor.src);
        let table = self.table_mut(table_id);
        table.add_constructor(constructor)?;
        Ok(())
    }
    pub fn get_table_or_create_empty(
        &mut self,
        name: &str,
        location: &Span,
    ) -> Result<TableId, Box<SleighError>> {
        //TODO check if creating the table is always required, or we can create
        //it only if a contructor is added to it. Or just remove empty tables
        //and check the Rc counts before converting into the final sleigh struct
        match self.global_scope.get(name) {
            Some(GlobalScope::Table(id)) => Ok(*id),
            Some(_) => Err(Box::new(SleighError::new_table(
                location.clone(),
                TableError::TableNameInvalid,
            ))),
            None => {
                let table = Table::new_empty(false, name.to_owned());
                self.tables.push(table);
                let table_id = TableId(self.tables.len() - 1);
                self.global_scope
                    .insert(name.to_owned(), GlobalScope::Table(table_id));
                Ok(table_id)
            }
        }
    }
}
