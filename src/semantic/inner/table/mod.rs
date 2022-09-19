pub mod disassembly;
pub mod execution;

use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::semantic::table::{ExecutionError, TableErrorSub, ToTableError};
use crate::semantic::{self, TableError};
use crate::syntax::block;
use crate::{InputSource, IDENT_INSTRUCTION};

use super::disassembly::Disassembly;
use super::display::Display;
use super::execution::ExecutionExport;
use super::execution::{Execution, ExecutionBuilder};
use super::pattern::{Pattern, PatternConstraint, TablePatternLen};
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

    pattern_len: RefCell<TablePatternLen>,

    //used to differ empty tables and ones that got converted
    converted: RefCell<bool>,
    result: Rc<semantic::table::Table>,
}

impl std::fmt::Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Table {}, pattern_len: {:?}, returning: {:?}",
            self.name,
            self.pattern_len(),
            self.export
        )
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
            pattern_len: RefCell::default(),
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
    pub fn solve<T>(&self, solved: &mut T) -> Result<(), TableError>
    where
        T: SolverStatus + Default,
    {
        //FUTURE: convert into iterator with try_reduce
        //solve all the pattern len
        {
            let mut constructors = self.constructors.borrow_mut();
            let mut len_iter = constructors
                .iter_mut()
                .map(|cons| cons.solve(solved).map(|_| cons.pattern.len()));
            if let Some(len) = len_iter.next() {
                let mut len: TablePatternLen = len?.into();
                for len_iter in len_iter {
                    let len_iter: TablePatternLen = len_iter?.into();
                    len = len.combine(len_iter)
                }
                let mut pattern_len = self.pattern_len.borrow_mut();
                if *pattern_len != len {
                    solved.i_did_a_thing();
                    *pattern_len = len;
                }
            }
        }

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
    pub fn pattern_len(&self) -> TablePatternLen {
        *self.pattern_len.borrow()
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
        let pattern_len = self.pattern_len().try_into().unwrap(/*TODO*/);
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
            (*ptr).pattern_len = pattern_len;
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
    pub src: InputSource,
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
    pub fn solve<T>(&mut self, solved: &mut T) -> Result<(), TableError>
    where
        T: SolverStatus + Default,
    {
        self.pattern.solve(solved).to_table(self.src.clone())?;
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
