pub mod disassembly;
pub mod execution;

use std::cell::Cell;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::semantic::table::{
    ExecutionError, TableErrorSub, ToTableError,
};
use crate::semantic::{self, TableError};
use crate::syntax::block;
use crate::InputSource;

use self::disassembly::Builder;

use super::disassembly::{Disassembly, DisassemblyBuilder};
use super::display::Display;
use super::execution::{Execution, ExecutionBuilder};
use super::pattern::Pattern;
use super::{FieldSize, GlobalScope, Sleigh, SolverStatus, WithBlock};

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
    export: Cell<bool>,
    export_size: Cell<FieldSize>,

    empty: RefCell<bool>,
    result: Rc<semantic::table::Table>,
}

impl std::fmt::Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Table {}, returning: {:?}",
            self.name,
            self.export_size.get(),
        )
    }
}

impl Table {
    pub fn new_empty(name: Rc<str>) -> Rc<Self> {
        let name = Rc::from(name);
        Rc::new_cyclic(|me| Self {
            name: Rc::clone(&name),
            constructors: RefCell::new(vec![]),
            result: Rc::new(semantic::table::Table::new_empty(name)),
            empty: RefCell::new(true),
            //it does not export, unless a exporting constructor is added
            export: Cell::new(false),
            export_size: Cell::default(),
            //export_time: RefCell::default(),
            me: Weak::clone(me),
        })
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
        let mut constructors = self.constructors.borrow_mut();
        //all the constructor need to export or non can export
        if constructors.is_empty() {
            //first constructor will define if this table exports or not
            if let Some(size) = constructor.export_size() {
                self.export.set(true);
                self.export_size.set(*size);
            }
        } else {
            //if this table exports, the new constructor also need to export
            match (self.export.get(), constructor.export_size()) {
                //Both do not export
                (false, None) => (),
                //Both exports
                (true, Some(_)) => (),
                (_, _) => {
                    //TODO find the export statement src
                    return Err(ExecutionError::InvalidExport(
                        constructor.src.clone(),
                    ))
                    .to_table(constructor.src);
                }
            }
        }
        constructors.push(constructor);
        Ok(())
    }
    pub fn export_size_capable<'a>(&'a self) -> Option<&'a Cell<FieldSize>> {
        self.export.get().then(|| &self.export_size)
    }
    pub fn export_size(&self) -> Option<FieldSize> {
        self.export.get().then(|| self.export_size.get())
    }
    pub fn update_export_size<F>(&self, mut action: F) -> Option<bool>
    where
        F: FnMut(FieldSize) -> Option<FieldSize>,
    {
        let old_size = self.export_size.get();
        let new_size = action(old_size)?;
        self.export_size.set(new_size);
        Some(new_size != old_size)
    }
    pub fn reference(&self) -> Rc<semantic::table::Table> {
        Rc::clone(&self.result)
    }
    pub fn solve<T>(&self, solved: &mut T) -> Result<(), TableError>
    where
        T: SolverStatus + Default,
    {
        self.constructors
            .borrow_mut()
            .iter_mut()
            .map(|constructor| constructor.solve(solved))
            .collect::<Result<(), _>>()?;

        let mut new_size = if self.export.get() {
            self.export_size.get()
        } else {
            return Ok(());
        };
        //find the sizes of all contructors
        for con in self.constructors.borrow().iter() {
            let (src, size) = if con.export_size().is_some() {
                (&con.src, con.export_size().unwrap())
            } else {
                continue;
            };
            new_size
                .update_action(|new_size| new_size.intersection(*size))
                .ok_or_else(|| TableError {
                    table_pos: src.clone(),
                    sub: TableErrorSub::TableConstructorExportSizeInvalid,
                })?;
        }

        //update all the constructors
        for con in self.constructors.borrow_mut().iter_mut() {
            let (src, size) = if con.export_size().is_some() {
                (con.src.clone(), con.mut_export_size().unwrap())
            } else {
                continue;
            };
            if size
                .update_action(|size| size.intersection(new_size))
                .unwrap()
            {
                solved.i_did_a_thing();
            }
            if size.is_undefined() {
                solved.iam_not_finished_location(&src);
            }
        }
        //update the export size
        if self.export_size.get() != new_size {
            self.export_size.set(new_size);
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(&self) -> Rc<semantic::table::Table> {
        let empty = *self.empty.borrow();
        if empty {
            let constructors = self
                .constructors
                .borrow_mut()
                .drain(..)
                .map(|x| x.convert())
                .collect();
            *self.empty.borrow_mut() = true;
            let mut result = self.result.constructors.borrow_mut();
            *result = constructors;
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
    pub fn export_size(&self) -> Option<&FieldSize> {
        self.execution.as_ref().map(|exe| exe.size()).flatten()
    }
    pub fn mut_export_size(&mut self) -> Option<&mut FieldSize> {
        self.execution.as_mut().map(|exe| exe.mut_size()).flatten()
    }
    pub fn solve<T>(&mut self, solved: &mut T) -> Result<(), TableError>
    where
        T: SolverStatus + Default,
    {
        self.disassembly.solve(solved).to_table(self.src.clone())?;
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

        semantic::table::Constructor {
            pattern,
            display,
            disassembly,
            execution,
        }
    }
}

impl<'a> Sleigh<'a> {
    pub(crate) fn insert_table_constructor(
        &mut self,
        with_block: &Option<WithBlock>,
        constructor: block::table::Constructor<'a>,
    ) -> Result<(), TableError> {
        let table_pos = self.input_src(constructor.src);
        let is_root = constructor.is_root();
        //if this is root table (instructions) check the with block table first,
        //otherwise use the table directly
        let table = match (is_root, with_block) {
            (true, Some(block)) => Rc::clone(&block.table),
            _ => self
                .get_table_or_create_empty(constructor.table_name())
                .ok_or(
                    TableErrorSub::TableNameInvalid.to_table(table_pos.clone()),
                )?,
        };

        let mut pattern = with_block
            .as_ref()
            .map(|with_block| with_block.pattern.clone())
            .unwrap_or_default();
        pattern
            .extend(self, constructor.pattern)
            .to_table(table_pos.clone())?;

        let disassembly = {
            let mut disassembly = with_block
                .as_ref()
                .map(|with_block| with_block.disassembly.clone())
                .unwrap_or_default();
            let mut builder = Builder::new(self, &mut disassembly);
            if let Some(input) = constructor.dissasembly {
                builder.extend(input).to_table(table_pos.clone())?;
            }
            disassembly
        };

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
