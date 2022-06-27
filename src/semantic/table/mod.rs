use std::cell::RefCell;
use std::rc::Rc;

use thiserror::Error;

use crate::IDENT_INSTRUCTION;
use crate::{from_error, InputSource};

use super::disassembly::Disassembly;
pub use super::disassembly::DisassemblyError;
pub use super::display::{Display, DisplayError};
use super::execution::Execution;
pub use super::execution::ExecutionError;
use super::inner;
pub use super::pattern::{Pattern, PatternError};

//pub mod disassembly;
//pub mod execution;

#[derive(Clone, Debug, Error)]
#[error("at {table_pos}\n{sub}")]
pub struct TableError {
    pub table_pos: InputSource,
    pub sub: TableErrorSub,
}

pub trait ToTableError<X> {
    fn to_table(self, table_pos: InputSource) -> Result<X, TableError>;
}
impl<X, T> ToTableError<X> for Result<X, T>
where
    T: Into<TableErrorSub>,
{
    fn to_table(self, table_pos: InputSource) -> Result<X, TableError> {
        self.map_err(|e| TableError {
            table_pos,
            sub: e.into(),
        })
    }
}

#[derive(Clone, Debug, Error)]
pub enum TableErrorSub {
    #[error("Table Constructor can't be inserted in invalid Table name")]
    TableNameInvalid,

    #[error("Table Constructor have invalid Export size")]
    TableConstructorExportSizeInvalid,

    #[error("Pattern Error: {0}")]
    Pattern(PatternError),
    #[error("Disassembly Error: {0}")]
    Disassembly(DisassemblyError),
    #[error("Display Error: {0}")]
    Display(DisplayError),
    #[error("Execution Error: {0}")]
    Execution(ExecutionError),
}
impl TableErrorSub {
    pub fn to_table(self, table_pos: InputSource) -> TableError {
        TableError {
            table_pos,
            sub: self,
        }
    }
}
from_error!(TableErrorSub, DisassemblyError, Disassembly);
from_error!(TableErrorSub, PatternError, Pattern);
from_error!(TableErrorSub, DisplayError, Display);
from_error!(TableErrorSub, ExecutionError, Execution);

#[derive(Clone, Debug)]
pub struct Constructor {
    pub pattern: Pattern,
    pub display: Display,
    pub disassembly: Disassembly,
    pub execution: Option<Execution>,
}

#[derive(Clone, Debug)]
pub struct Table {
    pub name: Rc<str>,
    pub constructors: RefCell<Vec<Constructor>>,
}

impl Table {
    pub fn is_root(&self) -> bool {
        self.name.as_ref() == IDENT_INSTRUCTION
    }
    pub fn new_empty(name: Rc<str>) -> Self {
        Self {
            name,
            constructors: RefCell::new(vec![]),
        }
    }
}

impl<'a> TryFrom<inner::Constructor> for Constructor {
    type Error = TableError;

    fn try_from(value: inner::Constructor) -> Result<Self, Self::Error> {
        let pattern = value.pattern.try_into().to_table(value.src)?;
        let display = value.display.into();
        let execution = value.execution.map(|x| x.convert());
        let disassembly = value.disassembly.convert();
        Ok(Self {
            pattern,
            display,
            execution,
            disassembly,
        })
    }
}
