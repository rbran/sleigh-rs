use std::rc::Rc;

use thiserror::Error;

use crate::{from_error, Span, NumberNonZeroUnsigned};

pub use super::disassembly::DisassemblyError;
pub use super::display::{Display, DisplayError};
use super::execution::Execution;
pub use super::execution::ExecutionError;
use super::pattern::PatternLen;
pub use super::pattern::{Pattern, PatternError};

//pub mod disassembly;
//pub mod execution;

//TODO not all table errors have a location
#[derive(Clone, Debug, Error)]
#[error("at {table_pos}\n{sub}")]
pub struct TableError {
    pub table_pos: Span,
    pub sub: TableErrorSub,
}

pub trait ToTableError<X> {
    fn to_table(self, table_pos: Span) -> Result<X, TableError>;
}
impl<X, T> ToTableError<X> for Result<X, T>
where
    T: Into<TableErrorSub>,
{
    fn to_table(self, table_pos: Span) -> Result<X, TableError> {
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
    pub fn to_table(self, table_pos: Span) -> TableError {
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

#[derive(Clone, Copy, Debug, Default)]
pub enum ExecutionExport {
    //don't return
    #[default]
    None,
    //value that is known at Dissassembly time
    Const(NumberNonZeroUnsigned),
    //value that can be know at execution time
    Value(NumberNonZeroUnsigned),
    //References/registers and other mem locations, all with the same size
    Reference(NumberNonZeroUnsigned),
    //multiple source, can by any kind of return, value or address,
    //but all with the same size
    Multiple(NumberNonZeroUnsigned),
}

impl ExecutionExport {
    pub fn len(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::None => None,
            Self::Const(len)
            | Self::Value(len)
            | Self::Reference(len)
            | Self::Multiple(len) => Some(*len),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Constructor {
    pub pattern: Pattern,
    pub display: Display,
    pub execution: Option<Execution>,
    pub src: Span,
}

#[derive(Clone, Debug)]
pub struct Table {
    is_root: bool,
    pub constructors: Box<[Rc<Constructor>]>,
    pub export: ExecutionExport,
    pub(crate) pattern_len: PatternLen,
}

impl Table {
    pub(crate) fn new_dummy(is_root: bool) -> Self {
        Self {
            is_root,
            constructors: Box::new([]),
            export: ExecutionExport::None,
            pattern_len: PatternLen::Defined(0 /*TODO*/),
        }
    }
    pub fn export(&self) -> &ExecutionExport {
        &self.export
    }
    pub fn is_root(&self) -> bool {
        self.is_root
    }
    pub fn pattern_len(&self) -> &PatternLen {
        &self.pattern_len
    }
}
