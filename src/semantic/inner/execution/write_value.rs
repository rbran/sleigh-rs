use crate::semantic::execution::{
    WriteBitrange, WriteExeVar, WriteTable, WriteTokenField, WriteValue,
    WriteVarnode,
};
use crate::semantic::inner::Sleigh;
use crate::Span;

use super::{
    Execution, FieldSize, FieldSizeMut, FieldSizeUnmutable, WriteScope,
};

impl WriteValue {
    pub fn from_write_scope(value: WriteScope, location: Span) -> Self {
        match value {
            WriteScope::Varnode(id) => {
                Self::Varnode(WriteVarnode { location, id })
            }
            WriteScope::Bitrange(id) => {
                Self::Bitrange(WriteBitrange { location, id })
            }
            WriteScope::TokenField(id) => {
                Self::TokenField(WriteTokenField { location, id })
            }
            WriteScope::TableExport(id) => {
                Self::TableExport(WriteTable { location, id })
            }
            WriteScope::Local(id) => Self::Local(WriteExeVar { location, id }),
        }
    }
    pub fn size(&self, sleigh: &Sleigh, execution: &Execution) -> FieldSize {
        match self {
            Self::Varnode(var) => {
                FieldSize::new_bytes(sleigh.varnode(var.id).len_bytes)
            }
            Self::Bitrange(var) => {
                FieldSize::new_bits(sleigh.bitrange(var.id).bits.len())
            }
            Self::TableExport(value) => {
                let export = sleigh.table(value.id).export.borrow();
                *export.as_ref().unwrap().size().unwrap()
            }
            Self::TokenField(ass) => {
                sleigh.token_field(ass.id).exec_value_len(sleigh)
            }
            Self::Local(var) => execution.variable(var.id).size.get(),
        }
    }
    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Varnode(var) => Box::new(FieldSizeUnmutable::from(
                FieldSize::new_bytes(sleigh.varnode(var.id).len_bytes),
            )),
            Self::Bitrange(var) => Box::new(FieldSizeUnmutable::from(
                FieldSize::new_bits(sleigh.bitrange(var.id).bits.len()),
            )),
            Self::TokenField(ass) => Box::new(FieldSizeUnmutable::from(
                sleigh.token_field(ass.id).exec_value_len(sleigh),
            )),
            Self::TableExport(table) => Box::new(sleigh.table(table.id)),
            Self::Local(local) => Box::new(&execution.variable(local.id).size),
        }
    }
}
