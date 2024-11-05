use crate::semantic::execution::WriteValue;
use crate::semantic::inner::Sleigh;

use super::{Execution, FieldSize, FieldSizeMut, FieldSizeUnmutable};

impl WriteValue {
    pub fn size(&self, sleigh: &Sleigh, execution: &Execution) -> FieldSize {
        match self {
            Self::Varnode(var) => {
                FieldSize::new_bytes(sleigh.varnode(*var).len_bytes)
            }
            Self::Bitrange(var) => {
                FieldSize::new_bits(sleigh.bitrange(*var).bits.len())
            }
            Self::TableExport(value) => {
                let export = sleigh.table(*value).export.borrow();
                *export.as_ref().unwrap().size().unwrap()
            }
            Self::TokenField { attach_id, .. } => FieldSize::new_bytes(
                sleigh.attach_varnodes_len_bytes(*attach_id),
            ),
            Self::Local { id, creation: _ } => {
                execution.variable(*id).size.get()
            }
        }
    }
    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Varnode(var) => Box::new(FieldSizeUnmutable::from(
                FieldSize::new_bytes(sleigh.varnode(*var).len_bytes),
            )),
            Self::Bitrange(var) => Box::new(FieldSizeUnmutable::from(
                FieldSize::new_bits(sleigh.bitrange(*var).bits.len()),
            )),
            Self::TokenField { attach_id, .. } => {
                Box::new(FieldSizeUnmutable::from(FieldSize::new_bytes(
                    sleigh.attach_varnodes_len_bytes(*attach_id),
                )))
            }
            Self::TableExport(table) => Box::new(sleigh.table(*table)),
            Self::Local { id, creation: _ } => {
                Box::new(&execution.variable(*id).size)
            }
        }
    }
}
