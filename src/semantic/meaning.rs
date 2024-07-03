use crate::semantic::{
    AttachLiteralId, AttachNumberId, AttachVarnodeId, VarnodeId,
};
use crate::{Number, NumberNonZeroUnsigned};

use super::inner::execution::FieldSize;
use super::{PrintBase, ValueFmt};

/// The value is translated with a varnode using this value as index.
/// In Display, print the varnode name.
/// In Disassembly, TODO: is unknown.
/// In Execution, read/write are done on the underlying varnode.
#[derive(Clone, Debug)]
pub struct AttachVarnode(pub Box<[(usize, VarnodeId)]>);
impl AttachVarnode {
    #[deprecated]
    pub(crate) fn execution_len(
        &self,
        sleigh: &super::inner::Sleigh,
    ) -> FieldSize {
        //all varnodes have the same len
        let varnode_bytes = sleigh.varnode(self.0[0].1).len_bytes;
        FieldSize::new_bytes(varnode_bytes)
    }
}

/// The Value is translated into this string. Only affect the value when it's
/// printed
#[derive(Clone, Debug)]
pub struct AttachLiteral(pub Box<[(usize, String)]>);

/// The Value is translated into a signed value with this Format.
/// In Display, print the translated value using this index.
/// In Disassembly, TODO: is unknown.
/// In Execution, the value is translanted and is automatically expanded to
/// the required len.
#[derive(Clone, Debug)]
pub struct AttachNumber(pub Box<[(usize, Number)]>);
impl AttachNumber {
    #[deprecated]
    pub(crate) fn execution_len(&self) -> FieldSize {
        let len_bits = self
            .0
            .iter()
            .map(|(_i, v)| v.bits_required())
            .max()
            .unwrap();
        let len_bits = NumberNonZeroUnsigned::new(len_bits.into()).unwrap();
        FieldSize::default().set_min_bits(len_bits).unwrap()
    }
    pub fn is_signed(&self) -> bool {
        self.0.iter().any(|(_i, v)| v.is_negative())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Meaning {
    NoAttach(ValueFmt),
    Varnode(AttachVarnodeId),
    Literal(AttachLiteralId),
    Number(PrintBase, AttachNumberId),
}

impl Meaning {
    pub fn is_varnode(&self) -> bool {
        matches!(self, Self::Varnode(_))
    }
    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal(_))
    }
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(_, _))
    }
    #[deprecated]
    pub fn execution_len(
        &self,
        sleigh: &super::inner::Sleigh,
    ) -> Option<FieldSize> {
        match self {
            // name don't change the value size
            Meaning::NoAttach(_) | Meaning::Literal(_) => None,
            Meaning::Varnode(vars_id) => {
                let vars = sleigh.attach_varnode(*vars_id);
                let varnode_bits =
                    sleigh.varnode(vars.0[0].1).len_bytes.get() * 8;
                Some(FieldSize::new_bits(varnode_bits.try_into().unwrap()))
            }
            Meaning::Number(_, values_id) => {
                let values = sleigh.attach_number(*values_id);
                let len_bits = values
                    .0
                    .iter()
                    .map(|(_i, v)| v.bits_required())
                    .max()
                    .unwrap();
                let len_bits =
                    NumberNonZeroUnsigned::new(len_bits.into()).unwrap();
                Some(FieldSize::default().set_min_bits(len_bits).unwrap())
            }
        }
    }
}
