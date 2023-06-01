use crate::meaning::Meaning;
use crate::semantic::{
    AttachLiteralId, AttachNumberId, AttachVarnodeId, PrintBase, TokenId,
    ValueFmt,
};
use crate::{Endian, FieldBits, NumberNonZeroUnsigned, Sleigh, Span};

#[derive(Clone, Debug)]
pub struct Token {
    pub(crate) name: Box<str>,
    pub location: Span,
    pub len_bytes: NumberNonZeroUnsigned,
    pub endian: Endian,
}

#[derive(Clone, Debug)]
pub struct TokenField {
    pub(crate) name: Box<str>,
    pub location: Span,
    pub bits: FieldBits,
    pub token: TokenId,
    pub attach: TokenFieldAttach,
}

impl Token {
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl TokenField {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn raw_value_is_signed(&self) -> bool {
        match self.attach {
            TokenFieldAttach::NoAttach(fmt) => fmt.signed,
            TokenFieldAttach::Varnode(_)
            | TokenFieldAttach::Literal(_)
            | TokenFieldAttach::Number(_, _) => false,
        }
    }
    pub fn execution_value_is_signed(&self, sleigh: &Sleigh) -> bool {
        match self.attach {
            TokenFieldAttach::NoAttach(fmt) => fmt.signed,
            TokenFieldAttach::Varnode(_) | TokenFieldAttach::Literal(_) => {
                false
            }
            TokenFieldAttach::Number(_, value) => {
                sleigh.attach_number(value).is_signed()
            }
        }
    }

    pub fn meaning(&self) -> Meaning {
        match self.attach {
            TokenFieldAttach::NoAttach(fmt) => Meaning::NoAttach(fmt),
            TokenFieldAttach::Varnode(id) => Meaning::Varnode(id),
            TokenFieldAttach::Literal(id) => Meaning::Literal(id),
            TokenFieldAttach::Number(base, id) => Meaning::Number(base, id),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenFieldAttach {
    /// No attach, just use the raw value
    NoAttach(ValueFmt),
    Varnode(AttachVarnodeId),
    Literal(AttachLiteralId),
    Number(PrintBase, AttachNumberId),
}
