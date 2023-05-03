use thiserror::Error;

use crate::BitRange;
use crate::NumberNonZeroUnsigned;
use crate::NumberUnsigned;
use crate::Span;

use super::meaning::Meaning;
use super::space::Space;
use super::GlobalElement;

#[derive(Clone, Debug, Error)]
pub enum VarnodeError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),
    #[error("Attach value/register/name to Context multiple times")]
    AttachMultiple(Span),
}

#[derive(Clone, Debug)]
pub struct Bitrange {
    pub location: Span,
    pub range: BitRange,
    pub varnode: GlobalElement<Varnode>,
}
impl Bitrange {
    pub fn location(&self) -> &Span {
        &self.location
    }
    pub fn range(&self) -> &BitRange {
        &self.range
    }
    pub fn varnode(&self) -> &GlobalElement<Varnode> {
        &self.varnode
    }
}
impl GlobalElement<Bitrange> {
    pub(crate) fn new_bitrange(
        name: &str,
        src: Span,
        range: BitRange,
        varnode: GlobalElement<Varnode>,
    ) -> Self {
        Self::new_from(
            name,
            Bitrange {
                location: src,
                range,
                varnode,
            },
        )
    }
}
#[derive(Clone, Debug)]
pub struct Context {
    pub location: Span,
    pub range: BitRange,
    pub varnode: GlobalElement<Varnode>,
    pub noflow: bool,
    pub meaning: Meaning,
}
impl Context {
    pub fn location(&self) -> &Span {
        &self.location
    }
    pub fn range(&self) -> &BitRange {
        &self.range
    }
    pub fn varnode(&self) -> &GlobalElement<Varnode> {
        &self.varnode
    }
    pub fn noflow(&self) -> bool {
        self.noflow
    }
    pub fn meaning(&self) -> &Meaning {
        &self.meaning
    }
}

#[derive(Clone, Debug)]
pub struct Varnode {
    pub location: Span,
    /// Offset (address) of this varnode in the Address Space
    pub offset: NumberUnsigned,
    /// Size of the varnode in bytes
    pub len_bytes: NumberNonZeroUnsigned,
    /// AddressSpace this varnode belongs to
    pub space: GlobalElement<Space>,
    //pub(crate) fields: Box<[Rc<BitRange>]>,
    //pub(crate) contexts: Box<[Rc<Context>]>,
}

impl Varnode {
    pub const fn location(&self) -> &Span {
        &self.location
    }
    pub const fn offset(&self) -> NumberUnsigned {
        self.offset
    }
    pub const fn len_bytes(&self) -> NumberNonZeroUnsigned {
        self.len_bytes
    }
    pub const fn space(&self) -> &GlobalElement<Space> {
        &self.space
    }
}

impl GlobalElement<Varnode> {
    pub(crate) fn new_varnode(
        name: &str,
        src: Span,
        offset: NumberUnsigned,
        len_bytes: NumberNonZeroUnsigned,
        space: GlobalElement<Space>,
    ) -> Self {
        Self::new_from(
            name,
            Varnode {
                location: src,
                offset,
                len_bytes,
                space,
            },
        )
    }
}
