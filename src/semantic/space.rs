use thiserror::Error;

use crate::{NumberNonZeroUnsigned, Span};

use super::GlobalElement;

#[derive(Clone, Debug, Error)]
pub enum SpaceError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
}

#[derive(Clone, Debug)]
pub struct Space {
    pub src: Span,
    pub space_type: SpaceType,
    pub wordsize: NumberNonZeroUnsigned,
    pub addr_size: NumberNonZeroUnsigned,
    //pub(crate) registers: ..,
}
impl Space {
    pub(crate) fn new(
        src: Span,
        space_type: SpaceType,
        word_bytes: NumberNonZeroUnsigned,
        addr_bytes: NumberNonZeroUnsigned,
    ) -> Self {
        Self {
            src,
            space_type,
            wordsize: word_bytes,
            addr_size: addr_bytes,
        }
    }
    pub fn can_write(&self) -> bool {
        self.space_type.can_write()
    }
    pub fn space_type(&self) -> SpaceType {
        self.space_type
    }
    ///wordsize len in bytes, AKA number of bytes that each address store,
    ///usually memory store only a u8 for each address, but kinds of memory
    ///can store more that one byte for each address.
    pub fn word_bytes(&self) -> NumberNonZeroUnsigned {
        self.wordsize
    }
    ///address len in bytes, eg 4bytes in 32bits arch and 8bytes in a 64bits
    pub fn addr_bytes(&self) -> NumberNonZeroUnsigned {
        self.addr_size
    }
    pub fn src(&self) -> &Span {
        &self.src
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum SpaceType {
    Ram,
    Rom,
    Register,
}

impl SpaceType {
    pub fn can_write(&self) -> bool {
        match self {
            SpaceType::Rom => false,
            SpaceType::Ram | SpaceType::Register => true,
        }
    }
}

impl GlobalElement<Space> {
    pub fn new_space(
        name: &str,
        src: Span,
        space_type: SpaceType,
        word_size: NumberNonZeroUnsigned,
        addr_size: NumberNonZeroUnsigned,
    ) -> Self {
        Self::new_from(name, Space::new(src, space_type, word_size, addr_size))
    }
}
