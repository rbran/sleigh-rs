use thiserror::Error;

use crate::base::NonZeroTypeU;
use crate::InputSource;

use super::GlobalElement;

#[derive(Clone, Debug, Error)]
pub enum SpaceError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
}

#[derive(Clone, Debug)]
pub struct Space {
    pub location: InputSource,
    pub space_type: SpaceType,
    pub wordsize: NonZeroTypeU,
    pub addr_size: NonZeroTypeU,
    //pub(crate) registers: ..,
}
impl Space {
    pub(crate) fn new(
        src: InputSource,
        space_type: SpaceType,
        word_bytes: NonZeroTypeU,
        addr_bytes: NonZeroTypeU,
    ) -> Self {
        Self {
            location: src,
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
    pub fn word_bytes(&self) -> NonZeroTypeU {
        self.wordsize
    }
    ///address len in bytes, eg 4bytes in 32bits arch and 8bytes in a 64bits
    pub fn addr_bytes(&self) -> NonZeroTypeU {
        self.addr_size
    }
    pub fn src(&self) -> &InputSource {
        &self.location
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
            SpaceType::Ram |
            SpaceType::Register => true,
        }
    }
}

impl GlobalElement<Space> {
    pub fn new_space(
        name: &str,
        src: InputSource,
        space_type: SpaceType,
        word_size: NonZeroTypeU,
        addr_size: NonZeroTypeU,
    ) -> Self {
        Self::new_from(name, Space::new(src, space_type, word_size, addr_size))
    }
}
