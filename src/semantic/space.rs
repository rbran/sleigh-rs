use crate::{NumberNonZeroUnsigned, Span};

#[derive(Clone, Debug)]
pub struct Space {
    pub src: Span,
    pub space_type: SpaceType,
    ///wordsize len in bytes, AKA number of bytes that each address store,
    ///usually memory store only a u8 for each address, but kinds of memory
    ///can store more that one byte for each address.
    pub wordsize: NumberNonZeroUnsigned,
    ///address len in bytes, eg 4bytes in 32bits arch and 8bytes in a 64bits
    pub addr_bytes: NumberNonZeroUnsigned,
}
impl Space {
    pub fn can_write(&self) -> bool {
        self.space_type.can_write()
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
