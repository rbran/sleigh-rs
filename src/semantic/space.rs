use crate::semantic::inner::FieldSize;
use std::rc::Rc;

use thiserror::Error;

use crate::base::NonZeroTypeU;
use crate::InputSource;

#[derive(Clone, Debug, Error)]
pub enum SpaceError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
}

#[derive(Clone, Debug)]
pub struct Space {
    pub name: Rc<str>,
    pub space_type: SpaceType,
}
impl Space {
    pub fn memory(&self) -> &Memory {
        match &self.space_type {
            SpaceType::Ram(mem)
            | SpaceType::Rom(mem)
            | SpaceType::Register(mem) => &mem,
        }
    }
}

#[derive(Clone, Debug)]
pub enum SpaceType {
    Ram(Memory),
    Rom(Memory),
    Register(Memory),
}

impl SpaceType {
    pub fn can_read(&self) -> bool {
        matches!(self, Self::Ram(_) | Self::Rom(_) | Self::Register(_))
    }
    pub fn can_write(&self) -> bool {
        matches!(self, Self::Ram(_) | Self::Register(_))
    }
}

#[derive(Clone, Debug)]
pub struct Memory {
    //num of bytes for the wordsize
    pub wordsize: NonZeroTypeU,
    //num of bytes for the address size
    pub addr_size: NonZeroTypeU,
    //pub(crate) registers: RefCell<Vec<Rc<Register>>>,
}

impl Memory {
    pub fn addr_len(&self) -> NonZeroTypeU {
        self.addr_size
    }
    pub fn addr_size(&self) -> FieldSize {
        FieldSize::new_bytes(self.addr_size)
    }
}
