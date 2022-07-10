use crate::semantic::inner::FieldSize;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

use thiserror::Error;

use crate::base::{IntTypeU, NonZeroTypeU};
use crate::BitRange;
use crate::InputSource;

use super::space::Space;
use super::{Meaning, PrintFmt};

#[derive(Clone, Debug, Error)]
pub enum VarnodeError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
    #[error("Missing Ref {0}")]
    MissingRef(InputSource),
}

#[derive(Clone, Debug)]
pub struct Varnode {
    pub name: Rc<str>,
    pub varnode_type: VarnodeType,
    me: Weak<Self>,
}

impl Varnode {
    pub fn new_memory(
        name: &str,
        offset: IntTypeU,
        size: NonZeroTypeU,
        space: &Rc<Space>,
    ) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            varnode_type: VarnodeType::Memory(VarnodeMemory {
                offset,
                size,
                space: Rc::clone(space),
            }),
            me: Weak::clone(me),
        })
    }
    pub fn new_bitrange(
        name: &str,
        range: BitRange,
        varnode: Rc<Varnode>,
    ) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            varnode_type: VarnodeType::BitRange(VarnodeField {
                range,
                varnode,
            }),
            me: Weak::clone(me),
        })
    }
    pub fn new_context(
        name: &str,
        range: BitRange,
        varnode: Rc<Varnode>,
        fmt: PrintFmt,
        signed: bool,
        noflow: bool,
    ) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            varnode_type: VarnodeType::Context(Context {
                bitrange: VarnodeField { range, varnode },
                fmt,
                signed,
                noflow,
                attach: RefCell::new(None),
            }),
            me: Weak::clone(me),
        })
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    pub fn src_varnode(&self) -> &Varnode {
        match &self.varnode_type {
            VarnodeType::Memory(_) => &self,
            VarnodeType::BitRange(VarnodeField { varnode, .. })
            | VarnodeType::Context(Context {
                bitrange: VarnodeField { varnode, .. },
                ..
            }) => match &varnode.varnode_type {
                VarnodeType::Memory(_) => &varnode,
                _ => unreachable!(),
            },
        }
    }
    pub fn context(&self) -> Option<&Context> {
        match &self.varnode_type {
            VarnodeType::Context(ctx) => Some(ctx),
            _ => None,
        }
    }
    pub fn memory(&self) -> &VarnodeMemory {
        match &self.src_varnode().varnode_type {
            VarnodeType::Memory(x) => x,
            _ => unreachable!(),
        }
    }
    pub fn space(&self) -> &Rc<Space> {
        &self.memory().space
    }
    pub fn value_bits(&self) -> NonZeroTypeU {
        use VarnodeType::*;
        match &self.varnode_type {
            BitRange(bit_range) => bit_range.value_bits(),
            Context(context) => context.value_bits(),
            Memory(mem) => mem.value_bits(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum VarnodeType {
    //TODO memory that have context pointing to it is also context?
    Memory(VarnodeMemory),
    BitRange(VarnodeField),
    Context(Context),
}

impl VarnodeType {
    pub fn is_memory(&self) -> bool {
        matches!(self, Self::Memory(_))
    }
    pub fn is_bit_range(&self) -> bool {
        matches!(self, Self::BitRange(_))
    }
    pub fn is_context(&self) -> bool {
        matches!(self, Self::Context(_))
    }
}

#[derive(Clone, Debug)]
pub struct VarnodeField {
    pub range: BitRange,
    pub varnode: Rc<Varnode>,
}
impl VarnodeField {
    pub fn value_size(&self) -> FieldSize {
        //otherwise, it can have any size, with the min of context bits
        FieldSize::new_unsized().set_min(self.value_bits()).unwrap()
    }
    pub fn value_bits(&self) -> NonZeroTypeU {
        self.range.size()
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub bitrange: VarnodeField,
    pub fmt: PrintFmt,
    pub signed: bool,
    pub noflow: bool,
    pub attach: RefCell<Option<Rc<Meaning>>>,
}

impl Context {
    fn value_size(&self) -> FieldSize {
        match self.attach.borrow().as_ref() {
            Some(meaning) if meaning.size().is_some() => {
                //if associated with reg, it have the reg size
                FieldSize::new_bits(meaning.size().unwrap())
            }
            _ => {
                //otherwise, it can have any size, with the min of context bits
                self.bitrange.value_size()
            }
        }
    }
    pub fn value_bits(&self) -> NonZeroTypeU {
        match self.attach.borrow().as_ref() {
            Some(meaning) if meaning.size().is_some() => {
                meaning.size().unwrap()
            }
            _ => self.bitrange.value_bits(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VarnodeMemory {
    /// Offset (address) of this varnode in the Address Space
    pub offset: IntTypeU,
    /// Size of the varnode in bytes
    pub size: NonZeroTypeU,
    /// AddressSpace this varnode belongs to
    pub space: Rc<Space>,
    //pub(crate) fields: Vec<Rc<BitRange>>,
    //pub(crate) contexts: Vec<Rc<Context>>,
}

impl VarnodeMemory {
    fn value_size(&self) -> FieldSize {
        FieldSize::new_bytes(self.size)
    }
    fn value_bytes(&self) -> NonZeroTypeU {
        self.size
    }
    fn value_bits(&self) -> NonZeroTypeU {
        NonZeroTypeU::new(self.size.get().checked_mul(8).unwrap()).unwrap()
    }
}
