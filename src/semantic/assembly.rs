use crate::semantic::inner::FieldSize;
use core::cell::Cell;
use std::cell::RefCell;
use std::ops::Range;
use std::rc::{Rc, Weak};

use thiserror::Error;

use crate::base::{IntTypeU, NonZeroTypeU};
use crate::InputSource;

use super::{Endian, Meaning, PrintFmt};

#[derive(Clone, Debug, Error)]
pub enum AssemblyError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
    #[error("Missing Ref {0}")]
    MissingRef(InputSource),
}

#[derive(Clone, Debug)]
pub struct Assembly {
    pub name: Rc<str>,
    pub assembly_type: AssemblyType,
    me: Weak<Self>,
}

impl Assembly {
    pub fn new_field(
        name: &str,
        field_start: IntTypeU,
        field_end: IntTypeU,
        signed: bool,
        fmt: PrintFmt,
        token: &Rc<Token>,
    ) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            assembly_type: AssemblyType::Field(Field {
                bit_range: field_start..field_end + 1,
                signed,
                fmt,
                token: Rc::clone(token),
                attach: RefCell::new(None),
            }),
            me: Weak::clone(me),
        })
    }
    pub fn new_start(name: &str, size: Rc<Cell<FieldSize>>) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            assembly_type: AssemblyType::Start(size),
            me: Weak::clone(me),
        })
    }
    pub fn new_next(name: &str, size: Rc<Cell<FieldSize>>) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            assembly_type: AssemblyType::Next(size),
            me: Weak::clone(me),
        })
    }
    pub fn new_epsilon(name: &str) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            assembly_type: AssemblyType::Epsilon,
            me: Weak::clone(me),
        })
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    pub fn name(&self) -> &Rc<str> {
        &self.name
    }
    pub fn field(&self) -> Option<&Field> {
        use AssemblyType::*;
        match &self.assembly_type {
            Field(field) => Some(&field),
            Epsilon | Start(_) | Next(_) => return None,
        }
    }
    pub fn token_len(&self) -> IntTypeU {
        match &self.assembly_type {
            AssemblyType::Field(x) => x.token.size.get(),
            AssemblyType::Epsilon
            | AssemblyType::Next(_)
            | AssemblyType::Start(_) => 0,
        }
    }
    pub fn value_len(&self) -> FieldSize {
        match &self.assembly_type {
            AssemblyType::Field(x) => x.value_len(),
            AssemblyType::Epsilon => FieldSize::new_unsized(),
            AssemblyType::Next(size) | AssemblyType::Start(size) => size.get(),
        }
    }
    pub fn value_bits(&self) -> Option<NonZeroTypeU> {
        match &self.assembly_type {
            AssemblyType::Field(x) => match x.attach.borrow().as_ref() {
                Some(meaning) => meaning.size(),
                _ => None,
            },
            AssemblyType::Epsilon => None,
            AssemblyType::Next(size) | AssemblyType::Start(size) => {
                size.get().final_value()
            }
        }
    }
    pub fn max_len(&self) -> Option<NonZeroTypeU> {
        match &self.assembly_type {
            AssemblyType::Field(x) => match x.attach.borrow().as_ref() {
                Some(meaning) => meaning.size(),
                _ => NonZeroTypeU::new(x.bit_range.end - x.bit_range.start),
            },
            AssemblyType::Epsilon => unreachable!(),
            AssemblyType::Next(size) | AssemblyType::Start(size) => {
                size.get().final_value()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum AssemblyType {
    Field(Field),
    Epsilon, //empty pattern
    //TODO: NonZeroU type and not FieldSize
    Start(Rc<Cell<FieldSize>>), // Offset of the address of the current instruction.
    Next(Rc<Cell<FieldSize>>), // Offset of the address of the next instruction.
}

#[derive(Clone, Debug)]
pub struct Token {
    pub name: Rc<str>,
    /// The min size of the instruction this token require
    pub size: NonZeroTypeU,
    /// Endian of the fields
    pub endian: Endian,
    /////all the fields
    //pub fields: Vec<Rc<Field>>,
}

#[derive(Clone)]
pub struct Field {
    pub bit_range: Range<IntTypeU>,
    pub signed: bool,
    pub fmt: PrintFmt,
    pub token: Rc<Token>,
    pub attach: RefCell<Option<Rc<Meaning>>>,
}

impl Field {
    fn value_len(&self) -> FieldSize {
        match self.attach.borrow().as_ref() {
            Some(meaning) if meaning.size().is_some() => {
                //if associated with reg, it have the reg size
                FieldSize::new_bits(meaning.size().unwrap())
            }
            _ => {
                //otherwise, it can have any size, with the min of bits size
                let size = FieldSize::new_unsized();
                size.set_min(
                    (self.bit_range.end - self.bit_range.start)
                        .try_into()
                        .unwrap(),
                );
                size
            }
        }
    }
}

impl std::fmt::Debug for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AssField bit_range: {:?}, signed {}, fmt: {:?}, token {:?}",
            self.bit_range, self.signed, self.fmt, self.token,
        )
    }
}
