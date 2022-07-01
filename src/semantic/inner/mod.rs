pub mod attach;
pub mod disassembly;
pub mod display;
pub mod execution;
pub mod pattern;
pub mod pcode_macro;
pub mod space;
pub mod table;
pub mod token;
pub mod user_function;
pub mod varnode;
pub mod with_block;

use std::cell::Cell;
use std::collections::HashMap;
use std::ops::{Bound, RangeBounds};
use std::rc::Rc;

use crate::base::{IntTypeU, NonZeroTypeU};
use crate::preprocessor::PreProcOutput;
use crate::semantic::assembly::Assembly;
use crate::InputSource;
use crate::{
    syntax, IDENT_EPSILON, IDENT_INSTRUCTION, IDENT_INST_NEXT, IDENT_INST_START,
};

pub use self::pattern::{Block, Pattern};
pub use self::pcode_macro::PcodeMacro;
pub use self::table::{Constructor, Table};
use self::user_function::UserFunction;
pub use self::with_block::WithBlock;

pub use super::space::Space;
pub use super::varnode::Varnode;
pub use super::{assembly, Endian, Meaning, SemanticError};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FieldRange {
    min: NonZeroTypeU,
    max: NonZeroTypeU,
}
impl FieldRange {
    fn new(min: NonZeroTypeU, max: Option<NonZeroTypeU>) -> Self {
        Self {
            min,
            max: max.unwrap_or(NonZeroTypeU::new(IntTypeU::MAX).unwrap()),
        }
    }
    fn new_unsize() -> Self {
        Self::new(1.try_into().unwrap(), None)
    }
    fn set_min(self, new_min: NonZeroTypeU) -> Option<Self> {
        if self.max < new_min {
            //max is less then the new_min, unable to set this value
            return None;
        }
        let min = self.min.max(new_min);
        Some(Self { min, max: self.max })
    }
    fn set_max(self, new_max: NonZeroTypeU) -> Option<Self> {
        if self.min > new_max {
            //unable to set this max value
            return None;
        }
        let max = self.max.min(new_max);
        Some(Self { min: self.min, max })
    }
    fn value(&self) -> Option<NonZeroTypeU> {
        (self.min == self.max).then_some(self.min)
    }
}
impl RangeBounds<NonZeroTypeU> for FieldRange {
    fn start_bound(&self) -> Bound<&NonZeroTypeU> {
        Bound::Included(&self.min)
    }

    fn end_bound(&self) -> Bound<&NonZeroTypeU> {
        Bound::Included(&self.max)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FieldAuto {
    None,
    Min,
    Value(NonZeroTypeU),
}
impl FieldAuto {
    fn in_range(self, range: &FieldRange) -> Self {
        match &self {
            Self::None | Self::Min => self,
            Self::Value(value) if range.contains(value) => self,
            Self::Value(_) => Self::None,
        }
    }
}

pub const FIELD_SIZE_BOOL: FieldSize = FieldSize::Unsized {
    range: FieldRange {
        min: unsafe { NonZeroTypeU::new_unchecked(1) },
        max: unsafe { NonZeroTypeU::new_unchecked(IntTypeU::MAX) },
    },
    possible: FieldAuto::Min,
};
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FieldSize {
    Unsized {
        range: FieldRange,
        possible: FieldAuto,
    },
    Value(NonZeroTypeU),
}
impl FieldSize {
    pub fn new_bits(bits: NonZeroTypeU) -> Self {
        Self::Value(bits)
    }
    pub fn new_bytes(bytes: NonZeroTypeU) -> Self {
        Self::Value(
            NonZeroTypeU::new(bytes.get().checked_mul(8).unwrap()).unwrap(),
        )
    }
    pub fn new_unsized() -> Self {
        Self::Unsized {
            range: FieldRange::new_unsize(),
            possible: FieldAuto::None,
        }
    }
    pub fn is_undefined(&self) -> bool {
        matches!(
            self,
            Self::Unsized {
                possible: FieldAuto::None,
                ..
            }
        )
    }
    pub fn final_value(&self) -> Option<NonZeroTypeU> {
        match self {
            Self::Value(value) => Some(*value),
            Self::Unsized { .. } => None,
        }
    }
    pub fn is_final(&self) -> bool {
        self.final_value().is_some()
    }
    pub fn update_action<F>(&mut self, mut action: F) -> Option<bool>
    where
        F: FnMut(Self) -> Option<Self>,
    {
        let new_size = action(*self)?;
        let changed = *self != new_size;
        *self = new_size;
        Some(changed)
    }
    pub fn set_final_value(self, final_value: NonZeroTypeU) -> Option<Self> {
        match self {
            //if already value, check if value is eq
            Self::Value(value) => (value == final_value).then_some(self),
            //if not in range, invalid
            Self::Unsized { range, .. } if !range.contains(&final_value) => {
                None
            }
            //if in range, valid
            Self::Unsized { .. } => Some(Self::Value(final_value)),
        }
    }
    pub fn min(&self) -> NonZeroTypeU {
        match self {
            Self::Value(value) => *value,
            Self::Unsized { range, .. } => range.min,
        }
    }
    pub fn set_min(self, min: NonZeroTypeU) -> Option<Self> {
        match self {
            Self::Value(value) => (min <= value).then_some(self),
            Self::Unsized { range, possible } => {
                let range = range.set_min(min)?;
                if let Some(value) = range.value() {
                    return Some(Self::Value(value));
                }
                let possible = possible.in_range(&range);
                Some(Self::Unsized { range, possible })
            }
        }
    }
    pub fn max(&self) -> NonZeroTypeU {
        match self {
            Self::Value(value) => *value,
            Self::Unsized { range, .. } => range.max,
        }
    }
    pub fn set_max(self, max: NonZeroTypeU) -> Option<Self> {
        match self {
            Self::Value(value) => (max >= value).then_some(self),
            Self::Unsized { range, possible } => {
                let range = range.set_max(max)?;
                if let Some(value) = range.value() {
                    return Some(Self::Value(value));
                }
                let possible = possible.in_range(&range);
                Some(Self::Unsized { range, possible })
            }
        }
    }
    pub fn possible_value(&self) -> Option<NonZeroTypeU> {
        match self {
            Self::Value(value) => Some(*value),
            Self::Unsized {
                possible: FieldAuto::Value(value),
                ..
            } => Some(*value),
            Self::Unsized {
                possible: FieldAuto::Min,
                ..
            } => Some(self.min()),
            Self::Unsized {
                possible: FieldAuto::None,
                ..
            } => None,
        }
    }
    pub fn possible_min(&self) -> bool {
        matches!(
            self,
            Self::Unsized {
                possible: FieldAuto::Min,
                ..
            }
        )
    }
    pub fn set_possible_min(mut self) -> Self {
        match self {
            Self::Value(_) => self,
            Self::Unsized {
                ref mut possible, ..
            } => {
                *possible = FieldAuto::Min;
                self
            }
        }
    }
    pub fn set_possible_value(
        mut self,
        pos_value: NonZeroTypeU,
    ) -> Option<Self> {
        match self {
            Self::Unsized { range, .. } if !range.contains(&pos_value) => None,
            Self::Unsized {
                ref mut possible, ..
            } => {
                *possible = FieldAuto::Value(pos_value);
                Some(self)
            }
            Self::Value(value) => (value == pos_value).then_some(self),
        }
    }
    pub fn intersection(self, other: Self) -> Option<Self> {
        Self::new_unsized()
            .set_min(self.min().max(other.min()))?
            .set_max(self.max().min(other.max()))
    }

    pub fn all_same_size(sizes: &mut [FieldSizeCell]) -> Option<bool> {
        //find the result size
        let new_size = sizes
            .iter()
            .try_fold(FieldSize::new_unsized(), |acc, size| {
                acc.intersection(size.get())
            })?;
        //update all the sizes
        let modified = sizes.iter_mut().fold(false, |acc, size| {
            let modified = match (new_size, size.get()) {
                (x @ FieldSize::Value(_), _) => size.set(x),
                (_, FieldSize::Value(_)) => unreachable!(),
                (
                    FieldSize::Unsized { range, .. },
                    FieldSize::Unsized { possible, .. },
                ) => {
                    let possible = possible.in_range(&range);
                    size.set(FieldSize::Unsized { range, possible })
                }
            };
            acc | modified
        });
        Some(modified)
    }
}
impl Default for FieldSize {
    fn default() -> Self {
        Self::new_unsized()
    }
}

pub trait FieldSizeMut {
    fn get(&self) -> FieldSize;
    fn set(&mut self, size: FieldSize) -> bool;
    fn update_action(
        &mut self,
        mut action: impl FnMut(FieldSize) -> Option<FieldSize>,
    ) -> Option<bool> {
        let new_size = action(self.get())?;
        Some(self.set(new_size))
    }
}

impl<'a> FieldSizeMut for &'a mut FieldSize {
    fn get(&self) -> FieldSize {
        **self
    }
    fn set(&mut self, size: FieldSize) -> bool {
        let old = std::mem::replace(*self, size);
        old != **self
    }
}
impl<'a> FieldSizeMut for &'a Cell<FieldSize> {
    fn get(&self) -> FieldSize {
        (*self).get()
    }
    fn set(&mut self, size: FieldSize) -> bool {
        let old = self.replace(size);
        old != self.get()
    }
}
impl FieldSizeMut for FieldSize {
    fn get(&self) -> FieldSize {
        *self
    }
    fn set(&mut self, size: FieldSize) -> bool {
        if *self != size {
            unreachable!("Try to modify FieldSizeMut owned");
        }
        false
    }
}

pub enum FieldSizeCell<'a> {
    Borrow(&'a mut FieldSize),
    Cell(&'a Cell<FieldSize>),
    ///Values that don't need to be updated, eg Varnodes
    Owned(FieldSize),
}
impl<'a> From<&'a mut FieldSize> for FieldSizeCell<'a> {
    fn from(value: &'a mut FieldSize) -> Self {
        Self::Borrow(value)
    }
}
impl<'a> From<&'a Cell<FieldSize>> for FieldSizeCell<'a> {
    fn from(value: &'a Cell<FieldSize>) -> Self {
        Self::Cell(value)
    }
}
impl<'a> From<FieldSize> for FieldSizeCell<'a> {
    fn from(value: FieldSize) -> Self {
        Self::Owned(value)
    }
}
impl<'a> FieldSizeMut for FieldSizeCell<'a> {
    fn get(&self) -> FieldSize {
        match self {
            Self::Borrow(x) => x.get(),
            Self::Cell(x) => x.get(),
            Self::Owned(x) => x.get(),
        }
    }
    fn set(&mut self, size: FieldSize) -> bool {
        match self {
            Self::Borrow(x) => x.set(size),
            Self::Cell(x) => x.set(size),
            Self::Owned(x) => x.set(size),
        }
    }
}

pub trait SolverStatus<T: SolverStatus = Self> {
    fn iam_not_finished(&mut self);
    fn iam_not_finished_location(&mut self, _location: &InputSource) {
        //ignore location by default
        self.iam_not_finished();
    }
    fn i_did_a_thing(&mut self);
    fn we_finished(&self) -> bool;
    fn we_did_a_thing(&self) -> bool;
    fn unfinished_locations(&self) -> &[InputSource] {
        &[]
    }
    fn combine(&mut self, other: &T) {
        if !other.we_finished() {
            self.iam_not_finished();
            for location in other.unfinished_locations() {
                self.iam_not_finished_location(location);
            }
        }
        if other.we_did_a_thing() {
            self.i_did_a_thing()
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Solved {
    did_a_thing: bool,
    finished: bool,
}

impl SolverStatus for Solved {
    fn iam_not_finished(&mut self) {
        self.finished = false;
    }
    fn i_did_a_thing(&mut self) {
        self.did_a_thing = true;
    }
    fn we_finished(&self) -> bool {
        self.finished
    }
    fn we_did_a_thing(&self) -> bool {
        self.did_a_thing
    }
    fn combine(&mut self, other: &Self) {
        self.did_a_thing |= other.we_did_a_thing();
        self.finished &= other.we_finished();
    }
}

impl Default for Solved {
    fn default() -> Self {
        Self {
            did_a_thing: false,
            finished: true,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct SolvedLocation {
    solved: Solved,
    locations: Vec<InputSource>,
}

impl SolverStatus for SolvedLocation {
    fn iam_not_finished_location(&mut self, location: &InputSource) {
        self.solved.finished = false;
        self.locations.push(location.clone());
    }
    fn iam_not_finished(&mut self) {
        //TODO: delete this, detect unecessary location ignoring
        panic!();
        //self.solved.finished = false;
    }
    fn i_did_a_thing(&mut self) {
        self.solved.did_a_thing = true;
    }
    fn we_finished(&self) -> bool {
        self.solved.finished
    }
    fn we_did_a_thing(&self) -> bool {
        self.solved.did_a_thing
    }
    fn unfinished_locations(&self) -> &[InputSource] {
        &self.locations
    }
    fn combine(&mut self, other: &Self) {
        self.solved.combine(&other.solved);
        self.locations
            .extend(other.unfinished_locations().iter().cloned());
    }
}

//TODO convert this to From trait
pub trait ConvertScope<T> {
    fn convert(&self) -> T;
}

/// All identifiers, this is used to enforce a unique name for each.
#[derive(Clone, Debug)]
pub enum GlobalScope {
    Space(Rc<Space>),
    Token(Rc<assembly::Token>),
    Assembly(Rc<assembly::Assembly>),
    UserFunction(Rc<UserFunction>),
    Varnode(Rc<Varnode>),
    PcodeMacro(Rc<PcodeMacro>),
    Table(Rc<Table>),
}

impl GlobalScope {
    //TODO Why? replace by option
    pub fn space_or<T>(&self, err: T) -> Result<Rc<Space>, T> {
        match self {
            GlobalScope::Space(x) => Ok(Rc::clone(x)),
            _ => Err(err),
        }
    }
    pub fn varnode_or<T>(&self, err: T) -> Result<Rc<Varnode>, T> {
        match self {
            GlobalScope::Varnode(x) => Ok(Rc::clone(x)),
            _ => Err(err),
        }
    }
    pub fn token_field_or<T>(
        &self,
        err: T,
    ) -> Result<Rc<assembly::Assembly>, T> {
        match self {
            GlobalScope::Assembly(x) => Ok(Rc::clone(x)),
            _ => Err(err),
        }
    }
    pub fn table_or<T>(&self, err: T) -> Result<Rc<Table>, T> {
        match self {
            GlobalScope::Table(x) => Ok(Rc::clone(x)),
            _ => Err(err),
        }
    }
    pub fn unwrap_table(&self) -> Option<Rc<Table>> {
        match self {
            GlobalScope::Table(x) => Some(Rc::clone(x)),
            _ => None,
        }
    }
    pub fn unwrap_space(&self) -> Option<Rc<Space>> {
        match self {
            GlobalScope::Space(x) => Some(Rc::clone(x)),
            _ => None,
        }
    }
    pub fn unwrap_varnode(&self) -> Option<Rc<Varnode>> {
        match self {
            GlobalScope::Varnode(x) => Some(Rc::clone(x)),
            _ => None,
        }
    }
    pub fn unwrap_assembly(&self) -> Option<Rc<assembly::Assembly>> {
        match self {
            GlobalScope::Assembly(x) => Some(Rc::clone(x)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Sleigh<'a> {
    root: &'a PreProcOutput,
    /// the default address space
    pub(crate) default_space: Option<Rc<Space>>,

    //data that will be passed to the final struct
    /// processor endian
    pub(crate) endian: Option<Endian>,
    /// memory access alignemnt
    pub(crate) alignment: Option<IntTypeU>,
    /// all the unique ident types, such Tables, Macros, Varnodes, etc.
    pub(crate) idents: HashMap<Rc<str>, GlobalScope>,

    //TODO: HACK: this this is not adequated, it requires that the addr size
    //being deduced some how
    exec_addr_size: Rc<Cell<FieldSize>>,
}

impl<'a> Sleigh<'a> {
    pub fn exec_addr_size(&self) -> Rc<Cell<FieldSize>> {
        Rc::clone(&self.exec_addr_size)
    }
    //pub fn exec_addr_size(&self) -> FieldSize {
    //    self.exec_addr_size.get()
    //}
    //pub fn update_exec_addr_size<F>(&self, mut action: F) -> Option<bool>
    //where
    //    F: FnMut(FieldSize) -> Option<FieldSize>,
    //{
    //    //self.exec_addr_size.update(action);
    //    let new_size = action(self.exec_addr_size.get())?;
    //    if new_size != self.exec_addr_size.get() {
    //        self.exec_addr_size.set(new_size);
    //        Some(true)
    //    } else {
    //        Some(false)
    //    }
    //}
    pub fn input_src(&self, src: &'a str) -> InputSource {
        self.root.source_data_start(src).unwrap().clone()
    }
    pub fn default_space(&self) -> Option<Rc<Space>> {
        self.default_space.as_ref().map(|x| Rc::clone(x))
    }
    pub fn get_global<'b>(&'b self, name: &'a str) -> Option<&'b GlobalScope> {
        self.idents.get(name)
    }
    pub fn set_endian(
        &mut self,
        endian: syntax::define::Endian,
    ) -> Result<(), SemanticError> {
        self.endian
            .replace(endian)
            .map(|_| Err(SemanticError::EndianMult))
            .unwrap_or(Ok(()))
    }
    pub fn set_alignment(
        &mut self,
        align: syntax::define::Alignment,
    ) -> Result<(), SemanticError> {
        self.alignment
            .replace(align.0)
            .map(|_| Err(SemanticError::AlignmentMult))
            .unwrap_or(Ok(()))
    }
    pub fn create_user_function(
        &mut self,
        func: syntax::define::UserFunction<'a>,
    ) -> Result<(), SemanticError> {
        let src = self.input_src(func.0);
        let user = UserFunction::new(&func.0, src);
        self.idents
            .insert(Rc::clone(user.name()), GlobalScope::UserFunction(user))
            .map(|_| Err(SemanticError::NameDuplicated))
            .unwrap_or(Ok(()))
    }
    fn process(
        &mut self,
        with_block: Option<WithBlock>,
        syntax: impl Iterator<Item = syntax::Assertation<'a>>,
    ) -> Result<(), SemanticError> {
        for assertation in syntax {
            use syntax::define::Define::*;
            use syntax::Assertation::*;
            match assertation {
                Define(Endian(endian)) => self.set_endian(endian)?,
                Define(Alignment(x)) => self.set_alignment(x)?,
                Define(Space(x)) => self.create_space(x)?,
                Define(Varnode(x)) => self.create_memory(x)?,
                Define(Bitrange(x)) => self.create_bitrange(x)?,
                Define(UserFunction(x)) => self.create_user_function(x)?,
                Define(Context(x)) => self.create_context(x)?,
                Define(Token(x)) => self.create_token(x)?,
                Attach(x) => self.create_attach(x)?,
                TableConstructor(x) => {
                    self.insert_table_constructor(&with_block, x)?
                }
                PcodeMacro(x) => self.create_pcode_macro(x)?,
                WithBlock(block) => {
                    //TODO remove this clone
                    let body = block.body.clone();
                    let with_block = crate::semantic::inner::WithBlock::new(
                        self,
                        &with_block,
                        block,
                    )?;
                    self.process(Some(with_block), body.into_iter())?;
                }
            }
        }
        Ok(())
    }
    pub fn new(
        syntax: syntax::Syntax<'a>,
        root: &'a PreProcOutput,
    ) -> Result<Self, SemanticError> {
        //TODO insert default global vars: such as `instruction`, `unique`, etc
        let mut sleigh = Sleigh {
            root,
            default_space: None,
            endian: None,
            alignment: None,
            idents: HashMap::default(),
            exec_addr_size: Rc::new(Cell::new(FieldSize::new_unsized())),
        };
        //TODO better default creation
        //insert defaults
        let def = Assembly::new_start(
            IDENT_INST_START,
            Rc::clone(&sleigh.exec_addr_size),
        );
        sleigh
            .idents
            .insert(Rc::clone(def.name()), GlobalScope::Assembly(def));
        let def = Assembly::new_next(
            IDENT_INST_NEXT,
            Rc::clone(&sleigh.exec_addr_size),
        );
        sleigh
            .idents
            .insert(Rc::clone(def.name()), GlobalScope::Assembly(def));
        let def = Assembly::new_epsilon(IDENT_EPSILON);
        sleigh
            .idents
            .insert(Rc::clone(def.name()), GlobalScope::Assembly(def));
        let name = Rc::from(IDENT_INSTRUCTION);
        let def = Table::new_empty(name);
        sleigh
            .idents
            .insert(Rc::clone(def.name()), GlobalScope::Table(def));

        sleigh.process(None, syntax.into_iter())?;
        Ok(sleigh)
    }
}
