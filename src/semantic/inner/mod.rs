pub mod attach;
pub mod disassembly;
pub mod display;
pub mod execution;
pub mod pattern;
pub mod pcode_macro;
pub mod space;
pub mod table;
pub mod token;
pub mod varnode;
pub mod with_block;

use indexmap::IndexMap;
use std::rc::Rc;

use crate::syntax::define::TokenFieldAttribute;
use crate::Span;
use crate::{
    syntax, IDENT_EPSILON, IDENT_INSTRUCTION, IDENT_INST_NEXT, IDENT_INST_START,
};
use crate::{Endian, NumberUnsigned};

pub use self::execution::{FieldAuto, FieldRange, FieldSize, FieldSizeMut};
pub use self::pattern::{Block, Pattern};
pub use self::pcode_macro::PcodeMacro;
pub use self::table::{Constructor, Table};
use self::token::TokenField;
use self::varnode::Context;
use self::with_block::WithBlockCurrent;

use super::space::Space;
use super::token::Token;
use super::user_function::UserFunction;
use super::varnode::{Bitrange, Varnode};
pub use super::SemanticError;
use super::{
    Epsilon, GlobalAnonReference, GlobalElement, GlobalReference, InstNext,
    InstStart, PrintBase, PrintFmt,
};

#[derive(Copy, Clone, Debug)]
pub struct PrintFlags {
    ///flag if signed was set
    pub signed_set: bool,
    ///flag if hex or dec was set
    pub base: Option<PrintBase>,
}

impl PrintFlags {
    pub fn from_token_att<'a>(
        src: &Span,
        att: impl Iterator<Item = &'a TokenFieldAttribute>,
    ) -> Result<Self, SemanticError> {
        let (mut signed_set, mut base) = (false, None);
        for att in att {
            use syntax::define::TokenFieldAttribute::*;
            match att {
                Hex if base.is_none() => base = Some(PrintBase::Hex),
                Dec if base.is_none() => base = Some(PrintBase::Dec),
                Hex | Dec => {
                    return Err(SemanticError::TokenFieldAttachDup(src.clone()))
                }
                Signed if !signed_set => signed_set = true,
                Signed => {
                    return Err(SemanticError::TokenFieldAttDup(src.clone()))
                }
            }
        }
        Ok(Self { signed_set, base })
    }
    pub fn is_set(&self) -> bool {
        self.signed_set || self.base.is_some()
    }
}

impl From<PrintFlags> for PrintFmt {
    fn from(flags: PrintFlags) -> Self {
        //if signed is set, this is signed, otherwise is unsigned
        let signed = flags.signed_set;
        //use the set base, if unset, use the default: hex
        let base = flags.base.unwrap_or(PrintBase::Hex);
        PrintFmt { signed, base }
    }
}

pub trait SolverStatus<T: SolverStatus = Self> {
    fn iam_not_finished_location(
        &mut self,
        location: &Span,
        file: &'static str,
        line: u32,
    );
    fn i_did_a_thing(&mut self);
    fn we_finished(&self) -> bool;
    fn we_did_a_thing(&self) -> bool;
    fn unfinished_locations(&self) -> &[(Span, &'static str, u32)];
    fn combine(&mut self, other: &Self);
}

#[derive(Clone, Copy, Debug)]
pub struct Solved {
    did_a_thing: bool,
    finished: bool,
}

impl SolverStatus for Solved {
    fn iam_not_finished_location(
        &mut self,
        _location: &Span,
        _file: &'static str,
        _line: u32,
    ) {
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
    fn unfinished_locations(&self) -> &[(Span, &'static str, u32)] {
        &[]
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
    locations: Vec<(Span, &'static str, u32)>,
}

impl SolverStatus for SolvedLocation {
    fn iam_not_finished_location(
        &mut self,
        location: &Span,
        file: &'static str,
        line: u32,
    ) {
        self.solved.iam_not_finished_location(location, file, line);
        self.locations.push((location.clone(), file, line));
    }
    fn i_did_a_thing(&mut self) {
        self.solved.i_did_a_thing();
    }
    fn we_finished(&self) -> bool {
        self.solved.we_finished()
    }
    fn we_did_a_thing(&self) -> bool {
        self.solved.we_did_a_thing()
    }
    fn unfinished_locations(&self) -> &[(Span, &'static str, u32)] {
        &self.locations
    }
    fn combine(&mut self, other: &Self) {
        self.solved.combine(&other.solved);
        self.locations
            .extend(other.unfinished_locations().iter().cloned());
    }
}

pub trait GlobalConvert {
    type FinalType;
    fn convert(&self) -> Rc<Self::FinalType>;
}

impl<T: GlobalConvert> GlobalAnonReference<T> {
    pub fn convert_reference(&self) -> GlobalAnonReference<T::FinalType> {
        let ele = self.element();
        let value = ele.convert();
        GlobalAnonReference {
            name: Rc::clone(&self.name),
            value: Rc::downgrade(&value),
        }
    }
}
impl<T: GlobalConvert> GlobalReference<T> {
    pub fn convert_reference(&self) -> GlobalReference<T::FinalType> {
        let ele = self.element();
        let value = ele.convert();
        GlobalReference {
            src: self.src.clone(),
            name: Rc::clone(&self.name),
            value: Rc::downgrade(&value),
        }
    }
}
impl<T: GlobalConvert> GlobalElement<T> {
    pub fn element_convert(&self) -> GlobalElement<T::FinalType> {
        GlobalElement::new(
            Rc::clone(self.name_raw()),
            Rc::clone(&self.element().convert()),
        )
    }
}

/// All identifiers, this is used to enforce a unique name for each.
#[derive(Clone, Debug)]
pub enum GlobalScope {
    Space(GlobalElement<Space>),
    Varnode(GlobalElement<Varnode>),
    Context(GlobalElement<Context>),
    Bitrange(GlobalElement<Bitrange>),
    Token(GlobalElement<Token>),
    TokenField(GlobalElement<TokenField>),
    InstStart(GlobalElement<InstStart>),
    InstNext(GlobalElement<InstNext>),
    Epsilon(GlobalElement<Epsilon>),
    UserFunction(GlobalElement<UserFunction>),
    PcodeMacro(GlobalElement<PcodeMacro>),
    Table(GlobalElement<Table>),
}

impl GlobalScope {
    pub fn name_raw(&self) -> &Rc<str> {
        match self {
            GlobalScope::Space(x) => x.name_raw(),
            GlobalScope::Token(x) => x.name_raw(),
            GlobalScope::TokenField(x) => x.name_raw(),
            GlobalScope::UserFunction(x) => x.name_raw(),
            GlobalScope::Varnode(x) => x.name_raw(),
            GlobalScope::PcodeMacro(x) => x.name_raw(),
            GlobalScope::Table(x) => x.name_raw(),
            GlobalScope::Context(x) => x.name_raw(),
            GlobalScope::Bitrange(x) => x.name_raw(),
            GlobalScope::InstStart(x) => x.name_raw(),
            GlobalScope::InstNext(x) => x.name_raw(),
            GlobalScope::Epsilon(x) => x.name_raw(),
        }
    }
    pub fn name(&self) -> &str {
        Rc::as_ref(self.name_raw())
    }
}

macro_rules! global_scope_something_or {
    ($type:ident, $var:ident, $name:ident) => {
        pub fn $name<T>(&self, err: T) -> Result<&GlobalElement<$type>, T> {
            match self {
                GlobalScope::$var(x) => Ok(x),
                _ => Err(err),
            }
        }
    };
}
macro_rules! global_scope_something_unwrap {
    ($type:ident, $var:ident, $name:ident) => {
        pub fn $name(&self) -> Option<&GlobalElement<$type>> {
            match self {
                GlobalScope::$var(x) => Some(x),
                _ => None,
            }
        }
    };
}
impl GlobalScope {
    //TODO implement *_or_else
    global_scope_something_or!(Space, Space, space_or);
    global_scope_something_or!(Varnode, Varnode, varnode_or);
    global_scope_something_or!(Context, Context, context_or);
    global_scope_something_or!(TokenField, TokenField, token_field_or);
    global_scope_something_or!(Table, Table, table_or);
    global_scope_something_unwrap!(Space, Space, unwrap_space);
    global_scope_something_unwrap!(Varnode, Varnode, unwrap_varnode);
    global_scope_something_unwrap!(Context, Context, unwrap_context);
    global_scope_something_unwrap!(TokenField, TokenField, unwrap_token_field);
    global_scope_something_unwrap!(Table, Table, unwrap_table);
}

#[derive(Clone, Debug)]
pub struct Sleigh {
    /// the default address space
    pub(crate) default_space: Option<GlobalElement<Space>>,

    //data that will be passed to the final struct
    /// processor endian
    pub(crate) endian: Option<Endian>,
    /// memory access alignemnt
    pub(crate) alignment: Option<NumberUnsigned>,
    /// all the unique ident types, such Tables, Macros, Varnodes, etc.
    pub(crate) idents: IndexMap<Rc<str>, GlobalScope>,

    //TODO: HACK: this this is not adequated, it requires that the addr size
    //being deduced some how
    pub exec_addr_size: Option<FieldSize>,
}

impl Sleigh {
    pub fn insert_global(
        &mut self,
        item: GlobalScope,
    ) -> Result<(), SemanticError> {
        self.idents
            .insert(Rc::clone(&item.name_raw()), item)
            .map(|_| Err(SemanticError::NameDuplicated))
            .unwrap_or(Ok(()))
    }
    pub fn exec_addr_size(&self) -> Option<&FieldSize> {
        self.exec_addr_size.as_ref()
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
    pub fn default_space(&self) -> Option<&GlobalElement<Space>> {
        self.default_space.as_ref()
    }
    pub fn get_global<'b>(&'b self, name: &str) -> Option<&'b GlobalScope> {
        self.idents.get(name)
    }
    pub fn set_endian(&mut self, endian: Endian) -> Result<(), SemanticError> {
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
    fn process(
        &mut self,
        with_block_current: &mut WithBlockCurrent,
        syntax: syntax::Sleigh,
    ) -> Result<(), SemanticError> {
        for assertation in syntax.assertations.into_iter() {
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
                Attach(x) => self.attach_meaning(x)?,
                TableConstructor(x) => {
                    self.insert_table_constructor(with_block_current, x)?
                }
                PcodeMacro(x) => self.create_pcode_macro(x)?,
                WithBlock(with_block) => {
                    //TODO remove this clone
                    let body = with_block_current.push(with_block);
                    self.process(with_block_current, body)?;
                    with_block_current.pop();
                }
            }
        }
        Ok(())
    }
    pub fn new(syntax: syntax::Sleigh) -> Result<Self, SemanticError> {
        //TODO insert default global vars: such as `instruction`, `unique`, etc
        let mut sleigh = Sleigh {
            default_space: None,
            endian: None,
            alignment: None,
            idents: IndexMap::default(),
            exec_addr_size: None,
        };
        //TODO better default creation
        //insert defaults
        let def = GlobalElement::new(
            Rc::from(IDENT_INST_START),
            Rc::new(InstStart(())),
        );
        sleigh
            .idents
            .insert(Rc::clone(def.name_raw()), GlobalScope::InstStart(def));
        let def = GlobalElement::new(
            Rc::from(IDENT_INST_NEXT),
            Rc::new(InstNext(())),
        );
        sleigh
            .idents
            .insert(Rc::clone(def.name_raw()), GlobalScope::InstNext(def));
        let def =
            GlobalElement::new(Rc::from(IDENT_EPSILON), Rc::new(Epsilon(())));
        sleigh
            .idents
            .insert(Rc::clone(def.name_raw()), GlobalScope::Epsilon(def));
        let def = GlobalElement::new(
            Rc::from(IDENT_INSTRUCTION),
            Table::new_empty(true),
        );
        sleigh
            .idents
            .insert(Rc::clone(def.name_raw()), GlobalScope::Table(def));

        sleigh.process(&mut WithBlockCurrent::default(), syntax)?;

        Ok(sleigh)
    }
}
