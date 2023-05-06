pub mod disassembly;
pub mod display;
pub mod execution;
pub mod meaning;
pub mod pattern;
pub mod pcode_macro;
pub mod space;
pub mod table;
pub mod token;
pub mod user_function;
pub mod varnode;

// internal representation used to convert from syntax to semantic
// represenatation
mod inner;

use indexmap::IndexMap;
use sleigh4rust::Endian;
use std::rc::Rc;
use std::rc::Weak;

use thiserror::Error;

use crate::semantic::inner::{SolvedLocation, SolverStatus};
use crate::syntax;
use crate::NumberNonZeroUnsigned;
use crate::SleighError;
use crate::Span;

use self::disassembly::DisassemblyError;
use self::inner::GlobalConvert;
use self::inner::Solved;
use self::pattern::PatternError;
use self::pcode_macro::PcodeMacro;
use self::pcode_macro::PcodeMacroError;
use self::space::Space;
use self::table::Table;
use self::table::TableError;
use self::token::*;
use self::user_function::UserFunction;
use self::varnode::Bitrange;
use self::varnode::Context;
use self::varnode::Varnode;

#[macro_export]
macro_rules! from_error {
    ($parent:ident, $child:ident, $name:ident $(,)?) => {
        impl From<$child> for $parent {
            fn from(input: $child) -> $parent {
                $parent::$name(input)
            }
        }
    };
}

#[derive(Clone, Debug, Error)]
pub enum WithBlockError {
    #[error("Table name Error")]
    TableName,
    #[error("Pattern Error")]
    Pattern(PatternError),
    #[error("Disassembly Error")]
    Disassembly(DisassemblyError),
}
from_error!(WithBlockError, PatternError, Pattern);
from_error!(WithBlockError, DisassemblyError, Disassembly);

#[derive(Error, Debug, Clone)]
pub enum SemanticError {
    #[error("Missing global endian definition")]
    EndianMissing,

    #[error("Multiple alignment definitions")]
    AlignmentMult,
    #[error("Multiple global endian definitions")]
    EndianMult,

    //TODO Src for duplication
    #[error("Name already taken")]
    NameDuplicated,

    #[error("Missing alignment definition")]
    AlignmentMissing,
    #[error("Missing default Space Address")]
    SpaceMissingDefault,
    #[error("Multiple default Space Address")]
    SpaceMultipleDefault,

    #[error("Space Address not found")]
    SpaceMissing,
    #[error("Invalid ref Space Address")]
    SpaceInvalid,
    #[error("Space duplicate attribute")]
    SpaceInvalidAtt,
    #[error("Space missing size")]
    SpaceMissingSize,
    #[error("Space invalid size")]
    SpaceInvalidSize,
    #[error("Space invalid wordsize")]
    SpaceInvalidWordSize,

    #[error("Invalid Varnode Size")]
    VarnodeInvalidVarnodeSize,
    #[error("Invalid Varnode Memory Size is too big")]
    VarnodeInvalidMemorySize,
    #[error("Invalid Varnode Memory Size is too big")]
    VarnodeInvalidMemoryEnd,
    #[error("Varnode not found")]
    VarnodeMissing,
    #[error("Varnode ref invalid")]
    VarnodeInvalid,

    #[error("Bitrange invalid bit size")]
    BitrangeInvalidSize,
    #[error("Bitrange ref varnode is too small")]
    BitrangeInvalidVarnodeSize,

    #[error("Context invalid Size")]
    ContextInvalidSize,
    #[error("Context duplicated attribute")]
    ContextInvalidAtt,

    #[error("Token invalid Size")]
    TokenInvalidSize,

    #[error("Token Field invalid Size")]
    TokenFieldInvalidSize,
    #[error("Token Field Duplicated Attribute {0}")]
    TokenFieldAttDup(Span),
    #[error("TokenField not found")]
    TokenFieldMissing,
    #[error("Token Field ref invalid")]
    TokenFieldInvalid,
    #[error("Token Field Attach duplicated {0}")]
    TokenFieldAttachDup(Span),
    #[error("Attach to a field with print flags set{0}")]
    AttachWithPrintFlags(Span),
    //#[error("Invalid Data {message}")]
    //InvalidData { message: String },
    //
    #[error("Table not found")]
    TableMissing,
    #[error("Invalid ref Table")]
    TableInvalid,

    #[error("Pattern Missing Ref")]
    PatternMissingRef,
    #[error("Pattern Invalid Ref")]
    PatternInvalidRef,

    #[error("Disassembly Ref not found")]
    DisassemblyMissingRef(Span),
    #[error("Disassembly Invalid Ref")]
    DisassemblyInvalidRef(Span),

    #[error("Display Ref not found")]
    DisplayMissingRef,
    #[error("Display Invalid Ref")]
    DisplayInvalidRef,

    #[error("Execution Label is defined multiple times")]
    ExecutionDuplicatedLabel,
    #[error("Execution Label not found")]
    ExecutionMissingLabel,

    //TODO: remove Satan
    #[error("TODO: Remove Satan")]
    Satanic,
    #[error("TODO: Remove Satan with ref")]
    SatanicRef(Span),

    //TODO: FOR REAL!!!
    #[error("Table Error: {0}")]
    Table(TableError),
    #[error("PcodeMacro Error")]
    PcodeMacro(PcodeMacroError),
    #[error("WithBlock Invalid name")]
    WithBlock(WithBlockError),
}
from_error!(SemanticError, TableError, Table);
from_error!(SemanticError, PcodeMacroError, PcodeMacro);
from_error!(SemanticError, WithBlockError, WithBlock);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum PrintBase {
    Dec,
    Hex,
}
impl PrintBase {
    pub fn is_hex(&self) -> bool {
        matches!(self, Self::Hex)
    }
    pub fn is_dec(&self) -> bool {
        matches!(self, Self::Dec)
    }
}
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct PrintFmt {
    pub(crate) signed: bool,
    pub(crate) base: PrintBase,
}
impl PrintFmt {
    pub fn signed(&self) -> bool {
        self.signed
    }
    pub fn base(&self) -> PrintBase {
        self.base
    }
}

#[derive(Clone, Copy, Debug)]
pub struct InstStart(());
#[derive(Clone, Copy, Debug)]
pub struct InstNext(());
#[derive(Clone, Copy, Debug)]
pub struct Epsilon(());

#[derive(Debug)]
pub struct GlobalAnonReference<T> {
    name: Rc<str>,
    value: Weak<T>,
}

impl<T> GlobalAnonReference<T> {
    pub fn from_element(element: &GlobalElement<T>) -> Self {
        Self {
            name: Rc::clone(element.name_raw()),
            value: Rc::downgrade(element.element_raw()),
        }
    }
    pub(crate) fn name_raw(&self) -> &Rc<str> {
        &self.name
    }
    pub fn element_ptr(&self) -> *const T {
        Weak::as_ptr(&self.value)
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn element(&self) -> GlobalElement<T> {
        GlobalElement::new(
            Rc::clone(self.name_raw()),
            self.value.upgrade().unwrap(),
        )
    }
}
impl<T> Eq for GlobalAnonReference<T> {}
impl<T> PartialEq for GlobalAnonReference<T> {
    fn eq(&self, other: &Self) -> bool {
        self.element_ptr() == other.element_ptr()
    }
}
impl<T> Clone for GlobalAnonReference<T> {
    fn clone(&self) -> Self {
        Self {
            name: Rc::clone(self.name_raw()),
            value: Weak::clone(&self.value),
        }
    }
}

#[derive(Debug)]
pub struct GlobalReference<T> {
    src: Span,
    name: Rc<str>,
    value: Weak<T>,
}

impl<T> GlobalReference<T> {
    pub fn from_element(element: &GlobalElement<T>, src: Span) -> Self {
        Self {
            src,
            name: Rc::clone(element.name_raw()),
            value: Rc::downgrade(element.element_raw()),
        }
    }
    pub(crate) fn name_raw(&self) -> &Rc<str> {
        &self.name
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn location(&self) -> &Span {
        &self.src
    }
    pub fn element(&self) -> GlobalElement<T> {
        GlobalElement::new(
            Rc::clone(self.name_raw()),
            self.value.upgrade().unwrap(),
        )
    }
    pub fn element_ptr(&self) -> *const T {
        Weak::as_ptr(&self.value)
    }
}
impl<T> Eq for GlobalReference<T> {}
impl<T> PartialEq for GlobalReference<T> {
    fn eq(&self, other: &Self) -> bool {
        self.element_ptr() == other.element_ptr()
    }
}
impl<T> From<GlobalReference<T>> for GlobalAnonReference<T> {
    fn from(input: GlobalReference<T>) -> Self {
        Self {
            name: input.name,
            value: input.value,
        }
    }
}
impl<T> Clone for GlobalReference<T> {
    fn clone(&self) -> Self {
        Self {
            src: self.src.clone(),
            name: Rc::clone(self.name_raw()),
            value: Weak::clone(&self.value),
        }
    }
}

#[derive(Debug)]
pub struct GlobalElement<T> {
    name: Rc<str>,
    value: Rc<T>,
}
impl<T> Clone for GlobalElement<T> {
    fn clone(&self) -> Self {
        Self {
            name: Rc::clone(&self.name),
            value: Rc::clone(&self.value),
        }
    }
}
impl<T> std::ops::Deref for GlobalElement<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<T> Eq for GlobalElement<T> {}
impl<T> PartialEq for GlobalElement<T> {
    fn eq(&self, other: &Self) -> bool {
        self.element_ptr() == other.element_ptr()
    }
}
impl<T> From<&GlobalReference<T>> for GlobalElement<T> {
    fn from(input: &GlobalReference<T>) -> Self {
        input.element()
    }
}
impl<T> From<&GlobalAnonReference<T>> for GlobalElement<T> {
    fn from(input: &GlobalAnonReference<T>) -> Self {
        input.element()
    }
}

impl<T> GlobalElement<T> {
    pub(crate) fn new(name: Rc<str>, value: Rc<T>) -> Self {
        Self { name, value }
    }
    pub(crate) fn new_from(name: &str, value: T) -> Self {
        Self {
            name: Rc::from(name),
            value: Rc::new(value),
        }
    }
    pub(crate) fn name_raw(&self) -> &Rc<str> {
        &self.name
    }
    pub(crate) fn element_raw(&self) -> &Rc<T> {
        &self.value
    }
    pub fn element_ptr(&self) -> *const T {
        Rc::as_ptr(&self.value)
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn element(&self) -> &T {
        &self.value
    }
    pub fn reference(&self) -> GlobalAnonReference<T> {
        GlobalAnonReference {
            name: Rc::clone(self.name_raw()),
            value: Rc::downgrade(self.element_raw()),
        }
    }
    pub(crate) fn reference_from(&self, location: Span) -> GlobalReference<T> {
        GlobalReference {
            src: location,
            name: Rc::clone(self.name_raw()),
            value: Rc::downgrade(self.element_raw()),
        }
    }
}

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
    pub fn token_field(&self) -> Option<&GlobalElement<TokenField>> {
        match self {
            GlobalScope::TokenField(x) => Some(x),
            _ => None,
        }
    }
    pub fn token(&self) -> Option<&GlobalElement<Token>> {
        match self {
            GlobalScope::Token(x) => Some(x),
            _ => None,
        }
    }
    pub fn space(&self) -> Option<&GlobalElement<Space>> {
        match self {
            GlobalScope::Space(x) => Some(x),
            _ => None,
        }
    }
    pub fn varnode(&self) -> Option<&GlobalElement<Varnode>> {
        match self {
            GlobalScope::Varnode(x) => Some(x),
            _ => None,
        }
    }
    pub fn context(&self) -> Option<&GlobalElement<Context>> {
        match self {
            GlobalScope::Context(x) => Some(x),
            _ => None,
        }
    }
    pub fn bitrange(&self) -> Option<&GlobalElement<Bitrange>> {
        match self {
            GlobalScope::Bitrange(x) => Some(x),
            _ => None,
        }
    }
    pub fn pcode_macro(&self) -> Option<&GlobalElement<PcodeMacro>> {
        match self {
            GlobalScope::PcodeMacro(x) => Some(x),
            _ => None,
        }
    }
    pub fn table(&self) -> Option<&GlobalElement<Table>> {
        match self {
            GlobalScope::Table(x) => Some(x),
            _ => None,
        }
    }
    pub fn user_function(&self) -> Option<&GlobalElement<UserFunction>> {
        match self {
            GlobalScope::UserFunction(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Sleigh {
    pub endian: Endian,
    pub alignment: u8,

    //pub default_space: Rc<space::Space>,
    //pub instruction_table: Rc<table::Table>,
    addr_len_bytes: NumberNonZeroUnsigned,

    //scope with all the global identifiers
    pub global_scope: IndexMap<Rc<str>, GlobalScope>,
}

impl Sleigh {
    pub fn endian(&self) -> Endian {
        self.endian
    }
    pub fn alignment(&self) -> u8 {
        self.alignment
    }
    pub fn addr_len_bytes(&self) -> NumberNonZeroUnsigned {
        self.addr_len_bytes
    }
    pub fn context_len(&self) -> u64 {
        //for now only allow the context to point to a single varnode
        let (varnode, mut low_bit, mut high_bit) = {
            let first_context =
                self.global_scope.values().find_map(GlobalScope::context);
            let Some(first_context) = first_context else {
                return 0;
            };
            let varnode = first_context.varnode.clone();
            (
                varnode,
                first_context.range.start(),
                first_context.range.end().get(),
            )
        };
        for context in
            self.global_scope.values().filter_map(GlobalScope::context)
        {
            if context.varnode != varnode {
                //TODO error
                panic!("Context pointing to multiple varnodes");
            }
            low_bit = low_bit.min(context.range.start());
            high_bit = high_bit.max(context.range.end().get());
        }
        low_bit = (low_bit + 7) / 8;
        high_bit = (high_bit + 7) / 8;
        high_bit - low_bit
    }
    pub(crate) fn new(value: syntax::Sleigh) -> Result<Self, SleighError> {
        let inner = inner::Sleigh::new(value)?;
        let context_len: usize = inner.context_len().try_into().unwrap();
        //HACK: verify that indirect recursion don't happen
        //NOTE we don't need to worry about direct (self) recursion.
        //AKA `Tablea` calling itself
        for table in inner
            .idents
            .values()
            .filter_map(|ident| ident.unwrap_table())
        {
            use std::ops::ControlFlow;
            if let ControlFlow::Break(rec) = table.pattern_indirect_recursion()
            {
                unimplemented!(
                    "Indirect recursion is not implemented at the moment {:?}",
                    rec
                );
            }
        }

        let mut tables = vec![];
        let mut pcode = vec![];
        let mut global_scope: IndexMap<Rc<str>, GlobalScope> = inner
            .idents
            .iter()
            .filter_map(|(k, v)| {
                let v = match v {
                    inner::GlobalScope::Space(x) => {
                        GlobalScope::Space(x.clone())
                    }
                    inner::GlobalScope::Token(x) => {
                        GlobalScope::Token(x.clone())
                    }
                    inner::GlobalScope::TokenField(x) => {
                        GlobalScope::TokenField(x.element_convert())
                    }
                    inner::GlobalScope::UserFunction(x) => {
                        GlobalScope::UserFunction(x.clone())
                    }
                    inner::GlobalScope::Varnode(x) => {
                        GlobalScope::Varnode(x.clone())
                    }
                    inner::GlobalScope::Context(x) => {
                        GlobalScope::Context(x.element_convert())
                    }
                    inner::GlobalScope::Bitrange(x) => {
                        GlobalScope::Bitrange(x.clone())
                    }
                    inner::GlobalScope::PcodeMacro(x) => {
                        pcode.push(x);
                        return None;
                    }
                    inner::GlobalScope::Table(x) => {
                        tables.push(x);
                        return None;
                    }
                    inner::GlobalScope::InstStart(x) => {
                        GlobalScope::InstStart(x.clone())
                    }
                    inner::GlobalScope::InstNext(x) => {
                        GlobalScope::InstNext(x.clone())
                    }
                    inner::GlobalScope::Epsilon(x) => {
                        GlobalScope::Epsilon(x.clone())
                    }
                };
                Some((Rc::clone(k), v))
            })
            .collect();

        //TODO: if unable to solve addr size or default space not set, error
        let exec_addr_size = inner.exec_addr_size().unwrap();
        let _ = inner.default_space.as_ref().unwrap();
        //solve all pcodes macros inside the tables
        //solve all tables
        for i in 0.. {
            let mut solved = Solved::default();
            for table in tables.iter() {
                table.solve(&mut solved)?;
            }
            if solved.we_finished() && !solved.we_did_a_thing() {
                break;
            }
            if i > 100 {
                //TODO return an error, but for now force the conversion,
                //so we can find where the error occour
                //break;
                panic!("Too many tries, unable to solve tables")
            }
            if !solved.we_did_a_thing() {
                //print the location that where unable to solve
                //TODO change solve functions to use <T: SolverStatus + ?Sized>
                let mut solved = SolvedLocation::default();
                for table in tables.iter() {
                    table.solve(&mut solved)?;
                }
                //TODO return an error, but for now force the conversion,
                //so we can find where the error occour
                //break;
                panic!("Unable to solve the table")
            }
        }

        global_scope.extend(tables.into_iter().map(|x| {
            x.convert(context_len);
            let x = x.element_convert();
            (Rc::clone(&x.name), GlobalScope::Table(x))
        }));
        //only convert used macros, AKA the ones that was solved/used by tables
        global_scope.extend(
            pcode
                .into_iter()
                .filter(|x| x.is_solved())
                .map(|x| x.element_convert())
                .map(|x| (Rc::clone(&x.name), GlobalScope::PcodeMacro(x))),
        );

        let endian = inner.endian.ok_or(SemanticError::EndianMissing)?;
        let alignment = inner.alignment.unwrap_or(0).try_into().unwrap();
        let inst_len_bytes = exec_addr_size.final_value().unwrap().get();
        assert!(inst_len_bytes % 8 == 0);
        assert!(inst_len_bytes / 8 != 0);
        let inst_len_bytes = (inst_len_bytes / 8).try_into().unwrap();
        //TODO check all constructor for tables have export of the same size
        Ok(Self {
            endian,
            alignment,
            global_scope,
            addr_len_bytes: inst_len_bytes,
        })
    }
    pub fn spaces<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<Space>> + 'a {
        self.global_scope.values().filter_map(|x| x.space())
    }
    pub fn varnodes<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<Varnode>> + 'a {
        self.global_scope.values().filter_map(|x| x.varnode())
    }
    pub fn contexts<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<Context>> + 'a {
        self.global_scope.values().filter_map(|x| x.context())
    }
    pub fn bitranges<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<Bitrange>> + 'a {
        self.global_scope.values().filter_map(|x| x.bitrange())
    }
    pub fn user_functions<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<UserFunction>> + 'a {
        self.global_scope.values().filter_map(|x| x.user_function())
    }
    pub fn pcode_macros<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<PcodeMacro>> + 'a {
        self.global_scope.values().filter_map(|x| x.pcode_macro())
    }
    pub fn tables<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<Table>> + 'a {
        self.global_scope.values().filter_map(|x| x.table())
    }
    pub fn tokens<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<Token>> + 'a {
        self.global_scope.values().filter_map(|x| x.token())
    }
    pub fn token_fields<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a GlobalElement<TokenField>> + 'a {
        self.global_scope.values().filter_map(|x| x.token_field())
    }
}
