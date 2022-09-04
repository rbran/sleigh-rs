pub mod assembly;
pub mod disassembly;
pub mod display;
pub mod execution;
pub mod pattern;
pub mod pcode_macro;
pub mod space;
pub mod table;
pub mod user_function;
pub mod varnode;

// internal representation used to convert from syntax to semantic
// represenatation
mod inner;

use std::collections::HashMap;
use std::fmt::Formatter;
use std::rc::Rc;

use thiserror::Error;

use crate::base::IntTypeS;
use crate::base::NonZeroTypeU;
use crate::preprocessor::PreProcOutput;
use crate::semantic::inner::{SolvedLocation, SolverStatus};
use crate::InputSource;
use crate::Table;
use crate::Token;
use crate::{PcodeMacro, Space, UserFunction, Varnode};

use self::disassembly::DisassemblyError;
use self::inner::Solved;
use self::pattern::PatternError;
use self::pcode_macro::PcodeMacroError;
use self::table::TableError;

use super::syntax;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Endian {
    Little,
    Big,
}

#[derive(Copy, Clone, Debug)]
pub enum PrintFmt {
    Hex,
    Dec,
}

#[derive(Clone, Debug)]
pub enum Meaning {
    Variable {
        size: NonZeroTypeU,
        vars: Vec<Option<Rc<varnode::Varnode>>>,
    },
    Name(Vec<Option<String>>),
    Value(Vec<Option<IntTypeS>>),
}

impl Meaning {
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable { .. })
    }
    pub fn size(&self) -> Option<NonZeroTypeU> {
        match self {
            Meaning::Variable { size, .. } => Some(*size),
            Meaning::Name(_) | Meaning::Value(_) => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum GlobalScope {
    Space(Rc<space::Space>),
    Varnode(Rc<varnode::Varnode>),
    Token(Rc<assembly::Token>),
    Assembly(Rc<assembly::Assembly>),
    UserFunction(Rc<user_function::UserFunction>),
    PcodeMacro(Rc<pcode_macro::PcodeMacro>),
    Table(Rc<table::Table>),
}

impl GlobalScope {
    pub fn unwrap_token_field(&self) -> Option<&Rc<assembly::Assembly>> {
        match self {
            GlobalScope::Assembly(x)
                if matches!(
                    &x.assembly_type,
                    assembly::AssemblyType::Field(_)
                ) =>
            {
                Some(x)
            }
            _ => None,
        }
    }
    pub fn unwrap_token(&self) -> Option<&Rc<assembly::Token>> {
        match self {
            GlobalScope::Token(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_space(&self) -> Option<&Rc<space::Space>> {
        match self {
            GlobalScope::Space(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_varnode(&self) -> Option<&Rc<varnode::Varnode>> {
        match self {
            GlobalScope::Varnode(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_pcode_macro(&self) -> Option<&Rc<pcode_macro::PcodeMacro>> {
        match self {
            GlobalScope::PcodeMacro(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_table(&self) -> Option<&Rc<table::Table>> {
        match self {
            GlobalScope::Table(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_user_function(
        &self,
    ) -> Option<&Rc<user_function::UserFunction>> {
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

    //scope with all the global identifiers
    pub global_scope: HashMap<Rc<str>, GlobalScope>,
    //GlobalScope
    //spaces: Vec<Rc<space::Space>>,
    //varnodes: Vec<Rc<varnode::Varnode>>,
    //tokens: Vec<Rc<assembly::Token>>,
    //assembles: Vec<Rc<assembly::Assembly>>,
    //user_functions: Vec<Rc<user_function::UserFunction>>,
    //pcode_macros: Vec<Rc<pcode_macro::PcodeMacro>>,
    //tables: Vec<Rc<table::Table>>,
}

//TODO impl Drop for Sleigh
//impl Drop for Sleigh {
//    fn drop(&mut self) {
//        todo!()
//    }
//}

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

pub trait DisplayBacktrace {
    fn display_backtrace(
        &self,
        f: &mut Formatter<'_>,
        output: PreProcOutput,
    ) -> Result<(), std::fmt::Error>;
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
    #[error("Token Field Duplicated Attribute")]
    TokenFieldAttDup,
    #[error("TokenField not found")]
    TokenFieldMissing,
    #[error("Token Field ref invalid")]
    TokenFieldInvalid,
    #[error("Token Field Attach duplicated")]
    TokenFieldAttachDup,
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
    DisassemblyMissingRef(InputSource),
    #[error("Disassembly Invalid Ref")]
    DisassemblyInvalidRef(InputSource),

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
    SatanicRef(InputSource),

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

impl Sleigh {
    pub fn new<'a>(
        value: syntax::Syntax<'a>,
        root: &'a PreProcOutput,
    ) -> Result<Self, SemanticError> {
        let mut inner = inner::Sleigh::new(value, root)?;
        let mut tables = vec![];
        let mut pcode = vec![];
        let mut global_scope: HashMap<Rc<str>, GlobalScope> = inner
            .idents
            .drain()
            .filter_map(|(k, v)| {
                let v = match v {
                    inner::GlobalScope::Space(x) => GlobalScope::Space(x),
                    inner::GlobalScope::Token(x) => GlobalScope::Token(x),
                    inner::GlobalScope::Assembly(x) => GlobalScope::Assembly(x),
                    inner::GlobalScope::UserFunction(x) => {
                        GlobalScope::UserFunction(x.convert())
                    }
                    inner::GlobalScope::Varnode(x) => GlobalScope::Varnode(x),
                    inner::GlobalScope::PcodeMacro(x) => {
                        pcode.push(Rc::clone(&x));
                        return None;
                    }
                    inner::GlobalScope::Table(x) => {
                        //tables are empty initially
                        tables.push(Rc::clone(&x));
                        return None;
                    }
                };
                Some((k, v))
            })
            .collect();

        //solve all pcodes macros inside the tables
        //solve all tables
        for i in 0.. {
            println!("Solve Tables");
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
                for (location, file, line) in solved.unfinished_locations() {
                    println!(
                        "Unable to solve at {}\n\tat {}:{}",
                        location, file, line
                    );
                }
                //TODO return an error, but for now force the conversion,
                //so we can find where the error occour
                //break;
                panic!("Unable to solve the table")
            }
        }
        println!("Solved Tables");
        for table in tables.drain(..).map(|x| x.convert()) {
            global_scope
                .insert(Rc::clone(&table.name), GlobalScope::Table(table));
        }
        //only convert used macros, AKA the ones that was solved/used by tables
        for pcode in pcode
            .drain(..)
            .filter(|x| x.is_solved())
            .map(|x| x.convert())
        {
            global_scope
                .insert(Rc::clone(&pcode.name), GlobalScope::PcodeMacro(pcode));
        }

        let endian = inner.endian.ok_or(SemanticError::EndianMissing)?;
        let alignment = inner.alignment.unwrap_or(0).try_into().unwrap();
        //TODO check all constructor for tables have export of the same size
        Ok(Self {
            endian,
            alignment,
            global_scope,
        })
    }
    pub fn spaces<'a>(&'a self) -> impl Iterator<Item = Rc<Space>> + 'a {
        self.global_scope
            .values()
            .filter_map(|x| x.unwrap_space())
            .map(Rc::clone)
    }
    pub fn varnodes<'a>(&'a self) -> impl Iterator<Item = Rc<Varnode>> + 'a {
        self.global_scope
            .values()
            .filter_map(|x| x.unwrap_varnode())
            .map(Rc::clone)
    }
    pub fn user_functions<'a>(
        &'a self,
    ) -> impl Iterator<Item = Rc<UserFunction>> + 'a {
        self.global_scope
            .values()
            .filter_map(|x| x.unwrap_user_function())
            .map(Rc::clone)
    }
    pub fn pcode_macros<'a>(
        &'a self,
    ) -> impl Iterator<Item = Rc<PcodeMacro>> + 'a {
        self.global_scope
            .values()
            .filter_map(|x| x.unwrap_pcode_macro())
            .map(Rc::clone)
    }
    pub fn tables<'a>(&'a self) -> impl Iterator<Item = Rc<Table>> + 'a {
        self.global_scope
            .values()
            .filter_map(|x| x.unwrap_table())
            .map(Rc::clone)
    }
    pub fn tokens<'a>(&'a self) -> impl Iterator<Item = Rc<Token>> + 'a {
        self.global_scope
            .values()
            .filter_map(|x| x.unwrap_token())
            .map(Rc::clone)
    }
    pub fn token_fields<'a>(
        &'a self,
    ) -> impl Iterator<Item = Rc<assembly::Assembly>> + 'a {
        self.global_scope
            .values()
            .filter_map(|x| x.unwrap_token_field())
            .map(Rc::clone)
    }
}
