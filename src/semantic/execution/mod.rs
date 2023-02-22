use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

use thiserror::Error;

use crate::{NumberNonZeroUnsigned, NumberUnsigned, Span, Number};

use super::pcode_macro::PcodeMacroInstance;
use super::table::{Table, TableError, TableErrorSub};
use super::token::TokenField;
use super::user_function::UserFunction;
use super::varnode::{Bitrange, Context, Varnode};
use super::{disassembly, GlobalReference, InstNext, InstStart};

mod op;

pub use self::op::{
    AddrDereference, Binary, DefinedFunction, Function, PrimitiveFunction,
    Truncate, Unary,
};

#[derive(Clone, Debug, Error)]
pub enum ExecutionError {
    #[error("Invalid Ref: {0}")]
    InvalidRef(Span),
    #[error("Missing Ref: {0}")]
    MissingRef(Span),

    //TODO migrate this to PcodeMacro
    #[error("Macro don't allow Build statements")]
    MacroBuildInvalid,

    #[error("Invalid Var declaration {0}")]
    InvalidVarDeclare(Span),
    #[error("Invalid Var Len {0}")]
    InvalidVarLen(Span),
    #[error("Label invalid {0}")]
    InvalidLabel(Span),
    #[error("Label not found {0}")]
    MissingLabel(Span),
    #[error("Label Duplicated {0}")]
    DuplicatedLabel(Span),

    #[error("Default address space not found")]
    DefaultSpace, //TODO src
    #[error("Can only `goto` to a label")]
    InvalidLocalGoto, //TODO src
    #[error("TableConstructor have diferent return types")]
    InvalidExport,
    #[error("BitRange can't be zero")]
    BitRangeZero, //TODO src

    #[error("Can't apply op to variable due to size at {0}")]
    VarSize(Span), //TODO sub-type error

    #[error("Call user Function with invalid param numbers {0}")]
    UserFunctionParamNumber(Span),
    #[error("Call user Function with invalid return Size {0}")]
    UserFunctionReturnSize(Span),

    //TODO remove this
    #[error("Invalid amb1: {0}")]
    InvalidAmb1(Span),
}

impl ExecutionError {
    pub fn to_table(self, table_pos: Span) -> TableError {
        TableError {
            table_pos,
            sub: TableErrorSub::Execution(self),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: Rc<str>,
}

impl Variable {
    pub fn new(name: Rc<str>) -> Self {
        Variable { name }
    }
}

#[derive(Clone, Debug)]
pub enum VariableScope {
    Value {
        value: NumberUnsigned,
        size: NumberNonZeroUnsigned,
    },
    AttachVarnode(GlobalReference<TokenField>),
    TableExport(GlobalReference<Table>),
}

#[derive(Clone, Debug)]
pub struct UserCall {
    pub function: GlobalReference<UserFunction>,
    pub params: Vec<Expr>,
}
impl UserCall {
    pub fn new(
        params: Vec<Expr>,
        function: GlobalReference<UserFunction>,
    ) -> Self {
        Self { params, function }
    }
}

#[derive(Clone, Debug)]
pub enum ReferencedValue {
    //only if translate into varnode
    TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Table(GlobalReference<Table>),
    //Param(InputSource, Rc<Parameter>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(NumberNonZeroUnsigned, Binary, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ExprValue),
    UserCall(UserCall),
    Reference(NumberNonZeroUnsigned, ReferencedValue),
    Op(NumberNonZeroUnsigned, Unary, Box<Expr>),
    New(Box<Expr>, Option<Box<Expr>>),
    CPool(Vec<Expr>),
}

#[derive(Clone, Debug)]
pub enum ExprValue {
    Int(Number),
    TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Varnode(GlobalReference<Varnode>),
    Context(GlobalReference<Context>),
    Bitrange(GlobalReference<Bitrange>),
    Table(GlobalReference<Table>),
    DisVar(Rc<disassembly::Variable>),
    ExeVar(Rc<Variable>),
    //Param(Rc<Parameter>),
}

#[derive(Clone, Debug, Copy)]
pub enum BranchCall {
    Goto,
    Call,
    Return,
}

#[derive(Clone, Debug)]
pub struct CpuBranch {
    pub cond: Option<Expr>,
    pub call: BranchCall,
    pub direct: bool,
    pub dst: Expr,
}

impl CpuBranch {
    pub fn new(
        cond: Option<Expr>,
        call: BranchCall,
        direct: bool,
        dst: Expr,
    ) -> Self {
        Self {
            cond,
            call,
            direct,
            dst,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LocalGoto {
    pub cond: Option<Expr>,
    pub dst: Rc<Block>,
}

#[derive(Clone, Debug)]
pub enum WriteValue {
    Varnode(GlobalReference<Varnode>),
    Bitrange(GlobalReference<Bitrange>),
    TokenField(GlobalReference<TokenField>), //only with attach variable
    TableExport(GlobalReference<Table>),
    Local(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub enum ReadValue {
    //value is always unsigned, negatives are preceded by an '-' op
    Integer(NumberUnsigned),
    DisVar(Rc<disassembly::Variable>),
    TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Varnode(GlobalReference<Varnode>),
    Context(GlobalReference<Context>),
    Bitrange(GlobalReference<Bitrange>),
    TableExport(GlobalReference<Table>),
    ExeVar(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub var: WriteValue,
    pub op: Option<Truncate>,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub struct MemWrite {
    pub addr: Expr,
    pub mem: AddrDereference,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub struct WriteAddr {
    pub space: AddrDereference,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct AssignmentVar {
    pub var: VariableScope,
    pub op: Option<Truncate>,
}

#[derive(Clone, Debug)]
pub struct Build {
    pub table: GlobalReference<Table>,
}

#[derive(Clone, Debug)]
pub struct MacroCall {
    pub params: Box<[Expr]>,
    pub function: Rc<PcodeMacroInstance>,
}
impl MacroCall {
    pub fn new(params: Box<[Expr]>, function: Rc<PcodeMacroInstance>) -> Self {
        Self { params, function }
    }
}

#[derive(Clone, Debug)]
pub enum ExportConst {
    DisVar(Rc<disassembly::Variable>),
    TokenField(GlobalReference<TokenField>),
    Context(GlobalReference<Context>),
    Table(GlobalReference<Table>),
}
#[derive(Clone, Debug)]
pub enum Export {
    Const(NumberNonZeroUnsigned, ExportConst),
    Value(Expr),
    Reference(Expr, AddrDereference),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Delayslot(NumberUnsigned),
    Export(Export),
    CpuBranch(CpuBranch),
    LocalGoto(LocalGoto),
    UserCall(UserCall),
    MacroCall(MacroCall),
    Build(Build),
    Declare(Rc<Variable>),
    Assignment(Assignment),
    MemWrite(MemWrite),
}

#[derive(Clone, Debug)]
pub struct Block {
    //None is entry block
    pub name: Option<Rc<str>>,
    pub next: RefCell<Option<Rc<Block>>>,
    pub statements: RefCell<Vec<Statement>>,
}

impl Block {
    pub fn new_empty(name: Option<Rc<str>>) -> Self {
        Block {
            name,
            next: RefCell::new(None),
            statements: RefCell::new(vec![]),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Execution {
    pub blocks: IndexMap<Rc<str>, Rc<Block>>,
    pub vars: IndexMap<Rc<str>, Rc<Variable>>,

    //entry_block have no name and is not on self.labels
    pub entry_block: Rc<Block>,
}

impl Default for Execution {
    fn default() -> Self {
        let entry_block = Block::new_empty(None);
        Execution {
            blocks: IndexMap::default(),
            vars: IndexMap::default(),
            entry_block: Rc::new(entry_block),
        }
    }
}
