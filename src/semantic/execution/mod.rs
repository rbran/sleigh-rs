use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use thiserror::Error;

use crate::base::{IntTypeU, NonZeroTypeU};
use crate::{InputSource, UserFunction};

use super::pcode_macro::PcodeMacroInstance;
use super::table::{Table, TableError, TableErrorSub};
use super::varnode::Varnode;
use super::{assembly, disassembly};

mod op;

pub use self::op::{
    AddrDereference, Binary, DefinedFunction, Function, PrimitiveFunction,
    Truncate, Unary,
};

#[derive(Clone, Debug, Error)]
pub enum ExecutionError {
    #[error("Invalid Ref: {0}")]
    InvalidRef(InputSource),
    #[error("Missing Ref: {0}")]
    MissingRef(InputSource),

    //TODO migrate this to PcodeMacro
    #[error("Macro don't allow Build statements")]
    MacroBuildInvalid,

    #[error("Invalid Var declaration {0}")]
    InvalidVarDeclare(InputSource),
    #[error("Label invalid {0}")]
    InvalidLabel(InputSource),
    #[error("Label not found {0}")]
    MissingLabel(InputSource),
    #[error("Label Duplicated {0}")]
    DuplicatedLabel(InputSource),

    #[error("Default address space not found")]
    DefaultSpace, //TODO src
    #[error("Can only `goto` to a label")]
    InvalidLocalGoto, //TODO src
    #[error("TableConstructor have diferent return types")]
    InvalidExport,
    #[error("BitRange can't be zero")]
    BitRangeZero, //TODO src

    #[error("Can't apply op to variable due to size at {0}")]
    VarSize(InputSource), //TODO sub-type error

    #[error("Call user Function with invalid param numbers {0}")]
    UserFunctionParamNumber(InputSource),
    #[error("Call user Function with invalid return Size {0}")]
    UserFunctionReturnSize(InputSource),

    //TODO remove this
    #[error("Invalid amb1: {0}")]
    InvalidAmb1(InputSource),
}

impl ExecutionError {
    pub fn to_table(self, table_pos: InputSource) -> TableError {
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
    Value { value: IntTypeU, size: NonZeroTypeU },
    AttachVarnode(Rc<assembly::Field>),
    TableExport(Rc<Table>),
}

#[derive(Clone, Debug)]
pub struct UserCall {
    function: Rc<UserFunction>,
    pub params: Vec<Expr>,
}
impl UserCall {
    pub fn new(params: Vec<Expr>, function: Rc<UserFunction>) -> Self {
        Self { params, function }
    }
}

#[derive(Clone, Debug)]
pub enum ReferencedValue {
    //only if translate into varnode
    Assembly(Rc<assembly::Assembly>),
    Table(Rc<Table>),
    //Param(InputSource, Rc<Parameter>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(NonZeroTypeU, Binary, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ExprValue),
    UserCall(UserCall),
    Reference(NonZeroTypeU, ReferencedValue),
    Op(NonZeroTypeU, Unary, Box<Expr>),
    New(Box<Expr>, Option<Box<Expr>>),
    CPool(Vec<Expr>),
}

#[derive(Clone, Debug)]
pub enum ExprValue {
    Int(IntTypeU),
    DisVar(Rc<disassembly::Variable>),
    Assembly(Rc<assembly::Assembly>),
    Varnode(Rc<Varnode>),
    Table(Rc<Table>),
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
    direct: bool,
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
    Varnode(Rc<Varnode>),
    Assembly(Rc<assembly::Assembly>), //only with attach variable
    TableExport(Rc<Table>),
    Local(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub enum ReadValue {
    //value is always unsigned, negatives are preceded by an '-' op
    Integer(IntTypeU),
    Disassembly(Rc<disassembly::Variable>),
    Assembly(Rc<assembly::Assembly>),
    Varnode(Rc<Varnode>),
    TableExport(Rc<Table>),
    Local(Rc<Variable>),
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
    pub table: Rc<Table>,
}

#[derive(Clone, Debug)]
pub struct MacroCall {
    pub params: Vec<Expr>,
    pub function: Rc<PcodeMacroInstance>,
}
impl MacroCall {
    pub fn new(params: Vec<Expr>, function: Rc<PcodeMacroInstance>) -> Self {
        Self { params, function }
    }
}

#[derive(Clone, Debug)]
pub enum ExportConst {
    DisVar(Rc<disassembly::Variable>),
    Assembly(Rc<assembly::Assembly>),
}
#[derive(Clone, Debug)]
pub enum Export {
    Const(NonZeroTypeU, ExportConst),
    Value(Expr),
    Reference(Expr, AddrDereference),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Delayslot(IntTypeU),
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
    pub blocks: HashMap<Rc<str>, Rc<Block>>,
    pub vars: HashMap<Rc<str>, Rc<Variable>>,

    //entry_block have no name and is not on self.labels
    pub entry_block: Rc<Block>,
}

impl Default for Execution {
    fn default() -> Self {
        let entry_block = Block::new_empty(None);
        Execution {
            blocks: HashMap::default(),
            vars: HashMap::default(),
            entry_block: Rc::new(entry_block),
        }
    }
}
