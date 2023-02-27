use std::rc::Rc;

use thiserror::Error;

use crate::semantic::varnode::Varnode;
use crate::{Number, NumberUnsigned, Span};

use super::table::Table;
use super::token::TokenField;
use super::varnode::Context;
use super::{GlobalReference, InstNext, InstStart};

#[derive(Clone, Debug, Error)]
pub enum DisassemblyError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),

    #[error("GlobalSet Address Ref missing {0}")]
    GlobalsetAddressMissing(Span),
    #[error("GlobalSet Address Ref invalid {0}")]
    GlobalsetAddressInvalid(Span),

    #[error("GlobalSet Address Ref not a context {0}")]
    GlobalsetAddressNotContext(Span),
}

#[derive(Clone, Debug, Copy)]
pub enum OpUnary {
    Negation,
    Negative,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Asr,
    Lsl,
    And,
    Or,
    Xor,
}

#[derive(Clone, Debug)]
pub enum ReadScope {
    //TODO: table??? Handle tables that the execution is just export Disassembly
    //Table(Reference<GlobalElement<Table>>),
    Integer(Number),
    Context(GlobalReference<Context>),
    TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Local(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub enum WriteScope {
    Context(GlobalReference<Context>),
    Local(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub enum AddrScope {
    Integer(NumberUnsigned),
    Table(GlobalReference<Table>),
    Varnode(GlobalReference<Varnode>),
    //TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Local(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub struct Variable {
    name: Rc<str>,
    //TODO
}

impl Variable {
    pub(crate) fn new(name: Rc<str>) -> Self {
        Self { name }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug)]
pub struct Expr {
    rpn: Box<[ExprElement]>,
}

impl Expr {
    pub(crate) fn new(rpn: Box<[ExprElement]>) -> Self {
        Self { rpn }
    }
    pub fn elements(&self) -> &[ExprElement] {
        &self.rpn
    }
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ReadScope),
    Op(Op),
    OpUnary(OpUnary),
}

#[derive(Clone, Debug)]
pub struct GlobalSet {
    //pub src: InputSource,
    pub address: AddrScope,
    pub context: GlobalReference<Context>,
}

impl GlobalSet {
    pub fn new(address: AddrScope, context: GlobalReference<Context>) -> Self {
        Self { address, context }
    }
    pub fn address(&self) -> &AddrScope {
        &self.address
    }
    pub fn context(&self) -> &GlobalReference<Context> {
        &self.context
    }
    //pub fn src(&self) -> &InputSource {
    //    &self.src
    //}
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub left: WriteScope,
    pub right: Expr,
}

impl Assignment {
    pub(crate) fn new(left: WriteScope, right: Expr) -> Self {
        Self { left, right }
    }
    pub fn left(&self) -> &WriteScope {
        &self.left
    }
    pub fn right(&self) -> &Expr {
        &self.right
    }
}

#[derive(Clone, Debug)]
pub enum Assertation {
    GlobalSet(GlobalSet),
    Assignment(Assignment),
}
