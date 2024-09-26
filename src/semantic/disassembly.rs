use crate::semantic::{
    ContextId, InstNext, InstStart, Span, TableId, TokenFieldId,
};
use crate::{Number, NumberUnsigned};

#[derive(Clone, Debug, Copy)]
pub enum OpUnary {
    Negation,
    Negative,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Asr,
    Lsl,
}

#[derive(Clone, Copy, Debug)]
pub enum ReadScope {
    //TODO: table??? Handle tables that the execution is just export Disassembly
    //Table(TableId),
    Integer(Number),
    Context(ContextId),
    TokenField(TokenFieldId),
    InstStart(InstStart),
    InstNext(InstNext),
    Local(VariableId),
}

#[derive(Clone, Debug)]
pub enum WriteScope {
    Context(ContextId),
    Local(VariableId),
}

#[derive(Clone, Debug)]
pub enum AddrScope {
    Integer(NumberUnsigned),
    Table(TableId),
    //Varnode(VarnodeId),
    //TokenField(TokenFieldId),
    InstStart(InstStart),
    InstNext(InstNext),
    Local(VariableId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VariableId(pub usize);

#[derive(Clone, Debug)]
pub struct Variable {
    pub(crate) name: Box<str>,
    pub location: Span,
    // TODO: a disassembly variable could have a defined len, defined by the export
    //pub value_type: Cell<VariableType>,
}

impl Variable {
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(Span, Op, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value { value: ReadScope, location: Span },
    Op(Span, OpUnary, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct GlobalSet {
    pub location: Span,
    pub address: AddrScope,
    pub context: ContextId,
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub left: WriteScope,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub enum Assertation {
    GlobalSet(GlobalSet),
    Assignment(Assignment),
}
