use crate::semantic::{
    ContextId, InstNext, InstStart, Span, TableId, TokenFieldId,
};
use crate::{Number, NumberNonZeroUnsigned, NumberUnsigned, SpaceId};

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
    And,
    Or,
    Xor,
    Asr,
    Lsl,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum VariableType {
    Reference(SpaceId),
    Value(Option<NumberNonZeroUnsigned>),
}

impl VariableType {
    pub(crate) fn set_len(self, len: NumberNonZeroUnsigned) -> Option<Self> {
        match self {
            VariableType::Reference(_) => None,
            VariableType::Value(None) => Some(Self::Value(Some(len))),
            VariableType::Value(Some(current_len)) if current_len == len => {
                Some(self)
            }
            VariableType::Value(Some(_)) => None,
        }
    }
    pub(crate) fn set_space(self, id: SpaceId) -> Option<Self> {
        match self {
            VariableType::Reference(current_id) if current_id == id => {
                Some(self)
            }
            VariableType::Value(None) => Some(Self::Reference(id)),
            VariableType::Reference(_) | VariableType::Value(Some(_)) => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub(crate) name: Box<str>,
    pub location: Span,
    /// a disassembly variable could have a defined len, defined by the export
    pub value_type: VariableType,
}

impl Variable {
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub(crate) rpn: Box<[ExprElement]>,
}

impl Expr {
    pub fn elements(&self) -> &[ExprElement] {
        &self.rpn
    }
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value { value: ReadScope, location: Span },
    Op(Op),
    OpUnary(OpUnary),
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
