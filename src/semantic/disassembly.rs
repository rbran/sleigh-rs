use std::collections::HashMap;
use std::rc::Rc;

use thiserror::Error;

use crate::base::IntTypeU;
use crate::semantic::varnode::Varnode;
use crate::InputSource;

use super::assembly::Assembly;
use super::table::Table;

#[derive(Clone, Debug, Error)]
pub enum DisassemblyError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
    #[error("Missing Ref {0}")]
    MissingRef(InputSource),

    #[error("GlobalSet Address Ref missing {0}")]
    GlobalsetAddressMissing(InputSource),
    #[error("GlobalSet Address Ref invalid {0}")]
    GlobalsetAddressInvalid(InputSource),

    #[error("GlobalSet Address Ref not a context {0}")]
    GlobalsetAddressNotContext(InputSource),
}

#[derive(Clone, Debug, Copy)]
pub enum OpUnary {
    Negation,
    Negative,
}

#[derive(Clone, Debug, Copy)]
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
    //Table(Rc<Table>),
    Integer(IntTypeU),
    Varnode(Rc<Varnode>),
    Assembly(Rc<Assembly>), //not including epsilon
    Local(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub enum WriteScope {
    Varnode(Rc<Varnode>), //context only
    Local(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub enum AddrScope {
    Int(IntTypeU),
    Table(Rc<Table>),
    Varnode(Rc<Varnode>),
    Assembly(Rc<Assembly>),
    Local(Rc<Variable>),
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: Rc<str>,
    //TODO
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub rpn: Vec<ExprElement>,
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ReadScope),
    Op(Op),
    OpUnary(OpUnary),
}
//impl<T> ExprElement<T> {
//    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> ExprElement<U> {
//        match self {
//            Self::Value(i) => ExprElement::Value(i.map(|i| f(i))),
//            Self::Op(op) => ExprElement::Op(op),
//            Self::OpUnary(op) => ExprElement::OpUnary(op),
//        }
//    }
//}
//impl<T, E> ExprElement<Result<T, E>> {
//    pub fn transpose(self) -> Result<ExprElement<T>, E> {
//        match self {
//            Self::Value(x) => match x.transpose() {
//                Ok(x) => Ok(ExprElement::Value(x)),
//                Err(e) => Err(e),
//            },
//            Self::Op(op) => Ok(ExprElement::Op(op)),
//            Self::OpUnary(op) => Ok(ExprElement::OpUnary(op)),
//        }
//    }
//}

#[derive(Clone, Debug)]
pub struct GlobalSet {
    pub address: AddrScope,
    pub context: Rc<Varnode>, //context only
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

#[derive(Clone, Debug, Default)]
pub struct Disassembly {
    //TODO disassembly need to be separated between before and after pattern
    //match but for now it is just a flag that make everything before or
    //after if there is an inst_next on the execution.
    pub pos: bool,
    pub vars: HashMap<Rc<str>, Rc<Variable>>,
    pub assertations: Vec<Assertation>,
}
impl Disassembly {
    pub fn variable(&self, name: &str) -> Option<Rc<Variable>> {
        self.vars.get(name).map(|x| Rc::clone(x))
    }
}
