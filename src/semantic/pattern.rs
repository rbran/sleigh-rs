use std::rc::Rc;

use thiserror::Error;

use crate::{from_error, InputSource};

use super::assembly::Assembly;
use super::disassembly::DisassemblyError;
use super::inner::disassembly;
use super::table::Table;
use super::varnode::Varnode;

#[derive(Clone, Debug, Error)]
pub enum PatternError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
    #[error("Missing Ref {0}")]
    MissingRef(InputSource),
    #[error("Unable to merge Blocks mixing & and | {0}")]
    InvalidMixOp(InputSource),

    #[error("Invalid assignment Error")]
    ConstraintExpr(DisassemblyError),
}
from_error!(PatternError, DisassemblyError, ConstraintExpr);

#[derive(Clone, Debug)]
pub struct Pattern {
    pub blocks: Vec<Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub op: Option<Op>,
    pub elements: Vec<Element>,
}

#[derive(Clone, Debug)]
pub struct Element {
    pub field: Field,
    pub ellipsis: Option<Ellipsis>,
}
#[derive(Clone, Debug)]
pub enum Field {
    Field {
        field: Reference,
        constraint: Option<Constraint>,
    },
    SubPattern(Pattern),
}
#[derive(Clone, Debug)]
pub enum Reference {
    Assembly(Rc<Assembly>),
    Varnode(Rc<Varnode>),
    Table(Rc<Table>),
}

#[derive(Clone, Debug)]
pub struct Constraint {
    pub op: CmpOp,
    pub value: ConstraintValue,
}

#[derive(Clone, Debug)]
pub enum ExprScope {
    Varnode(Rc<Varnode>), //Context only
    Assembly(Rc<Assembly>),
}

#[derive(Clone, Debug)]
pub struct ConstraintValue {
    pub expr: disassembly::Expr,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Op {
    And,
    Or,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Ellipsis {
    Left,
    Right,
}
