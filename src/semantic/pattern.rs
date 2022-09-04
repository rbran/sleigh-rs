use std::rc::Rc;

use thiserror::Error;

use crate::base::IntTypeU;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PatternLen {
    Defined(IntTypeU),
    Range {
        min: IntTypeU,
        max: Option<IntTypeU>,
    },
}

#[derive(Clone, Debug)]
pub struct Pattern {
    len: PatternLen,
    blocks: Vec<Block>,
}

impl Pattern {
    pub fn new(len: PatternLen, blocks: Vec<Block>) -> Self {
        Self { len, blocks }
    }
    pub fn len(&self) -> PatternLen {
        self.len
    }
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }
}

#[derive(Clone, Debug)]
pub enum Block {
    //block with multiple elements unified with ORs
    Or {
        len: IntTypeU,
        fields: Vec<FieldOr>,
    },
    //block with multiple elements unified with ANDs
    And {
        len: IntTypeU,
        fields: Vec<FieldAnd>,
    },
    //And block but with one or more sub tables that extend the size
    Expansive {
        //left/right len need to be smaller then extension min_len
        left_len: IntTypeU,
        left: Vec<FieldAnd>,
        //extension can also be Pattern, but I'll forbiden for now
        extension: Rc<Table>,
        right_len: IntTypeU,
        right: Vec<FieldAnd>,
    },
}

//Field used in Or Expressions
#[derive(Clone, Debug)]
pub enum FieldOr {
    Constraint {
        src: InputSource,
        assembly: Rc<Assembly>,
        constraint: Constraint,
    },
    SubPattern(SubPattern),
}
#[derive(Clone, Debug)]
pub enum FieldAnd {
    Constraint {
        field: ConstraintVariable,
        constraint: Constraint,
    },
    Field(Reference),
    SubPattern(SubPattern),
}

#[derive(Clone, Debug)]
pub struct Constraint {
    op: CmpOp,
    value: ConstraintField,
}

impl Constraint {
    pub fn new(op: CmpOp, value: ConstraintField) -> Self {
        Self { op, value }
    }
}

#[derive(Clone, Debug)]
pub enum ConstraintVariable {
    Assembly {
        src: InputSource,
        assembly: Rc<Assembly>,
    },
    Varnode {
        src: InputSource,
        varnode: Rc<Varnode>,
    },
}

#[derive(Clone, Debug)]
pub enum Reference {
    Assembly {
        src: InputSource,
        assembly: Rc<Assembly>,
    },
    Varnode {
        src: InputSource,
        varnode: Rc<Varnode>,
    },
    //tables that extend are not allowed here
    Table {
        src: InputSource,
        table: Rc<Table>,
    },
}

#[derive(Clone, Debug)]
pub struct SubPattern {
    src: InputSource,
    blocks: Vec<SubBlock>,
    //sub_pattern need to have a defined len
    len: IntTypeU,
}
impl SubPattern {
    pub fn new(
        src: InputSource,
        blocks: Vec<SubBlock>,
        //sub_pattern need to have a defined len
        len: IntTypeU,
    ) -> Self {
        Self { src, blocks, len }
    }
}

#[derive(Clone, Debug)]
pub enum SubBlock {
    //block with multiple elements unified with ORs
    Or {
        len: IntTypeU,
        fields: Vec<FieldOr>,
    },
    //block with multiple elements unified with ANDs
    And {
        len: IntTypeU,
        fields: Vec<FieldAnd>,
    },
}

#[derive(Clone, Debug)]
pub enum ExprScope {
    Varnode(Rc<Varnode>), //Context only
    Assembly(Rc<Assembly>),
}

#[derive(Clone, Debug)]
pub struct ConstraintField {
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
