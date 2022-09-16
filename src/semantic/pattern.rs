use std::ops::ControlFlow;
use std::rc::Rc;

use thiserror::Error;

use crate::base::IntTypeU;
use crate::{from_error, InputSource, Token};

use super::assembly::Assembly;
use super::disassembly::{DisassemblyError, Expr};
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

impl PatternLen {
    pub fn min(&self) -> IntTypeU {
        match self {
            PatternLen::Defined(min) | PatternLen::Range { min, max: _ } => {
                *min
            }
        }
    }
    pub fn max(&self) -> Option<IntTypeU> {
        match self {
            PatternLen::Defined(value) => Some(*value),
            PatternLen::Range { min: _, max } => *max,
        }
    }
    pub fn defined(&self) -> Option<IntTypeU> {
        match self {
            PatternLen::Defined(value) => Some(*value),
            PatternLen::Range { .. } => None,
        }
    }
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

impl Block {
    pub fn len(&self) -> PatternLen {
        match self {
            Block::And { len, .. } | Block::Or { len, .. } => {
                PatternLen::Defined(*len)
            }
            Block::Expansive { extension, .. } => extension.pattern_len(),
        }
    }
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
    pub fn op(&self) -> &CmpOp {
        &self.op
    }
    pub fn value(&self) -> &ConstraintField {
        &self.value
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
    pub fn src(&self) -> &InputSource {
        &self.src
    }
    pub fn blocks(&self) -> &[SubBlock] {
        &self.blocks
    }
    pub fn len(&self) -> IntTypeU {
        self.len
    }
}

#[derive(Clone, Debug)]
pub enum SubBlock {
    //block with multiple elements unified with ORs
    Or {
        token: Option<Rc<Token>>,
        len: IntTypeU,
        fields: Vec<FieldOr>,
    },
    //block with multiple elements unified with ANDs
    And {
        token: Option<Rc<Token>>,
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
    pub expr: Expr,
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

pub trait PatternWalker<B = ()> {
    fn start(&mut self, pattern: &Pattern) -> ControlFlow<B, ()> {
        pattern
            .blocks
            .iter()
            .try_for_each(|block| self.block(block))
    }
    fn block(&mut self, block: &Block) -> ControlFlow<B, ()> {
        match block {
            Block::Or { fields, .. } => {
                fields.iter().try_for_each(|field| self.field_or(field))
            }
            Block::And { fields, .. } => {
                fields.iter().try_for_each(|field| self.field_and(field))
            }
            Block::Expansive {
                left,
                extension,
                right,
                ..
            } => {
                left.iter().try_for_each(|field| self.field_and(field))?;
                right.iter().try_for_each(|field| self.field_and(field))?;
                self.extension(extension)
            }
        }
    }
    fn field_or(&mut self, field: &FieldOr) -> ControlFlow<B, ()> {
        match field {
            FieldOr::Constraint { assembly, .. } => self.assembly(assembly),
            FieldOr::SubPattern(sub_pattern) => self.sub_pattern(sub_pattern),
        }
    }
    fn field_and(&mut self, field: &FieldAnd) -> ControlFlow<B, ()> {
        match field {
            FieldAnd::Constraint {
                field: ConstraintVariable::Assembly { assembly, .. },
                ..
            } => self.assembly(assembly),
            FieldAnd::Constraint {
                field: ConstraintVariable::Varnode { varnode, .. },
                ..
            } => self.varnode(varnode),
            FieldAnd::Field(field) => self.reference(field),
            FieldAnd::SubPattern(sub_pattern) => self.sub_pattern(sub_pattern),
        }
    }
    fn assembly(&mut self, _assembly: &Rc<Assembly>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn varnode(&mut self, _varnode: &Rc<Varnode>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn extension(&mut self, _table: &Rc<Table>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn sub_pattern(&mut self, sub_pattern: &SubPattern) -> ControlFlow<B, ()> {
        sub_pattern
            .blocks
            .iter()
            .try_for_each(|block| self.sub_block(block))
    }
    fn reference(&mut self, reference: &Reference) -> ControlFlow<B, ()> {
        match reference {
            Reference::Assembly { assembly, .. } => self.assembly(assembly),
            Reference::Varnode { varnode, .. } => self.varnode(varnode),
            Reference::Table { table, .. } => self.table(table),
        }
    }
    fn table(&mut self, _table: &Rc<Table>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn sub_block(&mut self, sub_block: &SubBlock) -> ControlFlow<B, ()> {
        match sub_block {
            SubBlock::Or { fields, .. } => {
                fields.iter().try_for_each(|field| self.field_or(field))
            }
            SubBlock::And { fields, .. } => {
                fields.iter().try_for_each(|field| self.field_and(field))
            }
        }
    }
}
