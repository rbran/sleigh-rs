use std::ops::ControlFlow;
use std::rc::Rc;

use thiserror::Error;

use crate::base::IntTypeU;
use crate::{from_error, InputSource};

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

    #[error("Invalid Recursive at {0}")]
    InvalidRecursive(InputSource),
    #[error("In Or block, all elements need to have the same len")]
    InvalidOrLen(InputSource),
    #[error("Each patern can only have a single recursive")]
    MultipleRecursives(InputSource),

    #[error("Invalid assignment Error")]
    ConstraintExpr(DisassemblyError),
}
from_error!(PatternError, DisassemblyError, ConstraintExpr);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PatternLen {
    Defined(IntTypeU),
    Range { min: IntTypeU, max: IntTypeU },
    Min(IntTypeU),
}

impl PatternLen {
    pub fn is_recursive(&self) -> bool {
        matches!(self, Self::Min(_))
    }
    pub fn min(&self) -> IntTypeU {
        match self {
            Self::Min(min)
            | Self::Defined(min)
            | Self::Range { min, max: _ } => *min,
        }
    }
    pub fn max(&self) -> Option<IntTypeU> {
        match self {
            Self::Defined(value) => Some(*value),
            Self::Range { min: _, max } => Some(*max),
            Self::Min(_) => None,
        }
    }
    pub fn defined(&self) -> Option<IntTypeU> {
        match self {
            Self::Defined(value) => Some(*value),
            Self::Min(_) | Self::Range { .. } => None,
        }
    }
}
#[derive(Clone, Debug)]
pub struct FieldProductTable {
    table: Rc<Table>,
    always: bool,
    recursive: bool,
}

impl FieldProductTable {
    pub(crate) fn new(table: Rc<Table>, always: bool, recursive: bool) -> Self {
        Self {
            table,
            always,
            recursive,
        }
    }
    pub fn table(&self) -> &Rc<Table> {
        &self.table
    }
    pub fn always(&self) -> bool {
        self.always
    }
    pub fn recursive(&self) -> bool {
        self.recursive
    }
}

#[derive(Clone, Debug, Default)]
pub struct FieldProducts {
    fields: Vec<Rc<Assembly>>,
    tables: Vec<FieldProductTable>,
}
impl FieldProducts {
    pub fn new(
        fields: Vec<Rc<Assembly>>,
        tables: Vec<FieldProductTable>,
    ) -> Self {
        Self { fields, tables }
    }
    pub fn tables(&self) -> &[FieldProductTable] {
        &self.tables
    }
    pub fn fields(&self) -> &[Rc<Assembly>] {
        &self.fields
    }
}

#[derive(Clone, Debug)]
pub struct Pattern {
    len: PatternLen,
    products: FieldProducts,
    blocks: Vec<Block>,
}

impl Pattern {
    pub(crate) fn new(
        blocks: Vec<Block>,
        len: PatternLen,
        products: FieldProducts,
    ) -> Self {
        Self {
            blocks,
            len,
            products,
        }
    }
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }
    pub fn len(&self) -> &PatternLen {
        &self.len
    }
    pub fn produced(&self) -> &FieldProducts {
        &self.products
    }
}

#[derive(Clone, Debug)]
pub enum Block {
    ///block with multiple elements unified with ORs
    Or {
        len: IntTypeU,
        fields: Vec<FieldOr>,
        products: FieldProducts,
    },
    ///block with multiple elements unified with ANDs
    And {
        left_len: PatternLen,
        left: Vec<FieldAnd>,
        right_len: PatternLen,
        right: Vec<FieldAnd>,
        products: FieldProducts,
    },
}
impl Block {
    pub fn produced(&self) -> &FieldProducts {
        match self {
            Block::Or { products, .. } | Block::And { products, .. } => {
                products
            }
        }
    }
}

//Field used in Or Expressions
#[derive(Clone, Debug)]
pub enum FieldOr {
    Constraint {
        src: InputSource,
        field: ConstraintVariable,
        constraint: Constraint,
        implicit_fields: Vec<Rc<Assembly>>,
    },
    SubPattern {
        src: InputSource,
        sub: Pattern,
    },
}
#[derive(Clone, Debug)]
pub enum FieldAnd {
    Constraint {
        field: ConstraintVariable,
        constraint: Constraint,
    },
    Field(Reference),
    SubPattern {
        src: InputSource,
        sub: Pattern,
    },
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
    Table {
        self_ref: bool,
        src: InputSource,
        table: Rc<Table>,
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
    fn pattern(&mut self, pattern: &Pattern) -> ControlFlow<B, ()> {
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
            Block::And { left, right, .. } => {
                left.iter().try_for_each(|field| self.field_and(field))?;
                right.iter().try_for_each(|field| self.field_and(field))
            }
        }
    }
    fn field_or(&mut self, field: &FieldOr) -> ControlFlow<B, ()> {
        match field {
            FieldOr::Constraint {
                field: ConstraintVariable::Assembly { assembly, .. },
                ..
            } => self.assembly(assembly),
            FieldOr::Constraint {
                field: ConstraintVariable::Varnode { varnode, .. },
                ..
            } => self.varnode(varnode),
            FieldOr::SubPattern { sub, .. } => self.pattern(sub),
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
            FieldAnd::SubPattern { sub, .. } => self.pattern(sub),
        }
    }
    fn assembly(&mut self, _assembly: &Rc<Assembly>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn varnode(&mut self, _varnode: &Rc<Varnode>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn extension(&mut self, table: &Rc<Table>) -> ControlFlow<B, ()> {
        self.table(table)
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
}
