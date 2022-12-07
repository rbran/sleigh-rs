use std::ops::ControlFlow;

use thiserror::Error;

use crate::base::IntTypeU;
use crate::{from_error, InputSource};

use super::disassembly::{DisassemblyError, Expr};
use super::table::Table;
use super::token::TokenField;
use super::varnode::Context;
use super::{GlobalReference, InstStart};

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
    #[error("Mix `|` and `&` operations on pattern is forbidden")]
    MixOperations(InputSource),
    #[error("Field produced multiple times at {0} and {1}")]
    MultipleProduction(InputSource, InputSource),
    #[error("Field produced is implicit and abiguous")]
    AmbiguousProduction(InputSource),
    #[error("Pattern in Or statement without constraint")]
    UnrestrictedOr(InputSource),

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
pub struct ProducedTable {
    table: GlobalReference<Table>,
    always: bool,
    recursive: bool,
}

impl ProducedTable {
    pub(crate) fn new(
        table: GlobalReference<Table>,
        always: bool,
        recursive: bool,
    ) -> Self {
        Self {
            table,
            always,
            recursive,
        }
    }
    pub fn table(&self) -> &GlobalReference<Table> {
        &self.table
    }
    pub fn always(&self) -> bool {
        self.always
    }
    pub fn recursive(&self) -> bool {
        self.recursive
    }
}

#[derive(Clone, Debug)]
pub struct ProducedTokenField {
    //if this field is produced explicitly (on pattern) or implicitly deduced
    //the existence of by the use of it in a desassembly/execution
    explicit: bool,
    local: bool,
    field: GlobalReference<TokenField>,
}
impl ProducedTokenField {
    pub(crate) fn new(
        field: GlobalReference<TokenField>,
        local: bool,
        explicit: bool,
    ) -> Self {
        Self {
            field,
            local,
            explicit,
        }
    }
    pub fn token_field(&self) -> &GlobalReference<TokenField> {
        &self.field
    }
    pub fn is_explicit(&self) -> bool {
        self.explicit
    }
    pub fn is_local(&self) -> bool {
        self.local
    }
}

//#[derive(Clone, Debug, Default)]
//pub struct FieldProducts {
//    fields: Box<[ProductTokenField]>,
//    tables: Box<[ProductTable]>,
//}
//impl FieldProducts {
//    pub(crate) fn new(
//        fields: Box<[ProductTokenField]>,
//        tables: Box<[ProductTable]>,
//    ) -> Self {
//        Self { fields, tables }
//    }
//    pub fn tables(&self) -> &[ProductTable] {
//        &self.tables
//    }
//    pub fn fields(&self) -> &[ProductTokenField] {
//        &self.fields
//    }
//}

#[derive(Clone, Debug)]
pub struct Pattern {
    len: PatternLen,
    //products: FieldProducts,
    blocks: Box<[Block]>,
}

impl Pattern {
    pub(crate) fn new(
        blocks: Box<[Block]>,
        len: PatternLen,
        //products: FieldProducts,
    ) -> Self {
        Self {
            blocks,
            len,
            //products,
        }
    }
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }
    pub fn tables(&self) -> impl Iterator<Item = &ProducedTable> {
        self.blocks().iter().map(Block::tables).flatten()
    }
    pub fn token_fields(&self) -> impl Iterator<Item = &ProducedTokenField> {
        self.blocks()
            .iter()
            .map(|block| block.token_fields().iter())
            .flatten()
    }
    pub fn len(&self) -> &PatternLen {
        &self.len
    }
    //pub fn produced(&self) -> &FieldProducts {
    //    &self.products
    //}
}

#[derive(Clone, Debug)]
pub enum Block {
    And {
        len: PatternLen,
        token_len: IntTypeU,
        token_fields: Box<[ProducedTokenField]>,
        tables: Box<[ProducedTable]>,
        verifications: Box<[Verification]>,
    },
    //TODO or block can export token_fields?
    Or {
        len: PatternLen,
        token_fields: Box<[ProducedTokenField]>,
        tables: Box<[ProducedTable]>,
        branches: Box<[Verification]>,
    },
}
impl Block {
    pub fn tables(&self) -> &[ProducedTable] {
        match self {
            Block::And { tables, .. } | Block::Or { tables, .. } => tables,
        }
    }
    pub fn token_fields(&self) -> &[ProducedTokenField] {
        match self {
            Block::And { token_fields, .. }
            | Block::Or { token_fields, .. } => token_fields,
        }
    }
    pub fn len(&self) -> PatternLen {
        match self {
            Block::And { len, .. } | Block::Or { len, .. } => *len,
        }
    }
    pub fn verifications(&self) -> &[Verification] {
        match self {
            Block::And { verifications, .. }
            | Block::Or {
                branches: verifications,
                ..
            } => verifications,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Verification {
    ContextCheck {
        context: GlobalReference<Context>,
        op: CmpOp,
        value: ConstraintValue,
    },
    TableBuild {
        produced_table: ProducedTable,
        verification: Option<(CmpOp, ConstraintValue)>,
    },
    TokenFieldCheck {
        field: GlobalReference<TokenField>,
        op: CmpOp,
        value: ConstraintValue,
    },
    SubPattern {
        location: InputSource,
        pattern: Pattern,
    },
}

#[derive(Clone, Debug)]
pub enum ConstraintField {
    TokenField(GlobalReference<TokenField>),
    Context(GlobalReference<Context>),
    InstStart(GlobalReference<InstStart>),
    Table(GlobalReference<Table>),
}

#[derive(Clone, Debug)]
pub struct ConstraintValue {
    expr: Expr,
}

impl ConstraintValue {
    pub(crate) fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

impl ConstraintValue {
    pub fn expr(&self) -> &Expr {
        &self.expr
    }
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
        block
            .token_fields()
            .iter()
            .try_for_each(|prod| self.token_field(&prod.field))?;
        block
            .tables()
            .iter()
            .try_for_each(|prod| self.table(&prod.table))?;
        block
            .verifications()
            .iter()
            .try_for_each(|ver| self.verification(ver))
    }
    fn token_field(
        &mut self,
        _field: &GlobalReference<TokenField>,
    ) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn table(&mut self, _table: &GlobalReference<Table>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn context(
        &mut self,
        _table: &GlobalReference<Context>,
    ) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn value(&mut self, _value: &ConstraintValue) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn verification(
        &mut self,
        verification: &Verification,
    ) -> ControlFlow<B, ()> {
        match verification {
            Verification::ContextCheck {
                context,
                op: _,
                value,
            } => {
                self.context(context)?;
                self.value(value)
            }
            Verification::TableBuild {
                produced_table,
                verification,
            } => {
                if let Some((_op, value)) = verification {
                    self.value(value)?;
                }
                self.table(&produced_table.table)
            }
            Verification::SubPattern {
                location: _,
                pattern,
            } => self.pattern(pattern),
            Verification::TokenFieldCheck {
                field,
                op: _,
                value,
            } => {
                self.value(value)?;
                self.token_field(field)
            }
        }
    }
}
