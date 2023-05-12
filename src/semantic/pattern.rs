use std::ops::ControlFlow;
use std::rc::Rc;

use thiserror::Error;

use crate::semantic::inner::pattern::{BitConstraint, SinglePatternOrdering};
use crate::{from_error, Endian, NumberUnsigned, Span};

use super::disassembly::{Assertation, DisassemblyError, Expr, Variable};
use super::table::Table;
use super::token::TokenField;
use super::varnode::Context;
use super::{GlobalReference, InstStart};

#[derive(Clone, Debug, Error)]
pub enum PatternError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),
    #[error("Unable to merge Blocks mixing & and | {0}")]
    InvalidMixOp(Span),

    #[error("Invalid Recursive at {0}")]
    InvalidRecursive(Span),
    #[error("In Or block, all elements need to have the same len")]
    InvalidOrLen(Span),
    #[error("Each patern can only have a single recursive")]
    MultipleRecursives(Span),
    #[error("Mix `|` and `&` operations on pattern is forbidden")]
    MixOperations(Span),
    #[error("Field produced multiple times at {0} and {1}")]
    MultipleProduction(Span, Span),
    #[error("Field produced is implicit and abiguous")]
    AmbiguousProduction(Span),
    #[error("Pattern in Or statement without constraint")]
    UnrestrictedOr(Span),

    #[error("Invalid assignment Error")]
    ConstraintExpr(DisassemblyError),
}
from_error!(PatternError, DisassemblyError, ConstraintExpr);

// each bit have four possible states:
// mask 1, value X: bit is restricted to `value`
// mask 0, value 0: bit is unrestricted
// mask 0, value 1: bit is restricted but value can't be defined at compile time
#[derive(Clone, Copy, Default, Debug)]
pub struct PatternByte {
    pub(crate) value: u8,
    pub(crate) mask: u8,
}

impl PatternByte {
    pub fn defined_value(&self) -> u8 {
        self.value & self.mask
    }

    pub fn defined_bits(&self) -> u8 {
        self.mask
    }

    pub fn defined_bit(&self, bit: u8) -> bool {
        (self.mask >> bit) & 1 != 0
    }

    pub fn defined_bit_value(&self, bit: u8) -> Option<bool> {
        self.defined_bit(bit).then(|| (self.value >> bit) & 1 != 0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PatternLen {
    Defined(NumberUnsigned),
    Range {
        min: NumberUnsigned,
        max: NumberUnsigned,
    },
    Min(NumberUnsigned),
}

impl PatternLen {
    /// new range/defined len for a pattern
    pub fn new(min: NumberUnsigned, max: NumberUnsigned) -> Self {
        match min.cmp(&max) {
            std::cmp::Ordering::Greater => {
                unreachable!("PatternLen min({}) > max({})", min, max)
            }
            std::cmp::Ordering::Equal => Self::Defined(min),
            std::cmp::Ordering::Less => Self::Range { min, max },
        }
    }
    /// is this pattern can contain itself and grown to infinite
    pub fn is_recursive(&self) -> bool {
        matches!(self, Self::Min(_))
    }
    /// min size of the token this patterns requires to match
    pub fn min(&self) -> NumberUnsigned {
        match self {
            Self::Min(min)
            | Self::Defined(min)
            | Self::Range { min, max: _ } => *min,
        }
    }
    /// max size of the token this patterns requires to match
    pub fn max(&self) -> Option<NumberUnsigned> {
        match self {
            Self::Defined(value) => Some(*value),
            Self::Range { min: _, max } => Some(*max),
            Self::Min(_) => None,
        }
    }
    /// token size, if this pattern is non-growing
    pub fn single_len(&self) -> Option<NumberUnsigned> {
        match self {
            Self::Defined(value) => Some(*value),
            Self::Min(_) | Self::Range { .. } => None,
        }
    }
    /// create a new range calculated from the concatenation of the two original
    /// ranges
    pub fn concat(self, other: Self) -> Self {
        match (self, other) {
            (Self::Defined(x), Self::Defined(y)) => Self::Defined(x + y),
            (
                Self::Defined(ix @ ax) | Self::Range { min: ix, max: ax },
                Self::Defined(iy @ ay) | Self::Range { min: iy, max: ay },
            ) => {
                let min = ix + iy;
                let max = ax + ay;
                Self::new(min, max)
            }
            (
                Self::Min(x) | Self::Defined(x) | Self::Range { min: x, .. },
                Self::Min(y) | Self::Defined(y) | Self::Range { min: y, .. },
            ) => Self::Min(x + y),
        }
    }
    /// creates a new range that is the greater of the two
    pub(crate) fn greater(self, other: Self) -> Self {
        match (self, other) {
            (Self::Defined(x), Self::Defined(y)) => Self::Defined(x.max(y)),
            (
                Self::Defined(ix @ ax) | Self::Range { min: ix, max: ax },
                Self::Defined(iy @ ay) | Self::Range { min: iy, max: ay },
            ) => {
                let min = ix.max(iy);
                let max = ax.max(ay);
                Self::new(min, max)
            }
            (
                Self::Min(x) | Self::Defined(x) | Self::Range { min: x, .. },
                Self::Min(y) | Self::Defined(y) | Self::Range { min: y, .. },
            ) => Self::Min(x.max(y)),
        }
    }
    /// creates a new range that only include the intersection of the orignal
    /// ranges
    pub(crate) fn intersection(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::Defined(ix @ ax) | Self::Range { min: ix, max: ax },
                Self::Defined(iy @ ay) | Self::Range { min: iy, max: ay },
            ) => {
                let min = ix.min(iy);
                let max = ax.max(ay);
                Self::new(min, max)
            }
            (
                Self::Min(x) | Self::Defined(x) | Self::Range { min: x, .. },
                Self::Min(y) | Self::Defined(y) | Self::Range { min: y, .. },
            ) => Self::Min(x.min(y)),
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
    disassembly_vars: Box<[Rc<Variable>]>,
    blocks: Box<[Block]>,
    pos: Box<[Assertation]>,
}

impl Pattern {
    pub(crate) fn new(
        disassembly_vars: Box<[Rc<Variable>]>,
        blocks: Box<[Block]>,
        pos: Box<[Assertation]>,
        len: PatternLen,
    ) -> Self {
        Self {
            disassembly_vars,
            blocks,
            len,
            pos,
        }
    }
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }
    pub fn disassembly_vars(&self) -> &[Rc<Variable>] {
        &self.disassembly_vars
    }
    pub fn disassembly_pos_match(&self) -> &[Assertation] {
        &self.pos
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
    pub fn flat_pattern_len(&self) -> usize {
        let mut flat_len = 0;
        for block in self.blocks.iter() {
            //stop parsing if the len is not defined
            if let Some(len) = block.len().single_len() {
                flat_len += usize::try_from(len).unwrap() * 8;
            } else {
                flat_len += usize::try_from(block.len().min()).unwrap() * 8;
                break;
            }
        }
        flat_len
    }
    pub fn variants_num(&self) -> usize {
        self.blocks
            .iter()
            .fold(1, |acc, block| acc * block.variants_number())
    }
    pub fn bits_produced(&self) -> usize {
        let len = self.len.single_len().unwrap_or_else(|| self.len.min());
        usize::try_from(len).unwrap() * 8
    }

    pub(crate) fn constraint(
        &self,
        endian: Endian,
        variant_id: usize,
        context: &mut [BitConstraint],
        constraint: &mut [BitConstraint],
    ) -> Option<()> {
        let mut current = constraint;
        for block in self.blocks.iter() {
            block.constraint(endian, variant_id, context, current);
            let next_offset = block.bits_produced();
            current = &mut current[next_offset..];
        }
        Some(())
    }

    pub(crate) fn constraint_single(
        &self,
        endian: Endian,
        context_bytes: usize,
    ) -> Option<Vec<BitConstraint>> {
        let context_bits = context_bytes * 8;
        let mut final_buf = vec![
            BitConstraint::Unrestrained;
            context_bits + self.bits_produced()
        ];
        let self_buf = std::cell::RefCell::new(vec![
            BitConstraint::Unrestrained;
            context_bits
                + self.bits_produced()
        ]);

        let mut variant_gen =
            (0..self.variants_num()).into_iter().filter_map(|var| {
                let mut self_buf = self_buf.borrow_mut();
                self_buf.fill(BitConstraint::Unrestrained);
                let (self_context, self_constraint) =
                    self_buf.split_at_mut(context_bits);
                //ignore impossible variants
                self.constraint(endian, var, self_context, self_constraint)
            });

        let Some(_first) = variant_gen.next() else {
            // all patterns are impossible
            return None;
        };
        final_buf.copy_from_slice(&self_buf.borrow());

        for _ in variant_gen {
            final_buf
                .iter_mut()
                .zip(self_buf.borrow().iter())
                .for_each(|(x, y)| *x = x.least_restrictive(*y));
        }
        Some(final_buf)
    }

    ////7.8.1. Matching
    ////one pattern contains the other if all the cases that match the contained,
    ////also match the pattern.
    ////eg: `a` contains `b` if all cases that match `b` also match `a`. In other
    ////words `a` is a special case of `b`.
    ////NOTE the opose don't need to be true.
    pub(crate) fn ordering(
        &self,
        other: &Self,
        context_bytes: usize,
    ) -> SinglePatternOrdering {
        use BitConstraint::*;
        use SinglePatternOrdering::*;
        let self_pattern_len = self.bits_produced();
        let other_pattern_len = other.bits_produced();
        let max_pattern_len = self_pattern_len.max(other_pattern_len);
        //Endian don't matter here, as long it is consistent
        let self_buf = self.constraint_single(Endian::Little, context_bytes);
        let other_buf = other.constraint_single(Endian::Little, context_bytes);

        let self_extend = max_pattern_len - self_pattern_len;
        let other_extend = max_pattern_len - other_pattern_len;

        self_buf
            .unwrap()
            .into_iter()
            .chain(
                (0..self_extend)
                    .into_iter()
                    .map(|_| BitConstraint::Unrestrained),
            )
            .zip(
                other_buf.unwrap().into_iter().chain(
                    (0..other_extend)
                        .into_iter()
                        .map(|_| BitConstraint::Unrestrained),
                ),
            )
            .map(|(self_bit, other_bit)| match (self_bit, other_bit) {
                (Defined(_) | Restrained, Defined(_) | Restrained)
                | (Unrestrained, Unrestrained) => Eq,
                (Unrestrained, _) => Contained,
                (_, Unrestrained) => Contains,
            })
            .collect()
    }
}

#[derive(Clone, Debug)]
pub enum Block {
    And {
        len: PatternLen,
        token_len: NumberUnsigned,
        token_fields: Box<[ProducedTokenField]>,
        tables: Box<[ProducedTable]>,
        verifications: Box<[Verification]>,
        pre: Box<[Assertation]>,
        pos: Box<[Assertation]>,
        /// zero means it's the first in the chain
        variants_prior: usize,
        /// number of variants this block produce
        variants_number: usize,
    },
    //TODO `OR` block can produce token_fields?
    Or {
        len: PatternLen,
        token_fields: Box<[ProducedTokenField]>,
        tables: Box<[ProducedTable]>,
        branches: Box<[Verification]>,
        pos: Box<[Assertation]>,
        /// zero means it's the first in the chain
        variants_prior: usize,
        /// number of variants this block produce
        variants_number: usize,
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

    fn variants_number(&self) -> usize {
        match self {
            Block::And {
                variants_number, ..
            }
            | Block::Or {
                variants_number, ..
            } => *variants_number,
        }
    }

    fn bits_produced(&self) -> usize {
        let len = self.len().single_len().unwrap_or_else(|| self.len().min());
        usize::try_from(len).unwrap() * 8
    }

    fn constraint(
        &self,
        endian: Endian,
        variant_id: usize,
        context_bits: &mut [BitConstraint],
        constraint_bits: &mut [BitConstraint],
    ) -> Option<()> {
        match self {
            Self::And { verifications, .. } => {
                for verification in verifications.iter() {
                    match verification {
                        Verification::TableBuild { .. } => (),
                        Verification::ContextCheck { context, op, value } => {
                            //TODO update this once multiple varnode context
                            //is implemented
                            super::inner::pattern::apply_value(
                                context_bits,
                                endian,
                                context.element().range.clone(),
                                *op,
                                value,
                            )?
                        }
                        Verification::TokenFieldCheck { field, op, value } => {
                            let field = field.element();
                            let token_len: usize = field
                                .token()
                                .len_bytes()
                                .get()
                                .try_into()
                                .unwrap();
                            super::inner::pattern::apply_value(
                                &mut constraint_bits[0..token_len * 8],
                                field.token.endian,
                                field.range.clone(),
                                *op,
                                value,
                            )?
                        }
                        Verification::SubPattern {
                            location: _,
                            pattern,
                        } => pattern.constraint(
                            endian,
                            variant_id,
                            context_bits,
                            constraint_bits,
                        )?,
                    }
                }
                Some(())
            }
            Self::Or {
                branches,
                variants_prior,
                variants_number,
                ..
            } => {
                //find the correct verification in the OR to constraint
                let mut verification_id =
                    (variant_id / variants_prior) % variants_number;
                for branch in branches.iter() {
                    let verification_variants = branch.variants_number();
                    if verification_id < verification_variants {
                        match branch {
                            Verification::TableBuild { .. } => (),
                            Verification::ContextCheck {
                                context,
                                op,
                                value,
                            } => super::inner::pattern::apply_value(
                                context_bits,
                                endian,
                                context.element().range.clone(),
                                *op,
                                value,
                            )?,
                            Verification::TokenFieldCheck {
                                field,
                                op,
                                value,
                            } => {
                                let field = field.element();
                                let token_len: usize = field
                                    .token()
                                    .len_bytes()
                                    .get()
                                    .try_into()
                                    .unwrap();
                                super::inner::pattern::apply_value(
                                    &mut constraint_bits[0..token_len * 8],
                                    field.token.endian,
                                    field.range.clone(),
                                    *op,
                                    value,
                                )?
                            }
                            Verification::SubPattern { pattern, .. } => pattern
                                .constraint(
                                    endian,
                                    variant_id,
                                    context_bits,
                                    constraint_bits,
                                )?,
                        };
                        return Some(());
                    }
                    verification_id -= verification_variants;
                }
                unreachable!()
            }
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
        location: Span,
        pattern: Pattern,
    },
}
impl Verification {
    pub fn variants_number(&self) -> usize {
        match self {
            Self::SubPattern { pattern, .. } => pattern.variants_num(),
            _ => 1,
        }
    }
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
