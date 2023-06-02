use crate::disassembly::VariableId;
use crate::semantic::{
    ContextId, Sleigh as FinalSleigh, TableId, TokenFieldId,
};
use crate::{
    field_in_be, field_in_le, value_in_context, value_in_token, NumberUnsigned,
    Span,
};

use super::disassembly::{Assertation, Expr, Variable};
use super::InstStart;

/// Represent how a bit is limited in a pattern
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum BitConstraint {
    //can have any value
    #[default]
    Unrestrained,
    //only one value possible 0->false, 1->true
    Defined(bool),
    //the value is limited depending on other bits.
    Restrained,
}

impl BitConstraint {
    pub fn value(&self) -> Option<bool> {
        match self {
            Self::Defined(value) => Some(*value),
            _ => None,
        }
    }
    pub fn restrained(&self) -> bool {
        match self {
            BitConstraint::Unrestrained => false,
            BitConstraint::Defined(_) | BitConstraint::Restrained => true,
        }
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
    pub table: TableId,
    pub location: Span,
    pub always: bool,
    pub recursive: bool,
}

#[derive(Clone, Debug)]
pub struct ProducedTokenField {
    pub field: TokenFieldId,
    //if this field is produced explicitly (on pattern) or implicitly deduced
    //the existence of by the use of it in a desassembly/execution
    pub source: Option<Span>,
    pub local: bool,
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub len: PatternLen,
    pub disassembly_vars: Box<[Variable]>,
    pub blocks: Box<[Block]>,
    pub pos: Box<[Assertation]>,
}

impl Pattern {
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }

    pub fn disassembly_var(&self, id: VariableId) -> &Variable {
        &self.disassembly_vars[id.0]
    }

    pub fn disassembly_vars(&self) -> &[Variable] {
        &self.disassembly_vars
    }

    pub fn disassembly_pos_match(&self) -> &[Assertation] {
        &self.pos
    }

    pub fn produced_tables(&self) -> impl Iterator<Item = &ProducedTable> {
        self.blocks().iter().flat_map(Block::tables)
    }

    pub fn produced_token_fields(
        &self,
    ) -> impl Iterator<Item = &ProducedTokenField> {
        self.blocks()
            .iter()
            .flat_map(|block| block.token_fields().iter())
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
        sleigh: &FinalSleigh,
        variant_id: usize,
        context: &mut [BitConstraint],
        constraint: &mut [BitConstraint],
    ) -> Option<()> {
        let mut current = constraint;
        for block in self.blocks.iter() {
            block.constraint(sleigh, variant_id, context, current);
            let next_offset = block.bits_produced();
            current = &mut current[next_offset..];
        }
        Some(())
    }

    /// the bit pattern for all variants.
    pub(crate) fn pattern_bits_variants<'a>(
        &'a self,
        sleigh: &'a FinalSleigh,
    ) -> impl Iterator<Item = (usize, (Vec<BitConstraint>, Vec<BitConstraint>))> + 'a
    {
        let context_bits =
            usize::try_from(sleigh.context_memory.memory_bits).unwrap();
        let pattern_bits = self.bits_produced();
        let mut context_buf = vec![BitConstraint::default(); context_bits];
        let mut pattern_buf = vec![BitConstraint::default(); pattern_bits];
        (0..self.variants_num()).into_iter().filter_map(move |i| {
            context_buf.fill(BitConstraint::default());
            pattern_buf.fill(BitConstraint::default());
            self.constraint(sleigh, i, &mut context_buf, &mut pattern_buf)?;
            Some((i, (context_buf.clone(), pattern_buf.clone())))
        })
    }
}

#[derive(Clone, Debug)]
pub enum Block {
    And {
        len: PatternLen,
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
        sleigh: &FinalSleigh,
        variant_id: usize,
        context_bits: &mut [BitConstraint],
        constraint_bits: &mut [BitConstraint],
    ) -> Option<()> {
        match self {
            Self::And { verifications, .. } => {
                for verification in verifications.iter() {
                    apply_verification(
                        sleigh,
                        variant_id,
                        verification,
                        context_bits,
                        constraint_bits,
                    )?;
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
                        return apply_verification(
                            sleigh,
                            variant_id,
                            branch,
                            context_bits,
                            constraint_bits,
                        );
                    }
                    verification_id -= verification_variants;
                }
                unreachable!()
            }
        }
    }
}

fn apply_verification(
    sleigh: &FinalSleigh,
    variant_id: usize,
    verification: &Verification,
    context_bits: &mut [BitConstraint],
    constraint_bits: &mut [BitConstraint],
) -> Option<()> {
    match verification {
        Verification::TableBuild { .. } => Some(()),
        Verification::ContextCheck { context, op, value } => {
            let bits = sleigh.context_memory.context(*context);
            super::inner::pattern::apply_value(
                context_bits,
                field_in_le,
                value_in_context,
                bits,
                *op,
                value,
            )
        }
        Verification::TokenFieldCheck { field, op, value } => {
            let token_field = sleigh.token_field(*field);
            let token = sleigh.token(token_field.token);
            let token_len: usize = token.len_bytes.get().try_into().unwrap();
            let bit_order = match token.endian {
                crate::Endian::Little => field_in_le,
                crate::Endian::Big => field_in_be,
            };
            super::inner::pattern::apply_value(
                &mut constraint_bits[0..token_len * 8],
                bit_order,
                value_in_token,
                token_field.bits.clone(),
                *op,
                value,
            )
        }
        Verification::SubPattern { pattern, .. } => pattern.constraint(
            sleigh,
            variant_id,
            context_bits,
            constraint_bits,
        ),
    }
}

#[derive(Clone, Debug)]
pub enum Verification {
    ContextCheck {
        context: ContextId,
        op: CmpOp,
        value: ConstraintValue,
    },
    TableBuild {
        produced_table: ProducedTable,
        verification: Option<(CmpOp, ConstraintValue)>,
    },
    TokenFieldCheck {
        field: TokenFieldId,
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
    TokenField(TokenFieldId),
    Context(ContextId),
    InstStart(InstStart),
    Table(TableId),
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
