use crate::pattern::CmpOp;
use crate::semantic::inner::disassembly::{Expr, ExprElement, ReadScope};
use crate::semantic::inner::table::Table;
use crate::semantic::inner::token::TokenField;
use crate::semantic::inner::varnode::Context;
use crate::semantic::inner::{GlobalScope, Sleigh};
use crate::semantic::pattern::{PatternError, PatternLen};
use crate::semantic::GlobalReference;
use crate::syntax;
use crate::{GlobalElement, Span};

use super::constraint::{BitConstraint, BlockConstraint};
use super::{ConstraintValue, ConstructorPatternLen, Pattern, ProducedTable};

pub type FinalVerification = crate::semantic::pattern::Verification;
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
    pub fn from_constraint(
        sleigh: &Sleigh,
        field: &str,
        src: &Span,
        constraint: syntax::block::pattern::Constraint,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        let syntax::block::pattern::Constraint { op: cmp_op, value } =
            constraint;
        let value = ConstraintValue::new(sleigh, value)?;
        let field = sleigh
            .get_global(field)
            .ok_or(PatternError::MissingRef(src.clone()))?;
        match field {
            GlobalScope::TokenField(x) => Ok(Self::TokenFieldCheck {
                field: x.reference_from(src.clone()),
                op: cmp_op,
                value,
            }),
            //TODO create InstStart? Does start_start exists?
            GlobalScope::Context(x) => Ok(Self::ContextCheck {
                context: x.reference_from(src.clone()),
                op: cmp_op,
                value,
            }),
            GlobalScope::Table(x) => Ok({
                let verification = Some((cmp_op, value));
                let recursive = x.element_ptr() == this_table;
                let table = x.reference_from(src.clone());
                Self::TableBuild {
                    produced_table: ProducedTable {
                        table,
                        always: true,
                        recursive,
                    },
                    verification,
                }
            }),
            _ => return Err(PatternError::InvalidRef(src.clone())),
        }
    }
    pub fn new_context(
        context: &GlobalElement<Context>,
        src: Span,
        op: CmpOp,
        value: ConstraintValue,
    ) -> Self {
        Self::ContextCheck {
            context: context.reference_from(src),
            op,
            value,
        }
    }
    pub fn new_table(
        this_table: *const Table,
        table: &GlobalElement<Table>,
        src: Span,
        verification: Option<(CmpOp, ConstraintValue)>,
    ) -> Self {
        let recursive = table.element_ptr() == this_table;
        let table = table.reference_from(src);
        Self::TableBuild {
            produced_table: ProducedTable {
                table,
                always: true,
                recursive,
            },
            verification,
        }
    }
    pub fn new_token_field(
        field: &GlobalElement<TokenField>,
        src: Span,
        op: CmpOp,
        value: ConstraintValue,
    ) -> Self {
        Self::TokenFieldCheck {
            field: field.reference_from(src),
            op,
            value,
        }
    }
    pub fn root_len(&self) -> usize {
        match self {
            Verification::ContextCheck { .. }
            | Verification::TableBuild { .. } => 0,
            Verification::TokenFieldCheck {
                field,
                op: _,
                value: _,
            } => {
                let bytes: usize =
                    field.element().token.len_bytes().get().try_into().unwrap();
                bytes * 8usize
            }
            Verification::SubPattern {
                location: _,
                pattern,
            } => pattern.root_len(),
        }
    }
    pub fn tables<'a>(
        &'a self,
    ) -> Option<impl Iterator<Item = &'a ProducedTable> + 'a> {
        match self {
            Self::TokenFieldCheck { .. } | Self::ContextCheck { .. } => None,
            Self::TableBuild {
                produced_table,
                verification: _,
            } => {
                let iter: Box<dyn Iterator<Item = &'a _>> =
                    Box::new([produced_table].into_iter());
                Some(iter)
            }
            Self::SubPattern {
                location: _,
                pattern,
            } => {
                let iter: Box<dyn Iterator<Item = &'a _>> = Box::new(
                    pattern
                        .blocks
                        .iter()
                        .map(|block| block.tables.values())
                        .flatten(),
                );
                Some(iter)
            }
        }
    }
    pub fn token_field_check(&self) -> Option<&GlobalReference<TokenField>> {
        match self {
            Self::TokenFieldCheck { field, .. } => Some(field),
            Self::ContextCheck { .. }
            | Self::TableBuild { .. }
            | Self::SubPattern { .. } => None,
        }
    }
    pub fn sub_pattern(&self) -> Option<&Pattern> {
        match self {
            Self::TokenFieldCheck { .. }
            | Self::ContextCheck { .. }
            | Self::TableBuild { .. } => None,
            Self::SubPattern {
                location: _,
                pattern,
            } => Some(pattern),
        }
    }
    pub fn sub_pattern_mut(&mut self) -> Option<&mut Pattern> {
        match self {
            Self::TokenFieldCheck { .. }
            | Self::ContextCheck { .. }
            | Self::TableBuild { .. } => None,
            Self::SubPattern {
                location: _,
                pattern,
            } => Some(pattern),
        }
    }
    pub fn pattern_len(&self) -> Option<ConstructorPatternLen> {
        match self {
            Self::ContextCheck { .. } => {
                Some(ConstructorPatternLen::Basic(PatternLen::Defined(0)))
            }
            Self::TableBuild {
                produced_table:
                    ProducedTable {
                        table,
                        always: _,
                        recursive: true,
                    },
                verification: _,
            } => match table.element().pattern_len() {
                //if the table len is known, return it
                Some(table_len) => Some(table_len.into()),
                //otherwise the indication that this is a recursive
                None => Some(ConstructorPatternLen::NonGrowingRecursive(
                    PatternLen::Defined(0),
                )),
            },
            Self::TableBuild {
                produced_table:
                    ProducedTable {
                        table,
                        always: _,
                        recursive: false,
                    },
                verification: _,
            } => table
                .element()
                .pattern_len()
                .map(ConstructorPatternLen::Basic),
            Self::TokenFieldCheck {
                field,
                op: _,
                value: _,
            } => Some(ConstructorPatternLen::Basic(PatternLen::Defined(
                field.element().token.len_bytes().get(),
            ))),
            Self::SubPattern {
                location: _,
                pattern,
            } => pattern.len(),
        }
    }
    fn constraint_bits_field(
        &self,
        constraint: &mut [BitConstraint],
        field: &GlobalReference<TokenField>,
        op: &CmpOp,
        value: &ConstraintValue,
    ) {
        let field = field.element();
        let field_range = field.element().range();
        let range = field_range.0.start as usize..field_range.0.end as usize;
        let bits = constraint[range].iter_mut();

        let ConstraintValue { expr: Expr { rpn } } = value;
        match (op, rpn.first()) {
            (
                CmpOp::Eq,
                Some(ExprElement::Value(ReadScope::Integer(value))),
            ) => {
                let value_bits = bits
                    .enumerate()
                    .map(|(i, b)| (b, value.signed_super() & (1 << i) != 0));
                for (bit, value_bit) in value_bits {
                    //TODO create error here for this
                    *bit = bit.define(value_bit);
                }
            }
            (CmpOp::Eq | CmpOp::Ne, _) => {
                for bit in bits {
                    //error never happen with `BitConstraint::Restrained`
                    *bit = bit.most_restrictive(BitConstraint::Restrained)
                }
            }
            (_, _) => (),
        }
    }
    pub fn constraint(&self, constraint: &mut BlockConstraint, offset: usize) {
        match self {
            //NOTE this combine any sub-pattern
            Self::SubPattern {
                location: _,
                pattern,
            } => pattern.sub_pattern_constraint(constraint, offset),

            Self::TokenFieldCheck { field, op, value } => {
                self.constraint_bits_field(
                    &mut constraint.base[offset..],
                    field,
                    op,
                    value,
                );
            }
            Self::ContextCheck { .. } | Self::TableBuild { .. } => (),
        }
    }
    pub fn constraint_variant(&self, constraint: &mut [BitConstraint]) {
        match self {
            //NOTE this combine any sub-pattern
            Self::SubPattern {
                location: _,
                pattern,
            } => {
                pattern.sub_pattern_constraint_variant(constraint);
            }

            Self::TokenFieldCheck { field, op, value } => {
                self.constraint_bits_field(constraint, field, op, value);
            }
            Self::ContextCheck { .. } | Self::TableBuild { .. } => (),
        }
    }
}

impl From<Verification> for FinalVerification {
    fn from(value: Verification) -> Self {
        match value {
            Verification::ContextCheck { context, op, value } => {
                Self::ContextCheck {
                    context: context.convert_reference(),
                    op,
                    value: value.into(),
                }
            }
            Verification::TableBuild {
                produced_table,
                verification,
            } => Self::TableBuild {
                produced_table: produced_table.into(),
                verification: verification
                    .map(|(op, value)| (op, value.into())),
            },
            Verification::TokenFieldCheck { field, op, value } => {
                Self::TokenFieldCheck {
                    field: field.convert_reference(),
                    op,
                    value: value.into(),
                }
            }
            Verification::SubPattern { location, pattern } => {
                Self::SubPattern {
                    location,
                    pattern: pattern.convert(),
                }
            }
        }
    }
}
