use crate::semantic::pattern::{
    CmpOp, PatternLen, Verification as FinalVerification,
};
use crate::semantic::{ContextId, GlobalScope, TableId, TokenFieldId};
use crate::{syntax, PatternError, Span};

use super::{
    ConstraintValue, ConstructorPatternLen, DisassemblyBuilder, Pattern,
    ProducedTable, Sleigh,
};

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
    pub fn from_constraint(
        sleigh: &Sleigh,
        field: String,
        src: Span,
        constraint: syntax::block::pattern::Constraint,
        this_table: TableId,
    ) -> Result<Self, Box<PatternError>> {
        let syntax::block::pattern::Constraint { op: cmp_op, value } =
            constraint;
        let value = ConstraintValue::new(DisassemblyBuilder::parse_expr(
            sleigh, value.expr,
        )?);
        let field = sleigh
            .get_global(&field)
            .ok_or_else(|| Box::new(PatternError::MissingRef(src.clone())))?;
        match field {
            GlobalScope::TokenField(x) => Ok(Self::TokenFieldCheck {
                field: x,
                op: cmp_op,
                value,
            }),
            //TODO create InstStart? Does start_start exists?
            GlobalScope::Context(x) => Ok(Self::ContextCheck {
                context: x,
                op: cmp_op,
                value,
            }),
            GlobalScope::Table(x) => Ok({
                let verification = Some((cmp_op, value));
                let recursive = x == this_table;
                let table = x;
                Self::TableBuild {
                    produced_table: ProducedTable {
                        table,
                        always: true,
                        recursive,
                        location: src.clone(),
                    },
                    verification,
                }
            }),
            _ => Err(Box::new(PatternError::InvalidRef(src))),
        }
    }
    pub fn new_context(
        context: ContextId,
        _src: Span,
        op: CmpOp,
        value: ConstraintValue,
    ) -> Self {
        Self::ContextCheck { context, op, value }
    }
    pub fn new_table(
        this_table: TableId,
        table: TableId,
        location: Span,
        verification: Option<(CmpOp, ConstraintValue)>,
    ) -> Self {
        let recursive = table == this_table;
        Self::TableBuild {
            produced_table: ProducedTable {
                table,
                always: true,
                recursive,
                location,
            },
            verification,
        }
    }
    pub fn new_token_field(
        field: TokenFieldId,
        _src: Span,
        op: CmpOp,
        value: ConstraintValue,
    ) -> Self {
        Self::TokenFieldCheck { field, op, value }
    }
    pub fn root_len(&self, sleigh: &Sleigh) -> usize {
        match self {
            Verification::ContextCheck { .. }
            | Verification::TableBuild { .. } => 0,
            Verification::TokenFieldCheck {
                field,
                op: _,
                value: _,
            } => {
                let field = sleigh.token_field(*field);
                let token = sleigh.token(field.token);
                let bytes: usize = token.len_bytes.get().try_into().unwrap();
                bytes * 8usize
            }
            Verification::SubPattern {
                location: _,
                pattern,
            } => pattern.root_len(sleigh),
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
                        .flat_map(|block| block.base.tables.values()),
                );
                Some(iter)
            }
        }
    }
    pub fn token_field_check(&self) -> Option<TokenFieldId> {
        match self {
            Self::TokenFieldCheck { field, .. } => Some(*field),
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
    pub fn pattern_len(
        &self,
        sleigh: &Sleigh,
    ) -> Option<ConstructorPatternLen> {
        match self {
            Self::ContextCheck { .. } => {
                Some(ConstructorPatternLen::Basic(PatternLen::Defined(0)))
            }
            Self::TableBuild {
                produced_table:
                    ProducedTable {
                        table,
                        location: _,
                        always: _,
                        recursive: true,
                    },
                verification: _,
            } => match sleigh.table(*table).pattern_len() {
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
                        location: _,
                        always: _,
                        recursive: false,
                    },
                verification: _,
            } => sleigh
                .table(*table)
                .pattern_len()
                .map(ConstructorPatternLen::Basic),
            Self::TokenFieldCheck {
                field,
                op: _,
                value: _,
            } => {
                let field = sleigh.token_field(*field);
                let token = sleigh.token(field.token);
                Some(ConstructorPatternLen::Basic(PatternLen::Defined(
                    token.len_bytes.get(),
                )))
            }
            Self::SubPattern {
                location: _,
                pattern,
            } => pattern.len,
        }
    }
    pub fn variants_number(&self) -> usize {
        match self {
            Self::SubPattern { pattern, .. } => pattern.variants_num(),
            _ => 1,
        }
    }
    pub fn convert(self) -> FinalVerification {
        match self {
            Verification::ContextCheck { context, op, value } => {
                FinalVerification::ContextCheck { context, op, value }
            }
            Verification::TableBuild {
                produced_table,
                verification,
            } => FinalVerification::TableBuild {
                produced_table,
                verification,
            },
            Verification::TokenFieldCheck { field, op, value } => {
                FinalVerification::TokenFieldCheck { field, op, value }
            }
            Verification::SubPattern { location, pattern } => {
                FinalVerification::SubPattern {
                    location,
                    pattern: pattern.convert(),
                }
            }
        }
    }
}
