use crate::pattern::BitConstraint;
use crate::semantic::pattern::{CmpOp, ConstraintValue};
use crate::FieldBits;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SinglePatternOrdering {
    Eq,
    Conflict,
    Contains,
    Contained,
}

impl SinglePatternOrdering {
    pub fn combine(self, other: Self) -> Self {
        use SinglePatternOrdering::*;
        match (self, other) {
            (Conflict, _) | (_, Conflict) => Conflict,
            (Eq, other) | (other, Eq) => other,
            (Contains, Contains) | (Contained, Contained) => self,
            (Contains, Contained) | (Contained, Contains) => Conflict,
        }
    }
}
impl std::iter::FromIterator<SinglePatternOrdering> for SinglePatternOrdering {
    fn from_iter<T: IntoIterator<Item = SinglePatternOrdering>>(
        iter: T,
    ) -> Self {
        use SinglePatternOrdering::*;
        let mut acc = Eq;
        for i in iter {
            acc = acc.combine(i);
            if acc == Conflict {
                return Conflict;
            }
        }
        acc
    }
}

impl BitConstraint {
    pub fn define(self, bit: bool) -> Option<Self> {
        match self {
            Self::Unrestrained => Some(Self::Defined(bit)),
            Self::Defined(old_bit) if old_bit == bit => Some(self),
            Self::Defined(_old_bit) => None,
            // TODO this may not be possible, we are unable to verify that now
            Self::Restrained => Some(Self::Defined(bit)),
        }
    }
    /// select the most restrictive from both, None if they conflict
    pub fn most_restrictive(self, other: Self) -> Option<Self> {
        match (self, other) {
            //if one is unrestrained, just return the other
            (Self::Unrestrained, other) | (other, Self::Unrestrained) => {
                Some(other)
            }
            // both have the same value
            (Self::Restrained, Self::Restrained) => Some(self),
            (Self::Defined(self_value), Self::Defined(other_value))
                if self_value == other_value =>
            {
                Some(self)
            }
            // conflicting values
            (Self::Defined(_), Self::Defined(_)) => None,
            // TODO this may not be possible, we are unable to verify that now
            (other @ Self::Defined(_), Self::Restrained)
            | (Self::Restrained, other @ Self::Defined(_)) => Some(other),
        }
    }
    /// select the least restrictive from both
    pub fn least_restrictive(self, other: Self) -> Self {
        match (self, other) {
            (Self::Unrestrained, _other) | (_other, Self::Unrestrained) => {
                Self::Unrestrained
            }
            (Self::Defined(self_value), Self::Defined(other_value))
                if self_value != other_value =>
            {
                Self::Restrained
            }
            //both have the same value
            (Self::Defined(_), Self::Defined(_)) => self,
            (Self::Restrained, Self::Restrained)
            | (Self::Defined(_), Self::Restrained)
            | (Self::Restrained, Self::Defined(_)) => Self::Restrained,
        }
    }
}

pub fn apply_value(
    field: &mut [BitConstraint],
    bit_order: fn(u32, u32) -> u32,
    value_bits: FieldBits,
    op: CmpOp,
    value: &ConstraintValue,
) -> Option<()> {
    //only set the value, if its Eq and the value is known at compile time
    use crate::semantic::disassembly::{ExprElement, ReadScope};
    let value = match (op, value.expr().elements()) {
        (
            CmpOp::Eq,
            [ExprElement::Value {
                value: ReadScope::Integer(value),
                location: _,
            }],
        ) => Some(value.signed_super()),
        _ => None,
    };

    for (input_value_bit, field_bit) in value_bits.0.into_iter().enumerate() {
        let field_bits: u32 = field.len().try_into().unwrap();
        let field_bit = bit_order(field_bit.try_into().unwrap(), field_bits);
        let bit = &mut field[field_bit as usize];
        if let Some(value) = value {
            *bit = bit.define((value >> input_value_bit) & 1 != 0)?;
        } else {
            //error never happen with `BitConstraint::Restrained`
            *bit = bit.most_restrictive(BitConstraint::Restrained).unwrap();
        }
    }
    Some(())
}

#[cfg(test)]
mod test {
    #[test]
    fn test_apply_value() {
        let mut field = vec![super::BitConstraint::default(); 64];
        let dummy_location = crate::Span::File(crate::FileSpan {
            start: crate::FileLocation {
                file: std::rc::Rc::from(std::path::Path::new("")),
                line: 0,
                column: 0,
            },
            end_line: 0,
            end_column: 0,
        });
        super::apply_value(
            &mut field,
            crate::Endian::Big,
            crate::FieldBits(35..37),
            crate::semantic::pattern::CmpOp::Eq,
            &crate::semantic::pattern::ConstraintValue::new(
                crate::semantic::disassembly::Expr {
                    rpn: Box::from([
                        crate::semantic::disassembly::ExprElement::Value{
                            value: crate::semantic::disassembly::ReadScope::Integer(
                                crate::Number::Positive(2),
                            ),
                            location: dummy_location,
                        },
                    ]),
                },
            ),
        )
        .unwrap();
        for (i, bit) in field.iter().enumerate() {
            dbg!(&i);
            dbg!(&bit);
        }
        todo!();
    }
}
