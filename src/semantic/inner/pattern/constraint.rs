use crate::bit_in_field;
use crate::pattern::{CmpOp, ConstraintValue};
use crate::{BitRange, Endian};

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
    endian: Endian,
    value_bits: BitRange,
    op: CmpOp,
    value: &ConstraintValue,
) -> Option<()> {
    //only set the value, if its Eq and the value is known at compile time
    use crate::semantic::disassembly::{ExprElement, ReadScope};
    let value = match (op, value.expr().elements()) {
        (CmpOp::Eq, [ExprElement::Value(ReadScope::Integer(value))]) => {
            Some(value.signed_super())
        }
        _ => None,
    };

    for (input_value_bit, field_bit) in value_bits.0.into_iter().enumerate() {
        let field_bits: u32 = field.len().try_into().unwrap();
        let field_bit =
            bit_in_field(field_bit.try_into().unwrap(), endian, field_bits);
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
    use crate::Number;

    #[test]
    fn test_apply_value() {
        let mut field = vec![super::BitConstraint::default(); 64];
        super::apply_value(
            &mut field,
            crate::Endian::Big,
            crate::BitRange(35..37),
            crate::pattern::CmpOp::Eq,
            &crate::pattern::ConstraintValue::new(
                crate::disassembly::Expr::new(Box::from([
                    crate::disassembly::ExprElement::Value(
                        crate::disassembly::ReadScope::Integer(
                            Number::Positive(2),
                        ),
                    ),
                ])),
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
