use crate::pattern::BitConstraint;
use crate::semantic::pattern::{CmpOp, ConstraintValue};
use crate::FieldBits;

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
    constraint: &mut [BitConstraint],
    field_order: fn(usize, usize) -> usize,
    value_order: fn(usize, usize) -> usize,
    field_bits: FieldBits,
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

    let field_bits_len = field_bits.len().get().try_into().unwrap();
    for (value_bit, field_bit) in field_bits.0.enumerate() {
        let field_bit = field_bit.try_into().unwrap();
        let field_bit = field_order(field_bit, constraint.len());
        let bit = &mut constraint[field_bit];
        if let Some(value) = value {
            let value_bit = value_order(value_bit, field_bits_len);
            let value_bit = (value >> value_bit) & 1 != 0;
            *bit = bit.define(value_bit)?;
        } else {
            //error never happen with `BitConstraint::Restrained`
            *bit = bit.most_restrictive(BitConstraint::Restrained).unwrap();
        }
    }
    Some(())
}
