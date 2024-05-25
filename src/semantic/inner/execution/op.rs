use crate::semantic::execution::{
    Binary, MemoryLocation as FinalMemoryLocation,
};
use crate::semantic::inner::SolverStatus;
use crate::semantic::SpaceId;
use crate::{FloatType, NumberSigned, NumberUnsigned, Span};

use super::FieldSize;

#[derive(Clone, Debug)]
pub struct MemoryLocation {
    pub space: SpaceId,
    pub size: FieldSize,
    pub src: Span,
}
impl MemoryLocation {
    pub fn solve(&self, solved: &mut impl SolverStatus) {
        if !self.size.is_final() {
            solved.iam_not_finished(&self.src, file!(), line!())
        }
    }
    pub fn convert(self) -> FinalMemoryLocation {
        FinalMemoryLocation {
            len_bytes: self.size.possible_value().unwrap(),
            space: self.space,
        }
    }
}

impl Binary {
    #[cfg(feature = "no_overflow_inline")]
    pub fn execute(
        &self,
        left: NumberUnsigned,
        right: NumberUnsigned,
    ) -> Option<NumberUnsigned> {
        //COMPILER please optimize this
        let (left_s, right_s) = (left as NumberSigned, right as NumberSigned);
        let sig = |x: NumberSigned| x as NumberUnsigned;
        let (left_f, right_f) = (left as FloatType, right as FloatType);
        let float = |x: FloatType| x as NumberUnsigned;
        match self {
            Self::Add => left.checked_add(right),
            Self::Sub => left.checked_sub(right),
            Self::Mult => left.checked_mul(right),
            Self::Div => left.checked_div(right),
            Self::SigDiv => left_s.checked_div(right_s).map(sig),
            Self::Rem => Some(left % right),
            Self::SigRem => Some(sig(left_s % right_s)),
            Self::FloatAdd => Some(float(left_f + right_f)),
            Self::FloatSub => Some(float(left_f - right_f)),
            Self::FloatMult => Some(float(left_f * right_f)),
            Self::FloatDiv => Some(float(left_f / right_f)),
            Self::Lsl => left.checked_shl(right.try_into().ok()?),
            Self::Lsr => left.checked_shr(right.try_into().ok()?),
            Self::Asr => left_s.checked_shr(right.try_into().ok()?).map(sig),
            Self::BitAnd => Some(left & right),
            Self::BitXor => Some(left ^ right),
            Self::BitOr => Some(left | right),
            Self::Less => Some((left < right) as NumberUnsigned),
            Self::Greater => Some((left > right) as NumberUnsigned),
            Self::LessEq => Some((left <= right) as NumberUnsigned),
            Self::GreaterEq => Some((left >= right) as NumberUnsigned),
            Self::SigLess => Some((left_s < right_s) as NumberUnsigned),
            Self::SigGreater => Some((left_s > right_s) as NumberUnsigned),
            Self::SigLessEq => Some((left_s <= right_s) as NumberUnsigned),
            Self::SigGreaterEq => Some((left_s >= right_s) as NumberUnsigned),
            Self::FloatLess => Some((left_f < right_f) as NumberUnsigned),
            Self::FloatGreater => Some((left_f > right_f) as NumberUnsigned),
            Self::FloatLessEq => Some((left_f <= right_f) as NumberUnsigned),
            Self::FloatGreaterEq => Some((left_f >= right_f) as NumberUnsigned),
            Self::And => Some((left != 0 && right != 0) as NumberUnsigned),
            Self::Xor => Some(((left != 0) ^ (right != 0)) as NumberUnsigned),
            Self::Or => Some((left != 0 || right != 0) as NumberUnsigned),
            Self::Eq => Some((left == right) as NumberUnsigned),
            Self::Ne => Some((left != right) as NumberUnsigned),
            Self::FloatEq => Some((left_f == right_f) as NumberUnsigned),
            Self::FloatNe => Some((left_f != right_f) as NumberUnsigned),
            //TODO make IntTypeU Ref sized for this reason?
            //carry borrow can only be calculated if the type size is known
            Self::Carry | Self::SCarry | Self::SBorrow => None,
        }
    }
    #[cfg(not(feature = "no_overflow_inline"))]
    pub fn execute(
        &self,
        left: NumberUnsigned,
        right: NumberUnsigned,
    ) -> Option<NumberUnsigned> {
        //COMPILER please optimize this
        let (left_s, right_s) = (left as NumberSigned, right as NumberSigned);
        let sig = |x: NumberSigned| x as NumberUnsigned;
        let (left_f, right_f) = (left as FloatType, right as FloatType);
        let float = |x: FloatType| x as NumberUnsigned;
        match self {
            Self::Add => Some(left.wrapping_add(right)),
            Self::Sub => Some(left.wrapping_sub(right)),
            Self::Mult => left.checked_mul(right),
            Self::Div => left.checked_div(right),
            Self::SigDiv => left_s.checked_div(right_s).map(sig),
            Self::Rem => Some(left % right),
            Self::SigRem => Some(sig(left_s % right_s)),
            Self::FloatAdd => Some(float(left_f + right_f)),
            Self::FloatSub => Some(float(left_f - right_f)),
            Self::FloatMult => Some(float(left_f * right_f)),
            Self::FloatDiv => Some(float(left_f / right_f)),
            Self::Lsl => Some(left.wrapping_shl(right.try_into().ok()?)),
            Self::Lsr => Some(left.wrapping_shr(right.try_into().ok()?)),
            Self::Asr => left_s.checked_shr(right.try_into().ok()?).map(sig),
            Self::BitAnd => Some(left & right),
            Self::BitXor => Some(left ^ right),
            Self::BitOr => Some(left | right),
            Self::Less => Some((left < right) as NumberUnsigned),
            Self::Greater => Some((left > right) as NumberUnsigned),
            Self::LessEq => Some((left <= right) as NumberUnsigned),
            Self::GreaterEq => Some((left >= right) as NumberUnsigned),
            Self::SigLess => Some((left_s < right_s) as NumberUnsigned),
            Self::SigGreater => Some((left_s > right_s) as NumberUnsigned),
            Self::SigLessEq => Some((left_s <= right_s) as NumberUnsigned),
            Self::SigGreaterEq => Some((left_s >= right_s) as NumberUnsigned),
            Self::FloatLess => Some((left_f < right_f) as NumberUnsigned),
            Self::FloatGreater => Some((left_f > right_f) as NumberUnsigned),
            Self::FloatLessEq => Some((left_f <= right_f) as NumberUnsigned),
            Self::FloatGreaterEq => Some((left_f >= right_f) as NumberUnsigned),
            Self::And => Some((left != 0 && right != 0) as NumberUnsigned),
            Self::Xor => Some(((left != 0) ^ (right != 0)) as NumberUnsigned),
            Self::Or => Some((left != 0 || right != 0) as NumberUnsigned),
            Self::Eq => Some((left == right) as NumberUnsigned),
            Self::Ne => Some((left != right) as NumberUnsigned),
            Self::FloatEq => Some((left_f == right_f) as NumberUnsigned),
            Self::FloatNe => Some((left_f != right_f) as NumberUnsigned),
            //TODO make IntTypeU Ref sized for this reason?
            //carry borrow can only be calculated if the type size is known
            Self::Carry | Self::SCarry | Self::SBorrow => None,
        }
    }
}
