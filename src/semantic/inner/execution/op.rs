use crate::semantic::inner::{FieldSize, SolverStatus};
use crate::semantic::space::Space;
use crate::{
    FloatType, NumberNonZeroUnsigned, NumberSigned, NumberUnsigned, Span,
};

use crate::semantic::{execution, GlobalReference};

pub type FinalAddrDereference = execution::AddrDereference;
#[derive(Clone, Debug)]
pub struct AddrDereference {
    pub space: GlobalReference<Space>,
    pub size: FieldSize,
    pub src: Span,
}
impl AddrDereference {
    pub fn new(
        space: GlobalReference<Space>,
        size: FieldSize,
        src: Span,
    ) -> Self {
        Self { space, size, src }
    }
    pub fn output_size(&self) -> &FieldSize {
        &self.size
    }
    pub fn output_size_mut(&mut self) -> &mut FieldSize {
        &mut self.size
    }
    pub fn solve(&self, solved: &mut impl SolverStatus) {
        if !self.size.is_final() {
            solved.iam_not_finished_location(&self.src, file!(), line!())
        }
    }
    pub fn convert(self) -> FinalAddrDereference {
        FinalAddrDereference {
            size: self.size.possible_value().unwrap(),
            space: self.space,
        }
    }
}

pub type FinalTruncate = execution::Truncate;
#[derive(Clone, Debug)]
pub struct Truncate {
    pub lsb: NumberUnsigned,
    pub size: FieldSize,
}

impl Truncate {
    pub fn new(lsb: NumberUnsigned, size: NumberNonZeroUnsigned) -> Self {
        let size = FieldSize::new_bits(size);
        Self { lsb, size }
    }
    pub fn new_lsb(size: NumberNonZeroUnsigned) -> Self {
        let size = FieldSize::new_bytes(size);
        Self { lsb: 0, size }
    }
    pub fn new_msb(lsb: NumberUnsigned) -> Self {
        let size = FieldSize::new_unsized();
        //* 8 because msb is in bytes
        Self { lsb: lsb * 8, size }
    }
    pub fn input_min_bits(&self) -> Option<NumberNonZeroUnsigned> {
        Some(
            NumberNonZeroUnsigned::new(
                self.size.possible_value()?.get() + self.lsb,
            )
            .unwrap(),
        )
    }
    pub fn output_size(&self) -> &FieldSize {
        &self.size
    }
    pub fn output_size_mut(&mut self) -> &mut FieldSize {
        &mut self.size
    }
    pub fn convert(self) -> FinalTruncate {
        FinalTruncate::new(self.lsb, self.size.possible_value().unwrap())
    }
}

pub type FinalUnary = execution::Unary;
#[derive(Clone, Debug)]
pub enum Unary {
    //NOTE: Byte Range Operator is part of the expr::ExprElement::Ambiguous1
    Negation,
    BitNegation,
    Negative,
    FloatNegative,
    Popcount,
    Zext,
    Sext,
    FloatNan,
    FloatAbs,
    FloatSqrt,
    Int2Float,
    Float2Float,
    SignTrunc,
    FloatCeil,
    FloatFloor,
    FloatRound,
}

impl Unary {
    pub fn convert(self) -> FinalUnary {
        match self {
            Self::Negation => FinalUnary::Negation,
            Self::BitNegation => FinalUnary::BitNegation,
            Self::Negative => FinalUnary::Negative,
            Self::FloatNegative => FinalUnary::FloatNegative,
            Self::Popcount => FinalUnary::Popcount,
            Self::Zext => FinalUnary::Zext,
            Self::Sext => FinalUnary::Sext,
            Self::FloatNan => FinalUnary::FloatNan,
            Self::FloatAbs => FinalUnary::FloatAbs,
            Self::FloatSqrt => FinalUnary::FloatSqrt,
            Self::Int2Float => FinalUnary::Int2Float,
            Self::Float2Float => FinalUnary::Float2Float,
            Self::SignTrunc => FinalUnary::SignTrunc,
            Self::FloatCeil => FinalUnary::FloatCeil,
            Self::FloatFloor => FinalUnary::FloatFloor,
            Self::FloatRound => FinalUnary::FloatRound,
        }
    }
}

impl execution::Binary {
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
}
