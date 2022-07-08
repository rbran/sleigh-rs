use std::rc::Rc;

use execution::ExecutionError;

use crate::base::{FloatType, IntTypeS, IntTypeU, NonZeroTypeU};
use crate::semantic::inner::{FieldSize, SolverStatus};
use crate::semantic::space::Space;
use crate::InputSource;

use crate::semantic::execution;

pub type FinalAddrDereference = execution::AddrDereference;
#[derive(Clone, Debug)]
pub struct AddrDereference {
    pub space: Rc<Space>,
    pub size: FieldSize,
    pub src: InputSource,
}
impl AddrDereference {
    pub fn new(space: Rc<Space>, size: FieldSize, src: InputSource) -> Self {
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
            solved.iam_not_finished_location(&self.src)
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
    pub lsb: IntTypeU,
    pub size: FieldSize,
}

impl Truncate {
    pub fn new(lsb: IntTypeU, size: NonZeroTypeU) -> Self {
        let size = FieldSize::new_bits(size);
        Self { lsb, size }
    }
    pub fn new_lsb(size: NonZeroTypeU) -> Self {
        let size = FieldSize::new_bytes(size);
        Self { lsb: 0, size }
    }
    pub fn new_msb(lsb: IntTypeU) -> Self {
        let size = FieldSize::new_unsized();
        //* 8 because msb is in bytes
        Self { lsb: lsb * 8, size }
    }
    pub fn input_min_bits(&self) -> Option<NonZeroTypeU> {
        Some(
            NonZeroTypeU::new(self.size.possible_value()?.get() + self.lsb)
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
    pub fn execute(&self, left: IntTypeU, right: IntTypeU) -> Option<IntTypeU> {
        //COMPILER please optimize this
        let (left_s, right_s) = (left as IntTypeS, right as IntTypeS);
        let sig = |x: IntTypeS| x as IntTypeU;
        let (left_f, right_f) = (left as FloatType, right as FloatType);
        let float = |x: FloatType| x as IntTypeU;
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
            Self::Less => Some((left < right) as IntTypeU),
            Self::Greater => Some((left > right) as IntTypeU),
            Self::LessEq => Some((left <= right) as IntTypeU),
            Self::GreaterEq => Some((left >= right) as IntTypeU),
            Self::SigLess => Some((left_s < right_s) as IntTypeU),
            Self::SigGreater => Some((left_s > right_s) as IntTypeU),
            Self::SigLessEq => Some((left_s <= right_s) as IntTypeU),
            Self::SigGreaterEq => Some((left_s >= right_s) as IntTypeU),
            Self::FloatLess => Some((left_f < right_f) as IntTypeU),
            Self::FloatGreater => Some((left_f > right_f) as IntTypeU),
            Self::FloatLessEq => Some((left_f <= right_f) as IntTypeU),
            Self::FloatGreaterEq => Some((left_f >= right_f) as IntTypeU),
            Self::And => Some((left != 0 && right != 0) as IntTypeU),
            Self::Xor => Some(((left != 0) ^ (right != 0)) as IntTypeU),
            Self::Or => Some((left != 0 || right != 0) as IntTypeU),
            Self::Eq => Some((left == right) as IntTypeU),
            Self::Ne => Some((left != right) as IntTypeU),
            Self::FloatEq => Some((left_f == right_f) as IntTypeU),
            Self::FloatNe => Some((left_f != right_f) as IntTypeU),
            //TODO make IntTypeU Ref sized for this reason?
            //carry borrow can only be calculated if the type size is known
            Self::Carry | Self::SCarry | Self::SBorrow => None,
        }
    }
}
