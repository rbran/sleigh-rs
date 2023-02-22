use crate::semantic::pcode_macro::PcodeMacro;
use crate::semantic::space::Space;
use crate::semantic::user_function::UserFunction;
use crate::semantic::{GlobalElement, GlobalReference};
use crate::{NumberNonZeroUnsigned, NumberUnsigned};

// function call with variable number on params
#[derive(Copy, Clone, Debug)]
pub enum PrimitiveFunction {
    New,
    CPool,
}

#[derive(Clone, Debug)]
pub enum DefinedFunction {
    UserFunction(GlobalElement<UserFunction>),
    PcodeMacro(GlobalElement<PcodeMacro>),
}

#[derive(Clone, Debug)]
pub enum Function {
    Primitive(PrimitiveFunction),
    Defined(DefinedFunction),
}

#[derive(Clone, Debug)]
pub struct AddrDereference {
    pub space: GlobalReference<Space>,
    pub size: NumberNonZeroUnsigned,
}
impl AddrDereference {
    pub fn output_size(&self) -> NumberNonZeroUnsigned {
        self.size
    }
}

//#[derive(Clone, Copy, Debug)]
//pub struct AddrReference {
//    pub size: NonZeroTypeU,
//}
#[derive(Clone, Copy, Debug)]
pub struct Truncate {
    pub lsb: NumberUnsigned,
    pub size: NumberNonZeroUnsigned,
}

impl Truncate {
    pub fn new(lsb: NumberUnsigned, size: NumberNonZeroUnsigned) -> Self {
        Self { lsb, size }
    }
}

#[derive(Clone, Debug)]
pub enum Unary {
    //NOTE: The ByteRangeMsb, ByteRangeLsb and BitRange from docs are all
    //Truncate
    Truncate(Truncate),
    Dereference(AddrDereference),
    //Reference(AddrReference),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Binary {
    //Binary Arithmetic
    Mult,
    Div,
    SigDiv,
    Rem,
    FloatDiv,
    FloatMult,
    Add,
    Sub,
    FloatAdd,
    FloatSub,
    Lsl,
    Lsr,
    Asr,
    BitAnd,
    BitXor,
    BitOr,
    //Binary Logical
    SigLess,
    SigGreater,
    SigRem,
    SigLessEq,
    SigGreaterEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    FloatLess,
    FloatGreater,
    FloatLessEq,
    FloatGreaterEq,
    And,
    Xor,
    Or,
    Eq,
    Ne,
    FloatEq,
    FloatNe,
    //call functions
    Carry,
    SCarry,
    SBorrow,
}
