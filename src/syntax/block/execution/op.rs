use std::cmp::Ordering;
use std::ops::Range;

use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::sequence::{delimited, preceded, separated_pair, tuple};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::semantic::execution::Binary;
use crate::syntax::parser::{ident, number};
use crate::syntax::BitRange;
use crate::{NumberUnsigned, ParamNumber, SleighError, Span};

#[derive(Clone, Debug)]
pub struct ByteRangeMsb {
    pub value: NumberUnsigned,
    pub src: Span,
}
impl ByteRangeMsb {
    pub fn parse(
        input: &[Token],
    ) -> IResult<&[Token], ByteRangeMsb, SleighError> {
        map(delimited(tag!("("), number, tag!(")")), |(value, src)| {
            Self {
                src: src.clone(),
                value,
            }
        })(input)
    }
}
#[derive(Clone, Debug)]
pub struct ByteRangeLsb {
    pub value: NumberUnsigned,
    pub src: Span,
}
impl ByteRangeLsb {
    pub fn parse(
        input: &[Token],
    ) -> IResult<&[Token], ByteRangeLsb, SleighError> {
        map(preceded(tag!(":"), number), |(value, src)| Self {
            src: src.clone(),
            value,
        })(input)
    }
}

#[derive(Clone, Debug)]
pub enum Unary {
    //NOTE: ByteRangeMsb is part of the expr::ExprElement::Ambiguous1
    ByteRangeMsb(ByteRangeMsb),
    ByteRangeLsb(ByteRangeLsb),
    BitRange(BitRange),
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

impl Unary {
    pub fn precedence(&self, other: &Self) -> Ordering {
        macro_rules! order_enum {
            ($a:ident, $b:ident, $($elem:pat),* $(,)?) => {
                #[allow(unreachable_patterns)]
                match ($a, $b) {
                    $(
                        ($elem, $elem) => std::cmp::Ordering::Equal,
                        ($elem, _) => std::cmp::Ordering::Greater,
                        (_, $elem) => std::cmp::Ordering::Less,
                    )*
                }
            };
        }
        use Unary::*;
        order_enum!(
            self,
            other,
            ByteRangeMsb { .. } | ByteRangeLsb { .. },
            BitRange(_),
            Dereference(_),
            //Reference(_),
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
        )
    }
    pub fn parse_range(
        input: &[Token],
    ) -> IResult<&[Token], Range<NumberUnsigned>, SleighError> {
        map(
            delimited(
                tag!("["),
                separated_pair(number, tag!(","), number),
                tag!("]"),
            ),
            |((lsb, _), (n_bits, _))| lsb..lsb + n_bits,
        )(input)
    }
    pub fn parse_after(
        input: &[Token],
    ) -> IResult<&[Token], (Self, Span), SleighError> {
        alt((
            map(BitRange::parse, |x| {
                let location = x.src.clone();
                (Self::BitRange(x), location)
            }),
            map(ByteRangeMsb::parse, |x| {
                let location = x.src.clone();
                (Self::ByteRangeMsb(x), location)
            }),
            map(ByteRangeLsb::parse, |x| {
                let location = x.src.clone();
                (Self::ByteRangeLsb(x), location)
            }),
        ))(input)
    }
    pub fn parse_before(
        input: &[Token],
    ) -> IResult<&[Token], (Self, Span), SleighError> {
        alt((
            map(tag!("!"), |span| (Self::Negation, span.clone())),
            map(tag!("~"), |span| (Self::BitNegation, span.clone())),
            map(tag!("-"), |span| (Self::Negative, span.clone())),
            map(tag!("f-"), |span| (Self::FloatNegative, span.clone())),
            map(AddrDereference::parse, |x| {
                let location = x.src.clone();
                (Self::Dereference(x), location)
            }),
        ))(input)
    }
    pub fn parse_call_name(
        input_ori: &[Token],
    ) -> IResult<&[Token], (Self, &Span), SleighError> {
        alt((
            map(tag!("popcount"), |span| (Self::Popcount, span)),
            map(tag!("zext"), |span| (Self::Zext, span)),
            map(tag!("sext"), |span| (Self::Sext, span)),
            map(tag!("nan"), |span| (Self::FloatNan, span)),
            map(tag!("abs"), |span| (Self::FloatAbs, span)),
            map(tag!("sqrt"), |span| (Self::FloatSqrt, span)),
            map(tag!("int2float"), |span| (Self::Int2Float, span)),
            map(tag!("float2float"), |span| (Self::Float2Float, span)),
            map(tag!("trunc"), |span| (Self::SignTrunc, span)),
            map(tag!("ceil"), |span| (Self::FloatCeil, span)),
            map(tag!("floor"), |span| (Self::FloatFloor, span)),
            map(tag!("round"), |span| (Self::FloatRound, span)),
        ))(input_ori)
    }
}

macro_rules! op_parser {
    ($name:ident, $op:ident, $tag:tt) => {
        pub fn $name(
            input: &[Token],
        ) -> IResult<&[Token], (Self, &Span), SleighError> {
            map(tag!($tag), |span| (Self::$op, span))(input)
        }
    };
}
//used to define precedence
macro_rules! op_parser_levels {
    ($level:ident, $(($name:ident, $op:ident, $tag:tt)),* $(,)? ) => {
        $(op_parser!($name, $op, $tag);)*
        pub fn $level(input: &[Token]) -> IResult<&[Token], (Self, &Span), SleighError> {
            alt(($(Self::$name),*))(input)
        }
    };
}
impl Binary {
    //NOTE: the bigger operands need to come first otherwise the smallest
    //version will be consumend before the biggest one is checked.
    //eg: "<<" is consument with as "<"
    op_parser_levels!(
        level1,
        (sig_div, SigDiv, "s/"),
        (sig_rem, SigRem, "s%"),
        (float_div, FloatDiv, "f/"),
        (float_mult, FloatMult, "f*"),
        (mult, Mult, "*"),
        (div, Div, "/"),
        (rem, Rem, "%"),
    );

    op_parser_levels!(
        level2,
        (float_add, FloatAdd, "f+"),
        (float_sub, FloatSub, "f-"),
        (add, Add, "+"),
        (sub, Sub, "-"),
    );

    op_parser_levels!(
        level3,
        (asr, Asr, "s>>"),
        (lsl, Lsl, "<<"),
        (lsr, Lsr, ">>"),
    );

    op_parser_levels!(
        level4,
        (float_less_eq, FloatLessEq, "f<="),
        (float_greater_eq, FloatGreaterEq, "f>="),
        (sig_less_eq, SigLessEq, "s<="),
        (sig_greater_eq, SigGreaterEq, "s>="),
        (less_eq, LessEq, "<="),
        (greater_eq, GreaterEq, ">="),
        (float_less, FloatLess, "f<"),
        (float_greater, FloatGreater, "f>"),
        (sig_less, SigLess, "s<"),
        (sig_greater, SigGreater, "s>"),
        (less, Less, "<"),
        (greater, Greater, ">"),
    );

    op_parser_levels!(
        level5,
        (float_eq, FloatEq, "f=="),
        (float_ne, FloatNe, "f!="),
        (eq, Eq, "=="),
        (ne, Ne, "!="),
    );

    op_parser_levels!(
        level6,
        (and, And, "&&"),
        (xor, Xor, "^^"),
        (or, Or, "||"),
        (bit_and, BitAnd, "&"),
        (bit_xor, BitXor, "^"),
        (bit_or, BitOr, "|"),
    );
    pub fn parse_call_name(
        input_ori: &[Token],
    ) -> IResult<&[Token], (Self, &Span), SleighError> {
        alt((
            map(tag!("carry"), |src| (Self::Carry, src)),
            map(tag!("scarry"), |src| (Self::SCarry, src)),
            map(tag!("sborrow"), |src| (Self::SBorrow, src)),
        ))(input_ori)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum PrimitiveFunction {
    New,
    CPool,
}

impl PrimitiveFunction {
    pub fn range_params(&self) -> ParamNumber {
        match self {
            //Self::Popcount
            //| Self::Zext
            //| Self::Sext
            //| Self::FloatNan
            //| Self::FloatAbs
            //| Self::FloatSqrt
            //| Self::Int2Float
            //| Self::Float2Float
            //| Self::SignTrunc
            //| Self::FloatCeil
            //| Self::FloatFloor
            //| Self::FloatRound => ParamNumber::new(1, Some(1)),
            //Self::Carry | Self::SCarry | Self::SBorrow => {
            //    ParamNumber::new(2, Some(2))
            //}
            Self::New => ParamNumber::new(1, Some(2)),
            Self::CPool => ParamNumber::new(3, None),
        }
    }
}

impl PrimitiveFunction {
    pub fn find_call_name(name: &str) -> Option<Self> {
        match name {
            "newobject" => Some(Self::New),
            "cpool" => Some(Self::CPool),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    Primitive(PrimitiveFunction),
    // could be PCode/Macro
    UserDefined(String),
}
impl Function {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        let (input, (ident, _)) = ident(input)?;
        let function =
            if let Some(function) = PrimitiveFunction::find_call_name(&ident) {
                Self::Primitive(function)
            } else {
                Self::UserDefined(ident)
            };
        Ok((input, function))
    }
    pub fn range_params(&self) -> ParamNumber {
        match self {
            Function::Primitive(x) => x.range_params(),
            Function::UserDefined(_) => ParamNumber::new(0, None),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SpaceReference {
    pub src: Span,
    pub name: String,
}
impl SpaceReference {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            delimited(tag!("["), ident, tag!("]")),
            |(name, name_src)| Self {
                name,
                src: name_src.clone(),
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct AddrDereference {
    pub src: Span,
    pub space: Option<SpaceReference>,
    pub size: Option<ByteRangeLsb>,
}

impl AddrDereference {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            tuple((
                tag!("*"),
                opt(SpaceReference::parse),
                opt(ByteRangeLsb::parse),
            )),
            |(src, space, size)| Self {
                src: src.clone(),
                space,
                size,
            },
        )(input)
    }
}
