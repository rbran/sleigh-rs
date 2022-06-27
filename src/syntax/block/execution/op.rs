use std::cmp::Ordering;
use std::ops::Range;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{consumed, map, opt, value};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::IResult;

use crate::base::{empty_space0, ident, number_unsig, IntTypeU};
pub use crate::semantic::execution::Binary;
use crate::syntax::BitRange;
use crate::ParamNumber;

//impl<'a> Op<'a> {
//pub fn precedence(&self, other: &Self) -> Ordering {
//    match (self, other) {
//        (Op::Unary(x), Op::Unary(y)) => x.precedence(y),
//        (Op::Binary(x), Op::Binary(y)) => x.cmp(y),
//        (Op::Var(_), Op::Var(_)) => Ordering::Equal,
//        (Op::Unary(_), _) => Ordering::Greater,
//        (_, Op::Unary(_)) => Ordering::Less,
//        (Op::Binary(_), _) => Ordering::Greater,
//        (_, Op::Binary(_)) => Ordering::Less,
//    }
//}
//}

#[derive(Clone, Copy, Debug)]
pub struct ByteRangeMsb<'a> {
    pub value: IntTypeU,
    pub src: &'a str,
}
impl<'a> ByteRangeMsb<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, ByteRangeMsb<'a>> {
        map(
            consumed(delimited(
                pair(tag("("), empty_space0),
                number_unsig,
                pair(empty_space0, tag(")")),
            )),
            |(src, value)| Self { src, value },
        )(input)
    }
}
#[derive(Clone, Copy, Debug)]
pub struct ByteRangeLsb<'a> {
    pub value: IntTypeU,
    pub src: &'a str,
}
impl<'a> ByteRangeLsb<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, ByteRangeLsb<'a>> {
        map(
            consumed(preceded(pair(tag(":"), empty_space0), number_unsig)),
            |(src, value)| Self { src, value },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub enum Unary<'a> {
    //NOTE: ByteRangeMsb is part of the expr::ExprElement::Ambiguous1
    ByteRangeMsb(ByteRangeMsb<'a>),
    ByteRangeLsb(ByteRangeLsb<'a>),
    BitRange(BitRange<'a>),
    Dereference(AddrDereference<'a>),
    //Reference(AddrReference<'a>),
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

impl<'a> Unary<'a> {
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
    pub fn parse_range(input: &str) -> IResult<&str, Range<IntTypeU>> {
        map(
            delimited(
                pair(tag("["), empty_space0),
                separated_pair(
                    number_unsig,
                    tuple((empty_space0, tag(","), empty_space0)),
                    number_unsig,
                ),
                pair(empty_space0, tag("]")),
            ),
            |(lsb, n_bits)| lsb..lsb + n_bits,
        )(input)
    }
    pub fn parse_after(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(BitRange::parse, Self::BitRange),
            map(ByteRangeMsb::parse, Self::ByteRangeMsb),
            map(ByteRangeLsb::parse, Self::ByteRangeLsb),
        ))(input)
    }
    pub fn parse_before(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            value(Self::Negation, tag("!")),
            value(Self::BitNegation, tag("~")),
            value(Self::Negative, tag("-")),
            value(Self::FloatNegative, tag("f-")),
            //map(AddrReference::parse, Self::Reference),
            map(AddrDereference::parse, Self::Dereference),
        ))(input)
    }
    pub fn parse_call_name(input_ori: &'a str) -> IResult<&'a str, Self> {
        alt((
            value(Self::Popcount, tag("popcount")),
            value(Self::Zext, tag("zext")),
            value(Self::Sext, tag("sext")),
            value(Self::FloatNan, tag("nan")),
            value(Self::FloatAbs, tag("abs")),
            value(Self::FloatSqrt, tag("sqrt")),
            value(Self::Int2Float, tag("int2float")),
            value(Self::Float2Float, tag("float2float")),
            value(Self::SignTrunc, tag("trunc")),
            value(Self::FloatCeil, tag("ceil")),
            value(Self::FloatFloor, tag("floor")),
            value(Self::FloatRound, tag("round")),
        ))(input_ori)
    }
}

macro_rules! op_parser {
    ($name:ident, $op:ident, $tag:literal) => {
        pub fn $name(input: &str) -> IResult<&str, Self> {
            value(Self::$op, tag($tag))(input)
        }
    };
}
//used to define precedence
macro_rules! op_parser_levels {
    ($level:ident, $(($name:ident, $op:ident, $tag:literal)),* $(,)? ) => {
        $(op_parser!($name, $op, $tag);)*
        pub fn $level(input: &str) -> IResult<&str, Self> {
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
    pub fn parse_call_name(input_ori: &str) -> IResult<&str, Self> {
        alt((
            value(Self::Carry, tag("carry")),
            value(Self::SCarry, tag("scarry")),
            value(Self::SBorrow, tag("sborrow")),
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
pub enum Function<'a> {
    Primitive(PrimitiveFunction),
    // could be PCode/Macro
    UserDefined(&'a str),
}
impl<'a> Function<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        let (input, ident) = ident(input)?;
        let function =
            if let Some(function) = PrimitiveFunction::find_call_name(ident) {
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
pub struct SpaceReference<'a> {
    pub name: &'a str,
}
impl<'a> SpaceReference<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            delimited(
                tuple((empty_space0, tag("["), empty_space0)),
                ident,
                tuple((empty_space0, tag("]"), empty_space0)),
            ),
            |name| Self { name },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct AddrDereference<'a> {
    pub src: &'a str,
    pub space: Option<SpaceReference<'a>>,
    pub size: Option<ByteRangeLsb<'a>>,
}

impl<'a> AddrDereference<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(preceded(
                tag("*"),
                pair(opt(SpaceReference::parse), opt(ByteRangeLsb::parse)),
            )),
            |(src, (space, size))| Self { src, space, size },
        )(input)
    }
}
