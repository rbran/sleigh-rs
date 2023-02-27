use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::combinator::value;
use nom::IResult;

use crate::preprocessor::parser::ident;
use crate::NumberUnsigned;

use super::parser::{number, string};
use super::Span;

#[derive(Debug, Clone)]
pub struct Token {
    pub location: Span,
    pub token_type: TokenType,
}
impl Token {
    pub(crate) fn new(location: Span, token_type: TokenType) -> Self {
        Self {
            location,
            token_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    OpNotequal,
    OpBoolAnd,
    OpLeft,
    OpLessequal,
    OpEqual,
    OpGreatequal,
    OpRight,
    OpBoolXor,
    OpBoolOr,
    OpAbs,
    OpBorrow,
    CallKey,
    OpCarry,
    OpCeil,
    OpFnotequal,
    OpFmult,
    OpFadd,
    OpFsub,
    OpFdiv,
    OpFless,
    OpFlessequal,
    OpFequal,
    OpFgreat,
    OpFgreatequal,
    OpFloat2float,
    OpFloatSqrt,
    OpFloor,
    GotoKey,
    IfKey,
    OpInt2float,
    LocalKey,
    OpNan,
    ReturnKey,
    OpPopcount,
    OpRound,
    OpSrem,
    OpSdiv,
    OpSless,
    OpSlessequal,
    OpSgreat,
    OpSgreatequal,
    OpSright,
    OpSborrow,
    OpScarry,
    OpSext,
    OpTrunc,
    OpZext,
    OpGreat,
    OpLess,
    OpXor,
    OpAnd,
    OpOr,
    OpNeg,
    OpBitNeg,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpRem,
    OpAssign,
    DeliOpenParenteses,
    DeliCloseParenteses,
    DeliOpenBrackets,
    DeliCloseBrackets,
    DeliOpenCurly,
    DeliCloseCurly,
    Comma,
    DubleDot,
    StatementEnd,
    Ellipsis,
    Unimpl,
    SpaceConst,
    SpaceUnique,
    Underline,
    Number(NumberUnsigned),
    String(String),
    Ident(String),
}
impl TokenType {
    pub fn parse(data: &str) -> IResult<&str, Self> {
        //NOTE order is important, otherwise a `<<` could be consumed as two `<`
        //NOTE at the time of writing alt only allow tuples with 21 elements
        alt((
            map(number, Self::Number),
            // logic op for disassembly
            alt((
                value(Self::OpBoolAnd, tag("$and")),
                value(Self::OpBoolXor, tag("$xor")),
                value(Self::OpBoolOr, tag("$or")),
            )),
            // float operations
            alt((
                //3 chars
                value(Self::OpFnotequal, tag("f!=")),
                value(Self::OpFlessequal, tag("f<=")),
                value(Self::OpFequal, tag("f==")),
                value(Self::OpFgreatequal, tag("f>=")),
                //2 chars
                value(Self::OpFmult, tag("f*")),
                value(Self::OpFadd, tag("f+")),
                value(Self::OpFsub, tag("f-")),
                value(Self::OpFdiv, tag("f/")),
                value(Self::OpFless, tag("f<")),
                value(Self::OpFgreat, tag("f>")),
            )),
            // signed operations
            alt((
                //3 chars
                value(Self::OpSlessequal, tag("s<=")),
                value(Self::OpSgreatequal, tag("s>=")),
                value(Self::OpSright, tag("s>>")),
                //2 chars
                value(Self::OpSrem, tag("s%")),
                value(Self::OpSdiv, tag("s/")),
                value(Self::OpSless, tag("s<")),
                value(Self::OpSgreat, tag("s>")),
            )),
            // other operations, 2 chars
            alt((
                //2 chars
                value(Self::OpNotequal, tag("!=")),
                value(Self::OpLeft, tag("<<")),
                value(Self::OpLessequal, tag("<=")),
                value(Self::OpEqual, tag("==")),
                value(Self::OpGreatequal, tag(">=")),
                value(Self::OpRight, tag(">>")),
                value(Self::OpBoolAnd, tag("&&")),
                value(Self::OpBoolXor, tag("^^")),
                value(Self::OpBoolOr, tag("||")),
            )),
            // other operations, 1 char
            alt((
                value(Self::OpGreat, tag(">")),
                value(Self::OpLess, tag("<")),
                value(Self::OpXor, tag("^")),
                value(Self::OpAnd, tag("&")),
                value(Self::OpOr, tag("|")),
                value(Self::OpNeg, tag("~")),
                value(Self::OpBitNeg, tag("!")),
                value(Self::OpAdd, tag("+")),
                value(Self::OpSub, tag("-")),
                value(Self::OpMul, tag("*")),
                value(Self::OpDiv, tag("/")),
                value(Self::OpRem, tag("%")),
                value(Self::OpAssign, tag("=")),
            )),
            //other other
            alt((
                value(Self::DeliOpenParenteses, tag("(")),
                value(Self::DeliCloseParenteses, tag(")")),
                value(Self::DeliOpenBrackets, tag("[")),
                value(Self::DeliCloseBrackets, tag("]")),
                value(Self::DeliOpenCurly, tag("{")),
                value(Self::DeliCloseCurly, tag("}")),
                value(Self::Comma, tag(",")),
                value(Self::DubleDot, tag(":")),
                value(Self::StatementEnd, tag(";")),
                value(Self::Ellipsis, tag("...")),
            )),
            map(string, Self::String),
            Self::parse_words,
        ))(data)
    }
    fn parse_words(input: &str) -> IResult<&str, Self> {
        use TokenType::*;
        let (rest, name) = alt((ident, tag("_")))(input)?;
        #[rustfmt::skip]
        let token = match name {
            "_"           => Underline,
            //reserved keywords
            "if"          => IfKey,
            "abs"         => OpAbs,
            "nan"         => OpNan,
            "call"        => CallKey,
            "ceil"        => OpCeil,
            "goto"        => GotoKey,
            "sqrt"        => OpFloatSqrt,
            "sext"        => OpSext,
            "zext"        => OpZext,
            "const"       => SpaceConst,
            "trunc"       => OpTrunc,
            "carry"       => OpCarry,
            "floor"       => OpFloor,
            "local"       => LocalKey,
            "round"       => OpRound,
            "unimpl"      => Unimpl,
            "unique"      => SpaceUnique,
            "return"      => ReturnKey,
            "scarry"      => OpScarry,
            "borrow"      => OpBorrow,
            "sborrow"     => OpSborrow,
            "popcount"    => OpPopcount,
            "int2float"   => OpInt2float,
            "float2float" => OpFloat2float,
            //Generic Ident
            other         => Ident(other.to_owned()),
        };
        Ok((rest, token))
    }
}
