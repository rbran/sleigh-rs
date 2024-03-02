use crate::preprocessor::token::Token;
use crate::syntax::parser::{ident, this_ident};
use crate::{SleighError, Span};
use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::IResult;

use super::op::ByteRangeLsb;
use super::{expr, op};

#[derive(Clone, Debug)]
pub enum Export {
    Value(expr::Expr),
    Reference {
        space: op::AddrDereference,
        addr: expr::Expr,
    },
    Const {
        size: ByteRangeLsb,
        src: Span,
        value: String,
    },
    //TODO Unique
    //Unique{
    //  value: &[Token],
    //}
}

impl Export {
    fn parse_value(
        input: &[Token],
    ) -> IResult<&[Token], Self, Box<SleighError>> {
        map(expr::Expr::parse, Self::Value)(input)
    }
    fn parse_reference(
        input: &[Token],
    ) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            pair(op::AddrDereference::parse, expr::Expr::parse),
            |(space, addr)| Self::Reference { space, addr },
        )(input)
    }
    fn parse_const(
        input: &[Token],
    ) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            preceded(
                tuple((tag!("*"), tag!("["), tag!("const"), tag!("]"))),
                pair(ByteRangeLsb::parse, ident),
            ),
            |(size, (value, value_src))| Self::Const {
                size,
                value,
                src: value_src.clone(),
            },
        )(input)
    }
    fn parse_unique(
        input: &[Token],
    ) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            preceded(
                tuple((tag!("*"), tag!("["), tag!("unique"), tag!("]"))),
                pair(ByteRangeLsb::parse, ident),
            ),
            |_| todo!(),
        )(input)
    }
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        delimited(
            this_ident("export"),
            //NOTE order is important
            alt((
                Self::parse_unique,
                Self::parse_const,
                Self::parse_reference,
                Self::parse_value,
            )),
            tag!(";"),
        )(input)
    }
}
