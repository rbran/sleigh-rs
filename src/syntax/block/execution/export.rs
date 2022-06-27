use crate::base::empty_space1;
use crate::syntax::block::execution::ident;
use crate::syntax::separated_pair;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::IResult;

use super::op::ByteRangeLsb;
use super::{expr, op};
use crate::base::empty_space0;

#[derive(Clone, Debug)]
pub enum Export<'a> {
    Value(expr::Expr<'a>),
    Reference {
        space: op::AddrDereference<'a>,
        addr: expr::Expr<'a>,
    },
    Const {
        size: ByteRangeLsb<'a>,
        value: &'a str,
    },
    //TODO Unique
    //Unique{
    //  value: &'a str,
    //}
}

impl<'a> Export<'a> {
    fn parse_value(input: &'a str) -> IResult<&'a str, Self> {
        map(expr::Expr::parse, |value| Self::Value(value))(input)
    }
    fn parse_reference(input: &'a str) -> IResult<&'a str, Self> {
        map(
            separated_pair(
                op::AddrDereference::parse,
                empty_space1,
                expr::Expr::parse,
            ),
            |(space, addr)| Self::Reference { space, addr },
        )(input)
    }
    fn parse_const(input: &'a str) -> IResult<&'a str, Self> {
        map(
            preceded(
                tuple((
                    tag("*"),
                    empty_space0,
                    tag("["),
                    empty_space0,
                    tag("const"),
                    empty_space0,
                    tag("]"),
                    empty_space0,
                )),
                separated_pair(ByteRangeLsb::parse, empty_space1, ident),
            ),
            |(size, value)| Self::Const { size, value },
        )(input)
    }
    fn parse_unique(input: &'a str) -> IResult<&'a str, Self> {
        map(
            preceded(
                tuple((
                    tag("*"),
                    empty_space0,
                    tag("["),
                    empty_space0,
                    tag("unique"),
                    empty_space0,
                    tag("]"),
                    empty_space0,
                )),
                separated_pair(ByteRangeLsb::parse, empty_space1, ident),
            ),
            |_| todo!(),
        )(input)
    }
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        delimited(
            pair(tag("export"), empty_space0),
            //NOTE order is important
            alt((
                Self::parse_unique,
                Self::parse_const,
                Self::parse_reference,
                Self::parse_value,
            )),
            pair(empty_space0, tag(";")),
        )(input)
    }
}
