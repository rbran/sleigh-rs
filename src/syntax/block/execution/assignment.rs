use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::combinator::opt;
use nom::sequence::delimited;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::sequence::{pair, preceded};
use nom::IResult;

use crate::base::{empty_space0, empty_space1, ident};
use crate::syntax::BitRange;

use super::op::ByteRangeLsb;
use super::op::ByteRangeMsb;
use super::{expr, op};

#[derive(Clone, Debug)]
pub struct Declare<'a> {
    pub ident: &'a str,
    pub size: Option<ByteRangeLsb<'a>>,
}
impl<'a> Declare<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            delimited(
                pair(tag("local"), empty_space0),
                pair(ident, opt(preceded(empty_space0, ByteRangeLsb::parse))),
                pair(empty_space0, tag(";")),
            ),
            |(ident, op)| Self { ident, size: op },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct MemWrite<'a> {
    pub src: &'a str,
    pub mem: op::AddrDereference<'a>,
    pub addr: expr::Expr<'a>,
    pub right: expr::Expr<'a>,
}
impl<'a> MemWrite<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            terminated(
                tuple((
                    terminated(op::AddrDereference::parse, empty_space0),
                    expr::Expr::parse,
                    delimited(empty_space0, tag("="), empty_space0),
                    expr::Expr::parse,
                )),
                pair(empty_space0, tag(";")),
            ),
            |(mem, addr, src, right)| Self {
                src,
                mem,
                addr,
                right,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Assignment<'a> {
    pub local: bool,
    pub ident: &'a str,
    pub src: &'a str,
    pub op: Option<OpLeft<'a>>,
    pub right: expr::Expr<'a>,
}
impl<'a> Assignment<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            terminated(
                tuple((
                    map(opt(pair(tag("local"), empty_space1)), |x| x.is_some()),
                    ident,
                    opt(preceded(empty_space0, OpLeft::parse)),
                    delimited(empty_space0, tag("="), empty_space0),
                    expr::Expr::parse,
                )),
                pair(empty_space0, tag(";")),
            ),
            |(local, ident, op, src, right)| Self {
                local,
                ident,
                src,
                op,
                right,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub enum OpLeft<'a> {
    //assign to variable
    BitRange(BitRange<'a>),
    ByteRangeMsb(ByteRangeMsb<'a>),
    ByteRangeLsb(ByteRangeLsb<'a>),
}

impl<'a> OpLeft<'a> {
    fn parse(input: &'a str) -> IResult<&str, Self> {
        alt((
            map(BitRange::parse, Self::BitRange),
            map(ByteRangeMsb::parse, |_x| {
                todo!("MSB assignment is a thing?");
                //Self::ByteRangeMsb(x)
            }),
            map(ByteRangeLsb::parse, Self::ByteRangeLsb),
        ))(input)
    }
}
