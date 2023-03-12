use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::syntax::parser::ident;
use crate::syntax::BitRangeLsbLen;
use crate::SleighError;
use crate::Span;

use super::op::ByteRangeLsb;
use super::op::ByteRangeMsb;
use super::{expr, op};

#[derive(Clone, Debug)]
pub struct Declare {
    pub src: Span,
    pub name: String,
    pub size: Option<ByteRangeLsb>,
}
impl Declare {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            delimited(
                tag!("local"),
                pair(ident, opt(ByteRangeLsb::parse)),
                tag!(";"),
            ),
            |((name, name_src), op)| Self {
                name,
                src: name_src.clone(),
                size: op,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct MemWrite {
    pub src: Span,
    pub mem: op::AddrDereference,
    pub addr: expr::Expr,
    pub right: expr::Expr,
}
impl MemWrite {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            tuple((
                op::AddrDereference::parse,
                expr::Expr::parse,
                tag!("="),
                expr::Expr::parse,
                tag!(";"),
            )),
            |(mem, addr, src, right, _end)| Self {
                src: src.clone(),
                mem,
                addr,
                right,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub local: bool,
    pub ident: String,
    pub src: Span,
    pub op: Option<OpLeft>,
    pub right: expr::Expr,
}
impl Assignment {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            terminated(
                tuple((
                    map(opt(tag!("local")), |x| x.is_some()),
                    ident,
                    opt(OpLeft::parse),
                    tag!("="),
                    expr::Expr::parse,
                )),
                tag!(";"),
            ),
            |(local, (ident, _), op, src, right)| Self {
                local,
                ident,
                src: src.clone(),
                op,
                right,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub enum OpLeft {
    //assign to variable
    BitRange(BitRangeLsbLen),
    ByteRangeMsb(ByteRangeMsb),
    ByteRangeLsb(ByteRangeLsb),
}

impl OpLeft {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        alt((
            map(BitRangeLsbLen::parse, Self::BitRange),
            map(ByteRangeMsb::parse, |_x| {
                todo!("MSB assignment is a thing?");
                //Self::ByteRangeMsb(x)
            }),
            map(ByteRangeLsb::parse, Self::ByteRangeLsb),
        ))(input)
    }
}
