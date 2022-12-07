use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{consumed, map, map_res, opt, value};
use nom::multi::{many0, separated_list0};
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::IResult;

use super::disassembly;
use crate::base::{empty_space0, ident};

pub use crate::semantic::pattern::CmpOp;
pub use crate::semantic::pattern::Ellipsis;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Op {
    And,
    Or,
}
impl Op {
    pub fn parse(input: &str) -> IResult<&str, Op> {
        alt((value(Op::Or, tag("|")), value(Op::And, tag("&"))))(input)
    }
}

impl CmpOp {
    pub fn parse(input: &str) -> IResult<&str, CmpOp> {
        alt((
            value(CmpOp::Le, tag("<=")),
            value(CmpOp::Ge, tag(">=")),
            value(CmpOp::Ne, tag("!=")),
            //TODO order is important, so ">=" is not consumed as ">"
            value(CmpOp::Eq, tag("=")),
            value(CmpOp::Lt, tag("<")),
            value(CmpOp::Gt, tag(">")),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub struct ConstraintValue<'a> {
    pub expr: disassembly::Expr<'a>,
}

impl<'a> ConstraintValue<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            alt((
                delimited(
                    pair(tag("("), empty_space0),
                    |x| disassembly::Expr::parse(x, true),
                    pair(empty_space0, tag(")")),
                ),
                |x| disassembly::Expr::parse(x, false),
            )),
            |expr| ConstraintValue { expr },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Constraint<'a> {
    pub op: CmpOp,
    pub value: ConstraintValue<'a>,
}

impl<'a> Constraint<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            separated_pair(CmpOp::parse, empty_space0, ConstraintValue::parse),
            |(op, value)| Self { op, value },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub enum Field<'a> {
    Field {
        field: &'a str,
        constraint: Option<Constraint<'a>>,
    },
    SubPattern(Pattern<'a>),
}
impl<'a> Field<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(
                delimited(
                    pair(tag("("), empty_space0),
                    Pattern::parse,
                    pair(empty_space0, tag(")")),
                ),
                |sub| Field::SubPattern(sub),
            ),
            map(
                pair(ident, opt(preceded(empty_space0, Constraint::parse))),
                |(field, constraint)| Field::Field { field, constraint },
            ),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub struct Element<'a> {
    pub field: Field<'a>,
    pub ellipsis: Option<Ellipsis>,
}

impl<'a> Element<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map_res(
            tuple((
                opt(terminated(
                    value(Ellipsis::Left, tag("...")),
                    empty_space0,
                )),
                Field::parse,
                opt(preceded(empty_space0, value(Ellipsis::Right, tag("...")))),
            )),
            |(left, field, right)| {
                let ellipsis = match (left, right) {
                    (Some(_), Some(_)) => return Err(""),
                    (left, right) => left.or(right),
                };
                Ok(Element { field, ellipsis })
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Block<'a> {
    pub src: &'a str,
    pub first: Element<'a>,
    pub elements: Vec<(Op, Element<'a>)>,
}

impl<'a> Block<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(tuple((
                preceded(empty_space0, Element::parse),
                many0(pair(
                    preceded(empty_space0, Op::parse),
                    preceded(empty_space0, Element::parse),
                )),
            ))),
            |(src, (first, elements))| Self {
                src,
                first,
                elements,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Pattern<'a> {
    //NOTE: point after the `is`
    pub src: &'a str,
    pub blocks: Vec<Block<'a>>,
}
impl<'a> IntoIterator for Pattern<'a> {
    type Item = Block<'a>;
    type IntoIter = std::vec::IntoIter<Block<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.blocks.into_iter()
    }
}

impl<'a> Pattern<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(separated_list0(
                tuple((empty_space0, tag(";"), empty_space0)),
                Block::parse,
            )),
            |(src, blocks)| Pattern { src, blocks },
        )(input)
    }
}
