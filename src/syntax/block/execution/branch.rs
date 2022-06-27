use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt, value};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;

use crate::base::{empty_space0, empty_space1};
use crate::semantic::execution::BranchCall;

use super::expr::Expr;
use super::{expr, Label};

#[derive(Clone, Debug)]
pub struct Branch<'a> {
    pub cond: Option<expr::Expr<'a>>,
    pub call: BranchCall,
    pub dst: BranchDst<'a>,
}
impl<'a> Branch<'a> {
    pub fn parse_cond(input: &'a str) -> IResult<&'a str, Expr> {
        preceded(pair(tag("if"), empty_space0), expr::Expr::parse)(input)
    }
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            terminated(
                tuple((
                    opt(terminated(Self::parse_cond, empty_space0)),
                    BranchCall::parse,
                    BranchDst::parse,
                )),
                pair(empty_space0, tag(";")),
            ),
            |(cond, call, dst)| Self { cond, call, dst },
        )(input)
    }
}

impl BranchCall {
    pub fn parse(input: &str) -> IResult<&str, BranchCall> {
        alt((
            value(BranchCall::Goto, tag("goto")),
            value(BranchCall::Call, tag("call")),
            value(BranchCall::Return, tag("return")),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub enum BranchDst<'a> {
    Label(Label<'a>),
    Cpu { direct: bool, expr: expr::Expr<'a> },
}
impl<'a> BranchDst<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(preceded(empty_space0, Label::parse), |label| {
                Self::Label(label)
            }),
            map(preceded(empty_space1, expr::Expr::parse), |expr| {
                Self::Cpu { direct: true, expr }
            }),
            map(
                delimited(
                    tuple((empty_space0, pair(tag("["), empty_space0))),
                    expr::Expr::parse,
                    pair(empty_space0, tag("]")),
                ),
                |expr| Self::Cpu {
                    direct: false,
                    expr,
                },
            ),
        ))(input)
    }
}
