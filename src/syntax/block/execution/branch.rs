use nom::branch::alt;
use nom::combinator::{map, opt, value};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::semantic::execution::BranchCall;
use crate::SleighError;

use super::expr::Expr;
use super::{expr, Label};

#[derive(Clone, Debug)]
pub struct Branch {
    pub cond: Option<expr::Expr>,
    pub call: BranchCall,
    pub dst: BranchDst,
}
impl Branch {
    pub fn parse_cond(
        input: &[Token],
    ) -> IResult<&[Token], Expr, Box<SleighError>> {
        preceded(tag!("if"), expr::Expr::parse)(input)
    }
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            terminated(
                tuple((
                    opt(Self::parse_cond),
                    BranchCall::parse,
                    BranchDst::parse,
                )),
                tag!(";"),
            ),
            |(cond, call, dst)| Self { cond, call, dst },
        )(input)
    }
}

impl BranchCall {
    pub fn parse(
        input: &[Token],
    ) -> IResult<&[Token], BranchCall, Box<SleighError>> {
        alt((
            value(BranchCall::Goto, tag!("goto")),
            value(BranchCall::Call, tag!("call")),
            value(BranchCall::Return, tag!("return")),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub enum BranchDst {
    Label(Label),
    Cpu { direct: bool, expr: expr::Expr },
}
impl BranchDst {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        alt((
            map(Label::parse, Self::Label),
            map(expr::Expr::parse, |expr| Self::Cpu { direct: true, expr }),
            map(delimited(tag!("["), expr::Expr::parse, tag!("]")), |expr| {
                Self::Cpu {
                    direct: false,
                    expr,
                }
            }),
        ))(input)
    }
}
