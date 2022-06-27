use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::space0;
use nom::combinator::{consumed, map, value};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;

use crate::base::{end_of_line, ident, string};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IfCheck<'a> {
    Defined(&'a str),
    NotDefined(&'a str),
    Cmp {
        name: &'a str,
        op: CmpOp,
        value: String,
        src: &'a str,
    },
    Op {
        left: Box<IfCheck<'a>>,
        op: BoolOp,
        right: Box<IfCheck<'a>>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum CmpOp {
    Eq,
    Ne,
}

impl CmpOp {
    pub fn cmp(&self, op1: &str, op2: &str) -> bool {
        match self {
            CmpOp::Eq => op1 == op2,
            CmpOp::Ne => op1 != op2,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BoolOp {
    And,
    Or,
    Xor,
}

impl BoolOp {
    pub fn check(&self, cond1: bool, cond2: bool) -> bool {
        match self {
            BoolOp::And => cond1 && cond2,
            BoolOp::Or => cond1 || cond2,
            BoolOp::Xor => cond1 ^ cond2,
        }
    }
}

fn bool_op(input: &str) -> IResult<&str, BoolOp> {
    alt((
        value(BoolOp::Or, tag("||")),
        value(BoolOp::And, tag("&&")),
        value(BoolOp::Xor, tag("^^")),
    ))(input)
}

pub(crate) fn if_cond_check(input: &str) -> IResult<&str, IfCheck> {
    alt((
        map(
            consumed(tuple((
                ident,
                delimited(
                    space0,
                    alt((
                        value(CmpOp::Eq, tag("==")),
                        value(CmpOp::Ne, tag("!=")),
                    )),
                    space0,
                ),
                string,
            ))),
            |(src, (name, op, value))| IfCheck::Cmp {
                name,
                op,
                value,
                src,
            },
        ),
        map(
            preceded(
                pair(tag("defined"), space0),
                delimited(tag("("), ident, tag(")")),
            ),
            IfCheck::Defined,
        ),
        //if `(` then call recursive
        preceded(tag("("), if_cond_not_root),
    ))(input)
}

//TODO: improve this
pub(crate) fn if_cond(input: &str) -> IResult<&str, IfCheck> {
    map(
        pair(
            terminated(if_cond_check, space0),
            alt((
                // end of the if, only if we are root and not recursive
                map(end_of_line, |_| None),
                map(
                    pair(terminated(bool_op, space0), if_cond),
                    |(op, cond2)| Some((op, cond2)),
                ),
            )),
        ),
        |(cond1, rest)| match rest {
            Some((op, cond2)) => IfCheck::Op {
                op,
                left: Box::new(cond1),
                right: Box::new(cond2),
            },
            None => cond1,
        },
    )(input)
}

pub(crate) fn if_cond_not_root(input: &str) -> IResult<&str, IfCheck> {
    map(
        pair(
            terminated(if_cond_check, space0),
            alt((
                map(
                    pair(terminated(bool_op, space0), if_cond_not_root),
                    |(op, cond2)| Some((op, cond2)),
                ),
                // if ')' mean end of this recursice, only happen if not root
                map(tag(")"), |_| None),
            )),
        ),
        |(cond1, rest)| match rest {
            Some((op, cond2)) => IfCheck::Op {
                op,
                left: Box::new(cond1),
                right: Box::new(cond2),
            },
            None => cond1,
        },
    )(input)
}
