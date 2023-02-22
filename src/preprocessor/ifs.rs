use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, space0};
use nom::combinator::{consumed, map, peek, value};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;

use super::parser::{end_of_line, ident, string};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IfCheckOwned {
    Defined(String),
    NotDefined(String),
    Cmp {
        name: String,
        op: CmpOp,
        value: String,
    },
    Op {
        left: Box<IfCheckOwned>,
        op: BoolOp,
        right: Box<IfCheckOwned>,
    },
}

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
    //TODO make the verification lazy, so we can short circuit this
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
                map(peek(end_of_line), |_| None),
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

pub(crate) fn if_cond_check_owned(input: &str) -> IResult<&str, IfCheckOwned> {
    alt((
        map(
            tuple((
                map(ident, |x| x.to_owned()),
                delimited(
                    space0,
                    alt((
                        value(CmpOp::Eq, tag("==")),
                        value(CmpOp::Ne, tag("!=")),
                    )),
                    space0,
                ),
                string,
            )),
            |(name, op, value)| IfCheckOwned::Cmp { name, op, value },
        ),
        map(
            preceded(
                pair(tag("defined"), space0),
                delimited(tag("("), map(ident, |x| x.to_owned()), tag(")")),
            ),
            IfCheckOwned::Defined,
        ),
        //if `(` then call recursive
        preceded(tag("("), if_cond_owned_not_root),
    ))(input)
}

//TODO: improve this
pub(crate) fn if_cond_owned(input: &str) -> IResult<&str, IfCheckOwned> {
    map(
        pair(
            terminated(if_cond_check_owned, space0),
            alt((
                // end of the if, only if we are root and not recursive
                map(pair(end_of_line, peek(line_ending)), |_| None),
                map(
                    pair(terminated(bool_op, space0), if_cond_owned),
                    |(op, cond2)| Some((op, cond2)),
                ),
            )),
        ),
        |(cond1, rest)| match rest {
            Some((op, cond2)) => IfCheckOwned::Op {
                op,
                left: Box::new(cond1),
                right: Box::new(cond2),
            },
            None => cond1,
        },
    )(input)
}

pub(crate) fn if_cond_owned_not_root(
    input: &str,
) -> IResult<&str, IfCheckOwned> {
    map(
        pair(
            terminated(if_cond_check_owned, space0),
            alt((
                map(
                    pair(terminated(bool_op, space0), if_cond_owned_not_root),
                    |(op, cond2)| Some((op, cond2)),
                ),
                // if ')' mean end of this recursice, only happen if not root
                map(tag(")"), |_| None),
            )),
        ),
        |(cond1, rest)| match rest {
            Some((op, cond2)) => IfCheckOwned::Op {
                op,
                left: Box::new(cond1),
                right: Box::new(cond2),
            },
            None => cond1,
        },
    )(input)
}
