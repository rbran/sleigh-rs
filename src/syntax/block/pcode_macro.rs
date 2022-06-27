use nom::bytes::complete::tag;
use nom::combinator::{consumed, map};
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::IResult;

use crate::base::{empty_space0, empty_space1, ident};

use super::execution::Execution;

//TODO impl correctly the PCode Macro
#[derive(Clone, Debug)]
pub struct PcodeMacro<'a> {
    pub src: &'a str,
    pub name: &'a str,
    pub params: Vec<&'a str>,
    pub body: Execution<'a>,
}

impl<'a> PcodeMacro<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(tuple((
                preceded(pair(tag("macro"), empty_space1), ident),
                delimited(
                    tuple((empty_space0, tag("("), empty_space0)),
                    separated_list0(
                        tuple((empty_space0, tag(","), empty_space0)),
                        ident,
                    ),
                    pair(empty_space0, tag(")")),
                ),
                delimited(
                    tuple((empty_space0, tag("{"), empty_space0)),
                    Execution::parse,
                    pair(empty_space0, tag("}")),
                ),
            ))),
            |(src, (name, params, body))| Self {
                name,
                params,
                body,
                src,
            },
        )(input)
    }
}
