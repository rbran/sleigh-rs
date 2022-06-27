use nom::bytes::complete::tag;
use nom::combinator::{map, opt};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::IResult;

use crate::base::{empty_space0, empty_space1, ident};
use crate::syntax::Syntax;
use crate::IDENT_INSTRUCTION;

use super::disassembly::Disassembly;
use super::pattern::Pattern;

#[derive(Clone, Debug)]
pub struct WithBlock<'a> {
    table_name: Option<&'a str>,
    pub pattern: Pattern<'a>,
    pub dissasembly: Option<Disassembly<'a>>,
    pub body: Syntax<'a>,
}

impl<'a> WithBlock<'a> {
    pub fn table_name(&self) -> &'a str {
        self.table_name.unwrap_or(IDENT_INSTRUCTION)
    }
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            tuple((
                delimited(
                    pair(tag("with"), empty_space1),
                    opt(ident),
                    pair(empty_space0, tag(":")),
                ),
                preceded(empty_space0, Pattern::parse),
                opt(preceded(
                    empty_space0,
                    delimited(
                        pair(tag("["), empty_space0),
                        Disassembly::parse,
                        pair(empty_space0, tag("]")),
                    ),
                )),
                preceded(
                    empty_space0,
                    delimited(
                        pair(tag("{"), empty_space0),
                        Syntax::parse,
                        pair(empty_space0, tag("}")),
                    ),
                ),
            )),
            |(table_name, pattern, dissasembly, body)| Self {
                table_name,
                pattern,
                dissasembly,
                body,
            },
        )(input)
    }
}
