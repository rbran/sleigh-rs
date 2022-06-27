use nom::bytes::complete::tag;
use nom::combinator::{cut, map};
use nom::sequence::{pair, preceded};
use nom::IResult;

use crate::base::{empty_space1, ident};

#[derive(Clone, Debug)]
pub struct UserFunction<'a>(pub &'a str);

impl<'a> UserFunction<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            preceded(pair(tag("pcodeop"), empty_space1), cut(ident)),
            Self,
        )(input)
    }
}
