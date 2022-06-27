use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{cut, value};
use nom::sequence::{preceded, tuple};
use nom::IResult;

use crate::base::empty_space0;
use crate::syntax::define::Endian;

impl Endian {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        preceded(
            tuple((tag("endian"), empty_space0, tag("="), empty_space0)),
            cut(alt((
                value(Self::Little, tag("little")),
                value(Self::Big, tag("big")),
            ))),
        )(input)
    }
}
