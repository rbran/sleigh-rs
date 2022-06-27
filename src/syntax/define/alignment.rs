use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::sequence::{preceded, tuple};
use nom::IResult;

use crate::base::{empty_space0, number_unsig, IntTypeU};

#[derive(Clone, Debug)]
pub struct Alignment(pub IntTypeU);

impl Alignment {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        preceded(
            tuple((tag("alignment"), empty_space0, tag("="), empty_space0)),
            map(number_unsig, Self),
        )(input)
    }
}
