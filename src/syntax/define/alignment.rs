use nom::combinator::map;
use nom::sequence::{pair, preceded};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::syntax::parser::{number, this_ident};
use crate::{NumberUnsigned, SleighError};

#[derive(Clone, Debug)]
pub struct Alignment(pub NumberUnsigned);

impl Alignment {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        preceded(
            pair(this_ident("alignment"), tag!("=")),
            map(number, |(num, _span)| Self(num)),
        )(input)
    }
}
