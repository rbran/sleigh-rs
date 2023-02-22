use nom::branch::alt;
use nom::combinator::{cut, value};
use nom::sequence::{pair, preceded};
use nom::IResult;

use crate::SleighError;
use crate::preprocessor::token::Token;
use crate::syntax::define::Endian;
use crate::syntax::parser::this_ident;

impl Endian {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        preceded(
            pair(this_ident("endian"), tag!("=")),
            cut(alt((
                value(Self::Little, this_ident("little")),
                value(Self::Big, this_ident("big")),
            ))),
        )(input)
    }
}
