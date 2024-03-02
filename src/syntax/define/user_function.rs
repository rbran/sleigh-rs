use nom::combinator::{cut, map};
use nom::sequence::preceded;
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::syntax::parser::{ident, this_ident};
use crate::{SleighError, Span};

#[derive(Clone, Debug)]
pub struct UserFunction {
    pub src: Span,
    pub name: String,
}

impl UserFunction {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            preceded(this_ident("pcodeop"), cut(ident)),
            |(name, name_src)| Self {
                name,
                src: name_src.clone(),
            },
        )(input)
    }
}
