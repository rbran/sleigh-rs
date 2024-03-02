use nom::combinator::map;
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::syntax::parser::{ident, number, registerlist, this_ident};
use crate::{NumberUnsigned, SleighError, Span};

#[derive(Clone, Debug)]
pub struct Varnode {
    pub space_name: String,
    pub space_span: Span,
    pub offset: NumberUnsigned,
    pub value_bytes: NumberUnsigned,
    pub names: Vec<(Option<String>, Span)>,
}

impl Varnode {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            tuple((
                ident,
                preceded(pair(this_ident("offset"), tag!("=")), number),
                preceded(pair(this_ident("size"), tag!("=")), number),
                registerlist,
            )),
            |(
                (space_name, space_span),
                (offset, _),
                (value_bytes, _),
                names,
            )| Varnode {
                space_name,
                space_span: space_span.clone(),
                offset,
                value_bytes,
                names,
            },
        )(input)
    }
}
