use nom::branch::alt;
use nom::combinator::{cut, map, value};
use nom::multi::many0;
use nom::sequence::{pair, preceded};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::semantic::space::SpaceType;
use crate::syntax::parser::{ident, number, this_ident};
use crate::{NumberUnsigned, SleighError, Span};

impl SpaceType {
    fn parse(input: &[Token]) -> IResult<&[Token], SpaceType, SleighError> {
        alt((
            value(SpaceType::Ram, this_ident("ram_space")),
            value(SpaceType::Rom, this_ident("rom_space")),
            value(SpaceType::Register, this_ident("register_space")),
        ))(input)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Attribute {
    Type(SpaceType),
    Size(NumberUnsigned),
    WordSize(NumberUnsigned),
    Default,
}
impl Attribute {
    fn parse(input: &[Token]) -> IResult<&[Token], Attribute, SleighError> {
        alt((
            value(Attribute::Default, this_ident("default")),
            map(
                preceded(pair(this_ident("type"), tag!("=")), SpaceType::parse),
                Attribute::Type,
            ),
            map(
                preceded(pair(this_ident("size"), tag!("=")), number),
                |(x, span)| Attribute::Size(x),
            ),
            map(
                preceded(pair(this_ident("wordsize"), tag!("=")), number),
                |(x, span)| Attribute::WordSize(x),
            ),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub struct Space {
    pub name: String,
    pub src: Span,
    pub attributes: Vec<Attribute>,
}

impl Space {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            preceded(
                this_ident("space"),
                cut(pair(ident, many0(Attribute::parse))),
            ),
            |((name, name_span), attributes)| Space {
                name,
                src: name_span.clone(),
                attributes,
            },
        )(input)
    }
}
