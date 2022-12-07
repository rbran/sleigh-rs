use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{cut, map, value};
use nom::multi::many0;
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use crate::base::{empty_space0, empty_space1, ident, number_unsig, IntTypeU};
use crate::semantic::space::SpaceType;

impl SpaceType {
    fn parse(input: &str) -> IResult<&str, SpaceType> {
        alt((
            value(SpaceType::Ram, tag("ram_space")),
            value(SpaceType::Rom, tag("rom_space")),
            value(SpaceType::Register, tag("register_space")),
        ))(input)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Attribute {
    Type(SpaceType),
    Size(IntTypeU),
    WordSize(IntTypeU),
    Default,
}
impl Attribute {
    fn parse(input: &str) -> IResult<&str, Attribute> {
        alt((
            value(Attribute::Default, tag("default")),
            map(
                preceded(
                    tuple((tag("type"), empty_space0, tag("="), empty_space0)),
                    SpaceType::parse,
                ),
                Attribute::Type,
            ),
            map(
                preceded(
                    tuple((tag("size"), empty_space0, tag("="), empty_space0)),
                    number_unsig,
                ),
                Attribute::Size,
            ),
            map(
                preceded(
                    tuple((
                        tag("wordsize"),
                        empty_space0,
                        tag("="),
                        empty_space0,
                    )),
                    number_unsig,
                ),
                Attribute::WordSize,
            ),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub struct Space<'a> {
    pub name: &'a str,
    pub attributes: Vec<Attribute>,
}

impl<'a> Space<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            preceded(
                pair(tag("space"), empty_space1),
                cut(pair(
                    ident,
                    many0(preceded(empty_space1, Attribute::parse)),
                )),
            ),
            |(name, attributes)| Space { name, attributes },
        )(input)
    }
}
