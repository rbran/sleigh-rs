use nom::bytes::complete::tag;
use nom::combinator::{cut, map, opt};
use nom::multi::many0;
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::IResult;

use crate::base::{empty_space0, empty_space1, ident, number_unsig, IntTypeU};
use crate::syntax::define::Endian;

#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub name: &'a str,
    pub size: IntTypeU,
    pub endian: Option<Endian>,
    pub token_fields: Vec<TokenField<'a>>,
}

impl<'a> Token<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            preceded(
                pair(tag("token"), empty_space1),
                cut(tuple((
                    ident,
                    delimited(
                        tuple((empty_space0, tag("("), empty_space0)),
                        number_unsig,
                        tuple((empty_space0, tag(")"))),
                    ),
                    opt(preceded(empty_space0, Endian::parse)),
                    many0(preceded(empty_space1, TokenField::parse)),
                ))),
            ),
            |(name, size, endian, token_fields)| Token {
                name,
                size,
                endian,
                token_fields,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct TokenField<'a> {
    pub name: &'a str,
    pub start: IntTypeU,
    pub end: IntTypeU,
    pub attributes: Vec<TokenFieldAttribute>,
}

impl<'a> TokenField<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            tuple((
                terminated(
                    ident,
                    tuple((empty_space0, tag("="), empty_space0)),
                ),
                delimited(
                    pair(tag("("), empty_space0),
                    separated_pair(
                        number_unsig,
                        tuple((empty_space0, tag(","), empty_space0)),
                        number_unsig,
                    ),
                    pair(empty_space0, tag(")")),
                ),
                many0(preceded(empty_space0, TokenFieldAttribute::parse)),
            )),
            |(name, (start, end), attributes)| TokenField {
                name,
                start,
                end,
                attributes,
            },
        )(input)
    }
}

#[derive(Clone, Debug, Copy)]
pub enum TokenFieldAttribute {
    Hex,
    Dec,
    Signed,
}

impl TokenFieldAttribute {
    pub(crate) fn from_str(name: &str) -> Option<Self> {
        match name {
            "signed" => Some(Self::Signed),
            "hex" => Some(Self::Hex),
            "dec" => Some(Self::Dec),
            _ => None,
        }
    }
    fn parse(input_ori: &str) -> IResult<&str, TokenFieldAttribute> {
        let (input, att) = ident(input_ori)?;
        Self::from_str(att)
            .map(|att| (input, att))
            .ok_or(nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::MapOpt,
            }))
    }
}
