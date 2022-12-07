use nom::bytes::complete::tag;
use nom::combinator::{cut, map};
use nom::multi::many0;
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::IResult;

use crate::base::{empty_space0, empty_space1, ident, number_unsig, IntTypeU};

use super::TokenFieldAttribute;

#[derive(Clone, Debug)]
pub struct Context<'a> {
    pub varnode_name: &'a str,
    pub fields: Vec<ContextField<'a>>,
}

impl<'a> Context<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            preceded(
                pair(tag("context"), empty_space1),
                cut(pair(
                    ident,
                    many0(preceded(empty_space1, ContextField::parse)),
                )),
            ),
            |(varnode_name, fields)| Context {
                varnode_name,
                fields,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct ContextField<'a> {
    pub name: &'a str,
    pub start: IntTypeU,
    pub end: IntTypeU,
    pub attributes: Vec<ContextFieldAttribute>,
}

impl<'a> ContextField<'a> {
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
                many0(preceded(empty_space1, ContextFieldAttribute::parse)),
            )),
            |(name, (start, end), attributes)| Self {
                name,
                start,
                end,
                attributes,
            },
        )(input)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ContextFieldAttribute {
    Token(TokenFieldAttribute),
    Noflow,
}

impl ContextFieldAttribute {
    pub(crate) fn from_str(name: &str) -> Option<Self> {
        TokenFieldAttribute::from_str(name)
            .map(Self::Token)
            .or_else(|| match name {
                "noflow" => Some(Self::Noflow),
                _ => None,
            })
    }
    fn parse(input_ori: &str) -> IResult<&str, Self> {
        let (input, att) = ident(input_ori)?;
        Self::from_str(att)
            .map(|att| (input, att))
            .ok_or(nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::MapOpt,
            }))
    }
}
