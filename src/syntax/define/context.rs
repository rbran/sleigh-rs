use nom::combinator::{cut, map};
use nom::multi::many0;
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::syntax::parser::{ident, number, this_ident};
use crate::{NumberUnsigned, SleighError, Span};

use super::TokenFieldAttribute;

#[derive(Clone, Debug)]
pub struct Context {
    pub varnode_name: String,
    pub varnode_span: Span,
    pub fields: Vec<ContextField>,
}

impl Context {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            preceded(
                this_ident("context"),
                cut(pair(ident, many0(ContextField::parse))),
            ),
            |((varnode_name, varnode_span), fields)| Context {
                varnode_name,
                varnode_span: varnode_span.clone(),
                fields,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct ContextField {
    pub src: Span,
    pub name: String,
    pub start: NumberUnsigned,
    pub end: NumberUnsigned,
    pub attributes: Vec<ContextFieldAttribute>,
}

impl ContextField {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            tuple((
                terminated(ident, tag!("=")),
                delimited(
                    tag!("("),
                    separated_pair(number, tag!(","), number),
                    tag!(")"),
                ),
                many0(ContextFieldAttribute::parse),
            )),
            |((name, name_src), ((start, _), (end, _)), attributes)| Self {
                src: name_src.clone(),
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
    fn parse(input_ori: &[Token]) -> IResult<&[Token], Self, SleighError> {
        let (input, (att, location)) = ident(input_ori)?;
        Self::from_str(&att)
            .map(|att| (input, att))
            .ok_or(nom::Err::Error(SleighError::StatementInvalid(
                location.clone(),
            )))
    }
}
