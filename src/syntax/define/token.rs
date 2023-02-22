use nom::combinator::{cut, map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, preceded, separated_pair, terminated, tuple};
use nom::IResult;

use crate::preprocessor::token::Token as SleighToken;

use crate::syntax::define::Endian;
use crate::syntax::parser::{ident, number, this_ident};
use crate::{NumberUnsigned, SleighError, Span};

#[derive(Clone, Debug)]
pub struct Token {
    pub name: String,
    pub src: Span,
    pub size: NumberUnsigned,
    pub endian: Option<Endian>,
    pub token_fields: Vec<TokenField>,
}

impl Token {
    pub fn parse(
        input: &[SleighToken],
    ) -> IResult<&[SleighToken], Self, SleighError> {
        map(
            preceded(
                this_ident("token"),
                cut(tuple((
                    ident,
                    delimited(tag!("("), number, tag!(")")),
                    opt(Endian::parse),
                    many0(TokenField::parse),
                ))),
            ),
            |((name, name_span), (size, _), endian, token_fields)| Token {
                name,
                src: name_span.clone(),
                size,
                endian,
                token_fields,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct TokenField {
    pub name: String,
    pub name_span: Span,
    pub start: NumberUnsigned,
    pub end: NumberUnsigned,
    pub attributes: Vec<TokenFieldAttribute>,
}

impl TokenField {
    pub fn parse(
        input: &[SleighToken],
    ) -> IResult<&[SleighToken], Self, SleighError> {
        map(
            tuple((
                terminated(ident, tag!("=")),
                delimited(
                    tag!("("),
                    separated_pair(number, tag!(","), number),
                    tag!(")"),
                ),
                many0(TokenFieldAttribute::parse),
            )),
            |((name, name_span), ((start, _), (end, _)), attributes)| {
                TokenField {
                    name,
                    name_span: name_span.clone(),
                    start,
                    end,
                    attributes,
                }
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
    fn parse(
        input_ori: &[SleighToken],
    ) -> IResult<&[SleighToken], TokenFieldAttribute, SleighError> {
        let (input, (att, span)) = ident(input_ori)?;
        Self::from_str(&att)
            .map(|att| (input, att))
            .ok_or(nom::Err::Error(SleighError::StatementInvalid(span.clone())))
    }
}
