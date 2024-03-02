use nom::combinator::{cut, map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

use crate::preprocessor::token::Token as ParserToken;

use crate::syntax::define::Endian;
use crate::syntax::parser::{ident, number, this_ident};
use crate::syntax::BitRangeLsbMsb;
use crate::{NumberUnsigned, SleighError, Span};

use super::parse_endian;

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
        input: &[ParserToken],
    ) -> IResult<&[ParserToken], Self, Box<SleighError>> {
        map(
            preceded(
                this_ident("token"),
                cut(tuple((
                    ident,
                    delimited(tag!("("), number, tag!(")")),
                    opt(parse_endian),
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
    pub range: BitRangeLsbMsb,
    pub attributes: Vec<TokenFieldAttribute>,
}

impl TokenField {
    pub fn parse(
        input: &[ParserToken],
    ) -> IResult<&[ParserToken], Self, Box<SleighError>> {
        map(
            tuple((
                terminated(ident, tag!("=")),
                BitRangeLsbMsb::parse,
                many0(TokenFieldAttribute::parse),
            )),
            |((name, name_span), range, attributes)| TokenField {
                name,
                name_span: name_span.clone(),
                range,
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
    fn parse(
        input_ori: &[ParserToken],
    ) -> IResult<&[ParserToken], TokenFieldAttribute, Box<SleighError>> {
        let (input, (att, span)) = ident(input_ori)?;
        Self::from_str(&att)
            .map(|att| (input, att))
            .ok_or(nom::Err::Error(Box::new(SleighError::StatementInvalid(
                span.clone(),
            ))))
    }
}
