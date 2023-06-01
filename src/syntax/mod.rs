#[macro_use]
pub mod parser;

pub mod attach;
pub mod block;
pub mod define;

use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::{separated_pair, tuple};
use nom::IResult;

use crate::preprocessor::token::{Token, TokenType};
use crate::preprocessor::FilePreProcessor;
use crate::syntax::parser::Parser;
use crate::{Number, NumberUnsigned, SleighError, Span};

use self::attach::Attach;
use self::block::pcode_macro::PcodeMacro;
use self::block::table::Constructor;
use self::block::with_block::WithBlock;
use self::define::Define;
use self::parser::{ident, number, number_signed, number_unsigned};

#[derive(Clone, Debug)]
pub enum Value {
    Number(Span, Number),
    Ident(Span, String),
}

impl Value {
    pub fn location(&self) -> &Span {
        match self {
            Value::Number(location, _) | Value::Ident(location, _) => location,
        }
    }
    pub fn parse_unsigned(
        input: &[Token],
    ) -> IResult<&[Token], Self, SleighError> {
        alt((
            map(number_unsigned, |(x, span)| Self::Number(span.clone(), x)),
            map(ident, |(x, span)| Self::Ident(span.clone(), x)),
        ))(input)
    }
    pub fn parse_signed(
        input: &[Token],
    ) -> IResult<&[Token], Self, SleighError> {
        alt((
            map(number_signed, |(x, span)| Self::Number(span.clone(), x)),
            map(ident, |(x, span)| Self::Ident(span.clone(), x)),
        ))(input)
    }
}

//TODO move this
#[derive(Clone, Debug)]
pub struct BitRangeLsbMsb {
    pub src: Span,
    pub lsb_bit: NumberUnsigned,
    pub msb_bit: NumberUnsigned,
}
impl BitRangeLsbMsb {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            tuple((
                tag!("("),
                separated_pair(number, tag!(","), number),
                tag!(")"),
            )),
            |(start, ((lsb_bit, _), (msb_bit, _)), end)| Self {
                src: Span::combine(start.clone().start(), end.clone().end()),
                lsb_bit,
                msb_bit,
            },
        )(input)
    }
}
#[derive(Clone, Debug)]
pub struct BitRangeLsbLen {
    pub src: Span,
    pub lsb_bit: NumberUnsigned,
    pub n_bits: NumberUnsigned,
}
impl BitRangeLsbLen {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            tuple((
                tag!("["),
                separated_pair(number, tag!(","), number),
                tag!("]"),
            )),
            |(start, ((lsb_bit, _), (n_bits, _)), end)| Self {
                src: Span::combine(start.clone().start(), end.clone().end()),
                lsb_bit,
                n_bits,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub enum Assertation {
    Define(Define),
    Attach(Attach),
    TableConstructor(Constructor),
    PcodeMacro(PcodeMacro),
    WithBlock(WithBlock),
}
impl Assertation {
    fn parse(
        input: &mut FilePreProcessor,
        buf: &mut Vec<Token>,
        inside_with_block: bool,
    ) -> Result<Option<Self>, SleighError> {
        assert!(buf.is_empty());
        let token = match input.parse() {
            Ok(Some(token)) => token,
            Ok(None) => return Ok(None),
            Err(e) => return Err(e.into()),
        };
        buf.push(token);
        let token_ref = &buf[0];
        let assertation = match &token_ref.token_type {
            //TODO make those reserved words into tokens?
            TokenType::Ident(x) if x == "define" => {
                input.parse_until(buf, |x| x.token_type == token_type!(";"))?;
                Define::parse(buf).map(Self::Define)?
            }
            TokenType::Ident(x) if x == "attach" => {
                input.parse_until(buf, |x| x.token_type == token_type!(";"))?;
                Attach::parse(buf).map(Self::Attach)?
            }
            TokenType::Ident(x) if x == "macro" => {
                input.parse_until(buf, |x| x.token_type == token_type!("}"))?;
                PcodeMacro::parse(buf).map(Self::PcodeMacro)?
            }
            TokenType::Ident(x) if x == "with" => {
                WithBlock::parse(input, buf).map(Self::WithBlock)?
            }
            TokenType::Ident(_table) => {
                Constructor::parse(input, buf).map(Self::TableConstructor)?
            }
            //instruction table
            TokenType::DubleDot => {
                let src = token_ref.location.clone();
                buf.clear();
                Constructor::parse_table(input, buf, "".to_string(), src)
                    .map(Self::TableConstructor)?
            }
            //close with_block
            TokenType::DeliCloseCurly if inside_with_block => return Ok(None),
            _ => {
                return Err(SleighError::StatementInvalid(
                    token_ref.location.clone(),
                ))
            }
        };
        Ok(Some(assertation))
    }
}

#[derive(Clone, Debug)]
pub struct Sleigh {
    pub assertations: Vec<Assertation>,
}
impl Sleigh {
    pub fn parse(
        input: &mut FilePreProcessor,
        buf: &mut Vec<Token>,
        inside_with_block: bool,
    ) -> Result<Self, nom::Err<SleighError>> {
        let mut assertations = vec![];
        loop {
            buf.clear();
            let Some(ass) = Assertation::parse(input, buf, inside_with_block)? else {
            break
        };
            assertations.push(ass);
        }
        Ok(Self { assertations })
    }
}
