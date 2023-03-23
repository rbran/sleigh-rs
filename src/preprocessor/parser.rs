use std::borrow::Borrow;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::complete::{digit1, hex_digit1, not_line_ending, space0};
use nom::combinator::{eof, map, map_res, opt, recognize, value};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::{AsChar, IResult, InputIter, InputTake};

use crate::NumberUnsigned;

#[macro_export]
macro_rules! parse_or_else {
    ( , ) => {
        compile_error!();
    };
    ($parser:expr, $input:expr $(,)?) => {
        match $parser($input) {
            Ok((input, value)) => {
                $input = input;
                Some(value)
            }
            Err(x @ nom::Err::Failure(_)) => return Err(x),
            Err(nom::Err::Error(_)) => None,
            Err(nom::Err::Incomplete(_)) => unreachable!(),
        }
    };
}

pub fn is_ident_first(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || c == '.'
}

pub fn is_ident_middle(c: char) -> bool {
    is_ident_first(c) || c.is_ascii_digit()
}

pub(crate) fn ident(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        take_while1(is_ident_first),
        take_while(is_ident_middle),
    ))(input)
}
//TODO octal, hex and unicode code escape
fn in_quotes<'a>(buf: &str) -> IResult<&str, String> {
    let mut ret = String::new();
    let mut skip_delimiter = false;
    for (i, ch) in buf.iter_indices() {
        //TODO expand \n, \t, etc?
        let ch = ch.as_char();
        if ch == '\\' && !skip_delimiter {
            skip_delimiter = true;
        } else if ch == '"' && !skip_delimiter {
            return Ok((buf.take_split(i).0, ret));
        } else {
            ret.push(ch.as_char());
            skip_delimiter = false;
        }
    }
    Err(nom::Err::Error(nom::error::Error {
        input: buf,
        code: nom::error::ErrorKind::Fix,
    }))
}
pub(crate) fn string(input: &str) -> IResult<&str, String> {
    delimited(tag("\""), in_quotes, tag("\""))(input)
}

fn number_unsig_hex(input: &str) -> IResult<&str, NumberUnsigned> {
    map_res(preceded(tag("0x"), hex_digit1), |x: &str| {
        NumberUnsigned::from_str_radix(x.borrow(), 16)
    })(input)
}
fn number_unsig_bin(input: &str) -> IResult<&str, NumberUnsigned> {
    map_res(
        preceded(tag("0b"), take_while1(|c| matches!(c, '0' | '1'))),
        |x| NumberUnsigned::from_str_radix(x, 2),
    )(input)
}
fn number_unsig_dec(input: &str) -> IResult<&str, NumberUnsigned> {
    map_res(digit1, |x| NumberUnsigned::from_str_radix(x, 10))(input)
}
pub fn number(input: &str) -> IResult<&str, NumberUnsigned> {
    alt((number_unsig_hex, number_unsig_bin, number_unsig_dec))(input)
}

pub(crate) fn comment(input: &str) -> IResult<&str, &str> {
    recognize(pair(tag("#"), not_line_ending))(input)
}

//TODO delete the others and rename this
pub(crate) fn empty_space0(input: &str) -> IResult<&str, &str> {
    recognize(pair(space0, opt(comment)))(input)
}

pub(crate) fn end_of_line(input: &str) -> IResult<&str, &str> {
    recognize(tuple((opt(space0), opt(comment))))(input)
}

#[derive(Clone, Debug)]
pub enum Display {
    End,
    Concat,
    Ident(String),
    Literal(String),
}
//Never fails, IResult only for convenience
pub fn display_token(input: &str) -> IResult<&str, Option<Display>> {
    alt((
        value(Some(Display::Concat), tag("^")),
        map(string, |string| Some(Display::Literal(string))),
        map(ident, |ident| match ident {
            "is" => Some(Display::End),
            x => Some(Display::Ident(x.to_owned())),
        }),
        //if something else, just consume it until a possible ident is found
        map(
            take_while1(|c| c != '^' && c != '"' && !is_ident_first(c)),
            |x: &str| {
                assert!(!x.is_empty());
                Some(Display::Literal(x.to_owned()))
            },
        ),
        value(None, eof),
    ))(input)
}
