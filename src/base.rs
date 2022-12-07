use std::borrow::Borrow;

use nom::branch::alt;
use nom::bytes::complete::{is_a, tag, take_while, take_while1};
use nom::character::complete::{
    digit1, hex_digit1, line_ending, not_line_ending, space0,
};
use nom::combinator::{
    consumed, eof, map, map_res, opt, peek, recognize, value,
};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::{AsChar, IResult, InputIter, InputTake};

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

pub(crate) fn ident(input: &str) -> IResult<&str, &str> {
    let is_ident_first =
        |c: char| -> bool { c.is_ascii_alphabetic() || c == '_' || c == '.' };
    let is_ident =
        |c: char| -> bool { c.is_ascii_alphanumeric() || c == '_' || c == '.' };

    recognize(pair(take_while1(is_ident_first), take_while(is_ident)))(input)
}
pub(crate) fn ident_option<'a>(input: &str) -> IResult<&str, Option<&str>> {
    alt((value(None, tag("_")), map(ident, |x| Some(x))))(input)
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
//valid for X or "X"
pub(crate) fn string_inline<'a>(input: &str) -> IResult<&str, Option<String>> {
    alt((
        map(string, |x| Some(x)),
        map(ident_option, |x: Option<&str>| x.map(|x| x.to_string())),
    ))(input)
}

pub type IntTypeS = i64;
pub type IntTypeU = u64;
//pub(crate) type NonZeroTypeS = core::num::NonZeroI64;
pub type NonZeroTypeU = core::num::NonZeroU64;
pub type FloatType = f64;

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Number(&'a str, IntTypeU),
    Ident(&'a str),
}

impl<'a> Value<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(consumed(number_unsig), |(src, x)| Self::Number(src, x)),
            map(ident, |x| Self::Ident(x)),
        ))(input)
    }
}

fn number_sig_hex(input: &str) -> IResult<&str, IntTypeS> {
    map_res(
        separated_pair(opt(value((), tag("-"))), tag("0x"), hex_digit1),
        |(sig, x): (Option<()>, &str)| {
            IntTypeS::from_str_radix(x.borrow(), 16)
                .map(|x| x * if sig.is_some() { -1 } else { 1 })
        },
    )(input)
}
fn number_unsig_hex(input: &str) -> IResult<&str, IntTypeU> {
    map_res(preceded(tag("0x"), hex_digit1), |x: &str| {
        IntTypeU::from_str_radix(x.borrow(), 16)
    })(input)
}
fn number_sig_bin(input: &str) -> IResult<&str, IntTypeS> {
    map_res(
        separated_pair(
            opt(tag("-")),
            tag("0b"),
            take_while1(|c| matches!(c, '0' | '1')),
        ),
        |(sig, x)| {
            IntTypeS::from_str_radix(x, 2)
                .map(|x| x * if sig.is_some() { -1 } else { 1 })
        },
    )(input)
}
fn number_unsig_bin(input: &str) -> IResult<&str, IntTypeU> {
    map_res(
        preceded(tag("0b"), take_while1(|c| matches!(c, '0' | '1'))),
        |x| IntTypeU::from_str_radix(x, 2),
    )(input)
}
fn number_sig_dec(input: &str) -> IResult<&str, IntTypeS> {
    map_res(pair(opt(tag("-")), digit1), |(sig, x)| {
        IntTypeS::from_str_radix(x, 10)
            .map(|x| x * if sig.is_some() { -1 } else { 1 })
    })(input)
}
fn number_unsig_dec(input: &str) -> IResult<&str, IntTypeU> {
    map_res(digit1, |x| IntTypeU::from_str_radix(x, 10))(input)
}
pub(crate) fn number_sig(input: &str) -> IResult<&str, IntTypeS> {
    alt((number_sig_hex, number_sig_bin, number_sig_dec))(input)
}
pub(crate) fn number_unsig(input: &str) -> IResult<&str, IntTypeU> {
    alt((number_unsig_hex, number_unsig_bin, number_unsig_dec))(input)
}
pub(crate) fn number_sig_option(
    input: &str,
) -> IResult<&str, Option<IntTypeS>> {
    alt((map(number_sig, |x| Some(x)), value(None, tag("_"))))(input)
}
//pub(crate) fn number_unsig_option(
//    input: &str,
//) -> IResult<&str, Option<IntTypeU>> {
//    alt((map(number_unsig, |x| Some(x)), value(None, tag("_"))))(input)
//}

pub(crate) fn line_comment(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        tag("#"),
        not_line_ending,
        alt((peek(line_ending), eof)),
    )))(input)
}

pub(crate) const WS: &str = " \t\r\n\x0B";
pub(crate) fn empty_line(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        opt(space0),
        opt(pair(tag("#"), not_line_ending)),
        alt((line_ending, eof)),
    )))(input)
}

//match 1 or multiple empty spaces
pub(crate) fn empty_space1(input: &str) -> IResult<&str, &str> {
    recognize(many1(alt((line_comment, is_a(WS)))))(input)
}

//match 0 or multiple empty spaces, never fails
pub(crate) fn empty_space0(input: &str) -> IResult<&str, &str> {
    recognize(many0(alt((line_comment, is_a(WS)))))(input)
}

pub(crate) fn end_of_line(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        opt(space0),
        opt(line_comment),
        alt((eof, line_ending)),
    )))(input)
}

//pub(crate) fn until_space<'a, I, E>(input: I) -> IResult<I, I, E>
//where
//    I: InputTake
//        + Compare<&'static str>
//        + InputLength
//        + FindToken<char>
//        + InputIter<Item = char>
//        + InputTakeAtPosition<Item = char>
//        + Slice<Range<usize>>
//        + Slice<RangeFrom<usize>>
//        + Slice<RangeTo<usize>>
//        + Clone,
//    E: ParseError<I>,
//{
//    is_not(WS)(input)
//}

//fn list<'a, 'b, A, B, I, E, Output, T>(
//    element: A,
//) -> impl FnMut(I) -> IResult<I, Vec<Output>, E>
//where
//    I: InputHere<'a>,
//    E: ErrorHere<'a, I>,
//    A: Fn(I) -> IResult<I, Output, E>,
//{
//    delimited(
//        pair(tag("["), empty_space0),
//        separated_list1(empty_space1, element),
//        pair(empty_space0, tag("]")),
//    )
//}

pub(crate) fn fieldlist(input: &str) -> IResult<&str, Vec<&str>> {
    alt((
        map(ident, |x| vec![x]),
        delimited(
            pair(tag("["), empty_space0),
            separated_list1(empty_space1, ident),
            pair(empty_space0, tag("]")),
        ),
    ))(input)
}
pub(crate) fn registerlist(input: &str) -> IResult<&str, Vec<Option<&str>>> {
    alt((
        map(ident_option, |x| vec![x]),
        delimited(
            pair(tag("["), empty_space0),
            separated_list1(empty_space1, ident_option),
            pair(empty_space0, tag("]")),
        ),
    ))(input)
}

pub(crate) fn integerlist(input: &str) -> IResult<&str, Vec<Option<IntTypeS>>> {
    alt((
        map(number_sig_option, |x| vec![x]),
        delimited(
            pair(tag("["), empty_space0),
            separated_list1(empty_space1, number_sig_option),
            pair(empty_space0, tag("]")),
        ),
    ))(input)
}
pub(crate) fn stringlist(input: &str) -> IResult<&str, Vec<Option<String>>> {
    alt((
        map(string_inline, |x| vec![x]),
        delimited(
            pair(tag("["), empty_space0),
            separated_list1(empty_space1, string_inline),
            pair(empty_space0, tag("]")),
        ),
    ))(input)
}
