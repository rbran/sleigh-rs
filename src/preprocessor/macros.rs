use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{map, opt, recognize, value};
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;

use super::ifs::{if_cond, IfCheckOwned};
use super::parser::{end_of_line, ident, number, string};
use super::{
    MACRO_DEFINE, MACRO_ELIF, MACRO_ELSE, MACRO_ENDIF, MACRO_IF, MACRO_IFDEF,
    MACRO_IFNDEF, MACRO_INCLUDE, MACRO_UNDEFINE,
};

pub(crate) fn expansion(input: &str) -> IResult<&str, &str> {
    delimited(pair(tag("$("), space0), ident, pair(space0, tag(")")))(input)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DefineDataOwned {
    Alias(String),
    Value(String),
}
impl DefineDataOwned {
    fn parse(input: &str) -> IResult<&str, Self> {
        alt((
            map(ident, |ident| Self::Alias(ident.to_owned())),
            //TODO number to string? What about a Number type???
            map(
                alt((string, map(recognize(number), str::to_string))),
                Self::Value,
            ),
        ))(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MacroLine {
    Define {
        name: String,
        value: Option<DefineDataOwned>,
    },
    Undefine(String),
    Include(String),
    If(IfCheckOwned),
    ElIf(IfCheckOwned),
    Else,
    EndIf,
}
impl MacroLine {
    pub fn parse_define(input: &str) -> IResult<&str, Self> {
        map(
            delimited(
                pair(tag(MACRO_DEFINE), space1),
                pair(ident, opt(preceded(space1, DefineDataOwned::parse))),
                end_of_line,
            ),
            |(name, value)| Self::Define {
                name: name.to_owned(),
                value,
            },
        )(input)
    }
    fn parse_undefine(input: &str) -> IResult<&str, Self> {
        map(
            delimited(pair(tag(MACRO_UNDEFINE), space1), ident, end_of_line),
            |x| Self::Undefine(x.to_owned()),
        )(input)
    }
    fn parse_include(input: &str) -> IResult<&str, Self> {
        map(
            delimited(pair(tag(MACRO_INCLUDE), space0), string, end_of_line),
            |x| Self::Include(x.to_owned()),
        )(input)
    }
    fn parse_if(input: &str) -> IResult<&str, Self> {
        map(preceded(pair(tag(MACRO_IF), space1), if_cond), Self::If)(input)
    }
    fn parse_ifdef(input: &str) -> IResult<&str, Self> {
        map(
            delimited(pair(tag(MACRO_IFDEF), space1), ident, end_of_line),
            |x| Self::If(IfCheckOwned::Defined(x.to_owned())),
        )(input)
    }
    fn parse_ifndef(input: &str) -> IResult<&str, Self> {
        map(
            delimited(pair(tag(MACRO_IFNDEF), space1), ident, end_of_line),
            |x| Self::If(IfCheckOwned::NotDefined(x.to_owned())),
        )(input)
    }
    fn parse_elif(input: &str) -> IResult<&str, Self> {
        map(preceded(pair(tag(MACRO_ELIF), space1), if_cond), Self::ElIf)(input)
    }
    fn parse_else(input: &str) -> IResult<&str, Self> {
        value(Self::Else, pair(tag(MACRO_ELSE), end_of_line))(input)
    }
    fn parse_endif(input: &str) -> IResult<&str, Self> {
        value(Self::EndIf, pair(tag(MACRO_ENDIF), end_of_line))(input)
    }
    pub fn parse(input: &str) -> IResult<&str, Self> {
        preceded(
            space0,
            alt((
                Self::parse_define,
                Self::parse_undefine,
                Self::parse_include,
                Self::parse_if,
                Self::parse_ifdef,
                Self::parse_ifndef,
                Self::parse_elif,
                Self::parse_else,
                Self::parse_endif,
            )),
        )(input)
    }
}
