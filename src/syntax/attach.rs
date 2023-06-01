use nom::branch::alt;
use nom::combinator::{cut, eof, map};
use nom::sequence::{pair, preceded, terminated};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::{Number, SleighError, Span};

use super::parser::{
    fieldlist, numberlist, registerlist, stringlist, this_ident,
};

#[derive(Clone, Debug)]
pub struct Attach {
    pub src: Span,
    pub fields: Vec<(String, Span)>,
    pub meaning: Meaning,
}

#[derive(Clone, Debug)]
pub enum Meaning {
    Variable(Vec<(Option<String>, Span)>),
    Name(Vec<(Option<String>, Span)>),
    Number(Vec<(Option<Number>, Span)>),
}
impl Attach {
    pub fn parse(input: &[Token]) -> Result<Attach, SleighError> {
        let (_eof, (src, (fields, meaning))) = pair(
            this_ident("attach"),
            cut(terminated(
                alt((
                    Attach::attach_lists(
                        "variables",
                        map(registerlist, Meaning::Variable),
                    ),
                    Attach::attach_lists(
                        "names",
                        map(stringlist, Meaning::Name),
                    ),
                    Attach::attach_lists(
                        "values",
                        map(numberlist, Meaning::Number),
                    ),
                )),
                pair(tag!(";"), eof),
            )),
        )(input)?;
        Ok(Attach {
            src: src.clone(),
            fields,
            meaning,
        })
    }
    fn attach_lists<'a, Output>(
        name: &'a str,
        element: impl FnMut(
            &'a [Token],
        ) -> IResult<&'a [Token], Output, SleighError>,
    ) -> impl FnMut(
        &'a [Token],
    ) -> IResult<
        &'a [Token],
        (Vec<(String, Span)>, Output),
        SleighError,
    > {
        preceded(this_ident(name), pair(fieldlist, element))
    }
}
