use nom::combinator::eof;
use nom::multi::separated_list0;
use nom::sequence::{delimited, terminated, tuple};

use crate::preprocessor::token::Token;
use crate::syntax::parser::{ident, this_ident};
use crate::{SleighError, Span};

use super::execution::Execution;

//TODO impl correctly the PCode Macro
#[derive(Clone, Debug)]
pub struct PcodeMacro {
    pub src: Span,
    pub name: String,
    pub params: Vec<(String, Span)>,
    pub body: Execution,
}

impl PcodeMacro {
    pub fn parse(input: &[Token]) -> Result<Self, Box<SleighError>> {
        let (_eof, (_, (name, src), params, body)) = terminated(
            tuple((
                this_ident("macro"),
                ident,
                delimited(
                    tag!("("),
                    separated_list0(tag!(","), ident),
                    tag!(")"),
                ),
                delimited(tag!("{"), Execution::parse, tag!("}")),
            )),
            eof,
        )(input)?;
        Ok(Self {
            name,
            params: params.into_iter().map(|(x, s)| (x, s.clone())).collect(),
            body,
            src: src.clone(),
        })
    }
}
