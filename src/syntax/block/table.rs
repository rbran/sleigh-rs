use crate::preprocessor::token::Token;
use crate::preprocessor::FilePreProcessor;
use crate::syntax::parser::{ident, Parser};
use crate::{SleighError, Span, DISABLE_EXECUTION_PARSING};

use nom::branch::alt;
use nom::combinator::{cut, eof, map, opt, value};
use nom::sequence::{delimited, tuple};

use super::disassembly::Disassembly;
use super::display::Display;
use super::execution::Execution;
use super::pattern::Pattern;

#[derive(Clone, Debug)]
pub struct Constructor {
    pub src: Span,
    pub table_name: String,
    pub display: Display,
    pub pattern: Pattern,
    pub disassembly: Option<Disassembly>,
    pub execution: Option<Execution>,
}

impl Constructor {
    pub fn table_name(&self) -> &str {
        &self.table_name
    }
    pub fn parse(
        input: &mut FilePreProcessor,
        buf: &mut Vec<Token>,
    ) -> Result<Self, SleighError> {
        input.parse_until(buf, |x| &x.token_type == &token_type!(":"))?;
        let (_eof, ((table_name, table_src), _, _)) =
            tuple((ident, tag!(":"), eof))(buf)?;
        let table_src = table_src.clone();

        buf.clear();
        Self::parse_table(input, buf, table_name, table_src)
    }

    pub fn parse_table(
        input: &mut FilePreProcessor,
        buf: &mut Vec<Token>,
        table_name: String,
        src: Span,
    ) -> Result<Self, SleighError> {
        let display = Display::parse(input)?;

        input.parse_until(buf, |token| {
            matches!(
                &token.token_type,
                token_type!("}") | token_type!("unimpl")
            )
        })?;
        let (_eof, (pattern, disassembly, execution)) = tuple((
            cut(Pattern::parse),
            //disassembly is optional
            opt(delimited(tag!("["), Disassembly::parse, tag!("]"))),
            //semantic could be empty or unimplemented (None)
            cut(alt((
                value(None, tag!("unimpl")),
                delimited(
                    tag!("{"),
                    map(Execution::parse, |x| Some(x)),
                    tag!("}"),
                ),
            ))),
        ))(buf)?;

        let execution = if DISABLE_EXECUTION_PARSING {
            None
        } else {
            execution
        };
        Ok(Self {
            src,
            table_name,
            display,
            pattern,
            execution,
            disassembly,
        })
    }
}
