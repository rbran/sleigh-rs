use nom::combinator::{eof, opt};
use nom::sequence::{delimited, pair, terminated, tuple};

use crate::preprocessor::token::Token;
use crate::preprocessor::FilePreProcessor;
use crate::syntax::parser::{ident, this_ident, Parser};
use crate::syntax::Sleigh;
use crate::{SleighError, IDENT_INSTRUCTION};

use super::disassembly::Disassembly;
use super::pattern::Pattern;

#[derive(Clone, Debug)]
pub struct WithBlock {
    table_name: Option<String>,
    pub pattern: Pattern,
    pub disassembly: Option<Disassembly>,
    pub body: Sleigh,
}

impl WithBlock {
    pub fn table_name(&self) -> &str {
        self.table_name
            .as_ref()
            .map(String::as_str)
            .unwrap_or(IDENT_INSTRUCTION)
    }
    pub fn parse(
        input: &mut FilePreProcessor,
        buf: &mut Vec<Token>,
    ) -> Result<Self, SleighError> {
        input.parse_until(buf, |x| &x.token_type == &token_type!("{"))?;
        let (_eof, (table, pattern, disassembly)) = terminated(
            tuple((
                delimited(this_ident("with"), opt(ident), tag!(":")),
                Pattern::parse,
                opt(delimited(tag!("["), Disassembly::parse, tag!("]"))),
            )),
            pair(tag!("{"), eof),
        )(buf)?;
        let table_name = table.map(|(name, _span)| name);

        //parse assertations until find a lonely }
        buf.clear();
        let body = Sleigh::parse(input, buf, true)?;

        Ok(Self {
            table_name,
            pattern,
            disassembly,
            body,
        })
    }
}
