mod alignment;
mod bitrange;
mod context;
mod endian;
mod space;
mod token;
mod user_function;
mod varnode;

use nom::branch::alt;
use nom::combinator::{cut, eof, map};
use nom::sequence::{pair, preceded, terminated};

use crate::{Endian, SleighError};

use crate::preprocessor::token::Token as ParserToken;

pub use self::alignment::Alignment;
pub use self::bitrange::{BitRangeDef, VarnodeField};
pub use self::context::{Context, ContextField, ContextFieldAttribute};
pub use self::space::{Attribute, Space};
pub use self::token::{Token, TokenField, TokenFieldAttribute};
pub use self::user_function::UserFunction;
pub use self::varnode::Varnode;

use super::parser::this_ident;

#[derive(Clone, Debug)]
pub enum Define {
    Endian(Endian),
    Alignment(Alignment),
    Varnode(Varnode),
    Bitrange(BitRangeDef),
    Token(Token),
    UserFunction(UserFunction),
    Context(Context),
    Space(Space),
}

impl Define {
    pub fn parse(input: &[ParserToken]) -> Result<Define, SleighError> {
        let (_eof, define) = preceded(
            this_ident("define"),
            cut(terminated(
                alt((
                    map(Endian::parse, Define::Endian),
                    map(Alignment::parse, Define::Alignment),
                    map(UserFunction::parse, Define::UserFunction),
                    map(Space::parse, Define::Space),
                    map(Token::parse, Define::Token),
                    map(BitRangeDef::parse, Define::Bitrange),
                    map(Context::parse, Define::Context),
                    //NOTE Varnode need to be the last
                    map(Varnode::parse, Define::Varnode),
                )),
                pair(tag!(";"), eof),
            )),
        )(input)?;
        Ok(define)
    }
}
