mod alignment;
mod bitrange;
mod context;
mod space;
mod token;
mod user_function;
mod varnode;

use nom::branch::alt;
use nom::combinator::{cut, eof, map, value};
use nom::sequence::{pair, preceded, terminated};
use nom::IResult;

use crate::{Endian, SleighError};

use crate::preprocessor::token::Token as ParserToken;

pub use self::alignment::Alignment;
pub use self::bitrange::BitRangeDef;
pub use self::context::{Context, ContextFieldAttribute};
pub use self::space::{Attribute, Space};
pub use self::token::{Token, TokenFieldAttribute};
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

pub fn parse_endian(
    input: &[ParserToken],
) -> IResult<&[ParserToken], Endian, Box<SleighError>> {
    preceded(
        pair(this_ident("endian"), tag!("=")),
        cut(alt((
            value(Endian::Little, this_ident("little")),
            value(Endian::Big, this_ident("big")),
        ))),
    )(input)
}

impl Define {
    pub fn parse(input: &[ParserToken]) -> Result<Define, Box<SleighError>> {
        let (_eof, define) = preceded(
            this_ident("define"),
            cut(terminated(
                alt((
                    map(parse_endian, Define::Endian),
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
