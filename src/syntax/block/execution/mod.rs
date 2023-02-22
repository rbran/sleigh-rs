pub mod assignment;
pub mod branch;
pub mod export;
pub mod expr;
pub mod op;

use crate::preprocessor::token::Token;
use crate::syntax::block::execution::assignment::{
    Assignment, Declare, MemWrite,
};
use crate::syntax::parser::{ident, number, this_ident};
use crate::{NumberUnsigned, SleighError, Span};
use nom::branch::alt;
use nom::combinator::map;
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, pair, terminated};
use nom::IResult;

use self::branch::Branch;
use self::export::Export;

#[derive(Clone, Debug)]
pub enum Statement {
    Delayslot(Delayslot),
    Label(Label),
    Export(Export),
    Branch(Branch),
    Build(Build),
    Call(UserCall),
    Declare(Declare),
    Assignment(Assignment),
    MemWrite(MemWrite),
}

impl Statement {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        alt((
            //Move label out of statement?
            map(Label::parse, Self::Label),
            map(Delayslot::parse, Self::Delayslot),
            map(Export::parse, Self::Export),
            map(Branch::parse, Self::Branch),
            map(Build::parse, Self::Build),
            map(UserCall::parse_statement, Self::Call),
            map(Declare::parse, Self::Declare),
            map(Assignment::parse, Self::Assignment),
            map(MemWrite::parse, Self::MemWrite),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub struct Delayslot(pub NumberUnsigned);

impl Delayslot {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            terminated(
                delimited(
                    pair(this_ident("delayslot"), tag!("(")),
                    number,
                    tag!(")"),
                ),
                tag!(";"),
            ),
            |(value, _)| Self(value),
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Label {
    pub src: Span,
    pub name: String,
}

impl Label {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            delimited(tag!("<"), ident, tag!(">")),
            |(name, name_src)| Self {
                name,
                src: name_src.clone(),
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Build {
    pub src: Span,
    pub table_name: String,
}
impl Build {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            delimited(this_ident("build"), ident, tag!(";")),
            |(table_name, name_src)| Self {
                table_name,
                src: name_src.clone(),
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct UserCall {
    //top of the stack contains the call (op::Op)
    pub params: Vec<expr::Expr>,
    pub src: Span,
    pub name: String,
}

impl UserCall {
    pub fn new(function: String, src: Span, params: Vec<expr::Expr>) -> Self {
        Self {
            name: function,
            src,
            params,
        }
    }
    pub fn parse_statement(
        input: &[Token],
    ) -> IResult<&[Token], Self, SleighError> {
        terminated(UserCall::parse_expr, tag!(";"))(input)
    }
    pub fn parse_expr(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            pair(
                ident,
                delimited(
                    tag!("("),
                    separated_list0(tag!(","), expr::Expr::parse),
                    tag!(")"),
                ),
            ),
            |((name, name_src), params)| Self {
                params,
                src: name_src.clone(),
                name,
            },
        )(input)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Execution {
    pub statements: Vec<Statement>,
}

impl Execution {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(many0(Statement::parse), |statements| Self { statements })(input)
    }
}
