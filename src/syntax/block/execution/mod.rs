pub mod assignment;
pub mod branch;
pub mod export;
pub mod expr;
pub mod op;

use crate::syntax::block::execution::assignment::Assignment;
use crate::syntax::block::execution::assignment::Declare;
use crate::syntax::block::execution::assignment::MemWrite;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;

use crate::base::{empty_space0, ident, number_unsig, IntTypeU};

use self::branch::Branch;
use self::export::Export;

#[derive(Clone, Debug)]
pub enum Statement<'a> {
    Delayslot(Delayslot),
    Label(Label<'a>),
    Export(Export<'a>),
    Branch(Branch<'a>),
    Build(Build<'a>),
    Call(UserCall<'a>),
    Declare(Declare<'a>),
    Assignment(Assignment<'a>),
    MemWrite(MemWrite<'a>),
}

impl<'a> Statement<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
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
pub struct Delayslot(pub IntTypeU);

impl Delayslot {
    fn parse(input: &str) -> IResult<&str, Self> {
        map(
            terminated(
                delimited(
                    pair(tag("delayslot("), empty_space0),
                    number_unsig,
                    pair(empty_space0, tag(")")),
                ),
                pair(empty_space0, tag(";")),
            ),
            |value| Self(value),
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Label<'a> {
    pub name: &'a str,
}

impl<'a> Label<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            delimited(
                pair(tag("<"), empty_space0),
                ident,
                pair(empty_space0, tag(">")),
            ),
            |name| Self { name },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Build<'a> {
    pub table: &'a str,
}
impl<'a> Build<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            delimited(
                pair(tag("build"), empty_space0),
                ident,
                pair(empty_space0, tag(";")),
            ),
            |table| Self { table },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct UserCall<'a> {
    //top of the stack contains the call (op::Op)
    pub params: Vec<expr::Expr<'a>>,
    pub function: &'a str,
}

impl<'a> UserCall<'a> {
    pub fn new(function: &'a str, params: Vec<expr::Expr<'a>>) -> Self {
        Self { function, params }
    }
    pub fn parse_statement(input: &'a str) -> IResult<&'a str, Self> {
        terminated(UserCall::parse_expr, pair(empty_space0, tag(";")))(input)
    }
    pub fn parse_expr(input: &'a str) -> IResult<&'a str, Self> {
        map(
            pair(
                ident,
                delimited(
                    tuple((empty_space0, tag("("), empty_space0)),
                    separated_list0(
                        tuple((empty_space0, tag(","), empty_space0)),
                        expr::Expr::parse,
                    ),
                    pair(empty_space0, tag(")")),
                ),
            ),
            |(function, params)| Self { params, function },
        )(input)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Execution<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Execution<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            many0(preceded(empty_space0, Statement::parse)),
            |statements| Self { statements },
        )(input)
    }
}

#[cfg(test)]
mod test {
    //    use nom::{combinator::eof, sequence::terminated};
    //
    //    use crate::processor::table::semantic::{
    //        parse_semantic, Assignment, ByteRange, Expr, ExprElement, LeftElement,
    //        OpBinary, OpLeft, OpUnary, SemanticParser, Statement, Value,
    //    };
    //
    //    #[test]
    //    fn test_assignment1() {
    //        use Statement::*;
    //        let test = "local tmp:4 = (value >> 31); tmp2 = tmp(0);";
    //        let (_, semantic) = terminated(parse_semantic, eof)(test).unwrap();
    //        assert_eq!(
    //            semantic,
    //            SemanticParser {
    //                statements: vec![
    //                    VarSet(Assignment {
    //                        local: true,
    //                        left: LeftElement {
    //                            name: "tmp".into(),
    //                            op: Some(OpLeft::ByteRange(ByteRange {
    //                                lsb: true,
    //                                n_bytes: 4,
    //                            }))
    //                        },
    //                        right: Some(Expr {
    //                            rpn: vec![
    //                                ExprElement::Value(Value::Var("value".into())),
    //                                ExprElement::Value(Value::Int(31)),
    //                                ExprElement::OpBinary(OpBinary::Lsr)
    //                            ]
    //                        }),
    //                    }),
    //                    VarSet(Assignment {
    //                        local: false,
    //                        left: LeftElement {
    //                            name: "tmp2".into(),
    //                            op: None
    //                        },
    //                        right: Some(Expr {
    //                            rpn: vec![
    //                                ExprElement::Value(Value::Var("tmp".into())),
    //                                ExprElement::OpUnary(OpUnary::ByteRange(
    //                                    ByteRange {
    //                                        lsb: false,
    //                                        n_bytes: 0,
    //                                    }
    //                                )),
    //                            ]
    //                        }),
    //                    })
    //                ]
    //            }
    //        );
    //    }
}
