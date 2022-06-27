use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt, value};
use nom::multi::many0;
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::IResult;

use crate::base::{empty_space0, ident, Value};
pub use crate::semantic::disassembly::{Op, OpUnary};

impl OpUnary {
    fn parse(input: &str) -> IResult<&str, Self> {
        use OpUnary::*;
        alt((value(Negation, tag("~")), value(Negative, tag("-"))))(input)
    }
}
impl Op {
    fn parse(input: &str, safe: bool) -> IResult<&str, Self> {
        if safe {
            Self::parse_safe(input)
        } else {
            Self::parse_unsafe(input)
        }
    }
    fn parse_unsafe(input: &str) -> IResult<&str, Self> {
        alt((
            value(Self::Add, tag("+")),
            value(Self::Sub, tag("-")),
            value(Self::Mul, tag("*")),
            value(Self::Div, tag("/")),
            value(Self::Asr, tag(">>")),
            value(Self::Lsl, tag("<<")),
            value(Self::And, tag("$and")),
            value(Self::Or, tag("$or")),
            value(Self::Xor, tag("$xor")),
        ))(input)
    }
    fn parse_safe(input: &str) -> IResult<&str, Self> {
        alt((
            Self::parse_unsafe,
            value(Self::And, tag("&")),
            value(Self::Or, tag("|")),
            value(Self::Xor, tag("^")),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub struct Expr<'a> {
    pub rpn: Vec<ExprElement<'a>>,
}
#[derive(Clone, Debug)]
pub enum ExprElement<'a> {
    Value(Value<'a>),
    Op(Op),
    OpUnary(OpUnary),
}
impl<'a> IntoIterator for Expr<'a> {
    type Item = ExprElement<'a>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.rpn.into_iter()
    }
}
impl<'a> Expr<'a> {
    fn value(
        input: &'a str,
        stack: &'_ mut Vec<ExprElement<'a>>,
    ) -> IResult<&'a str, ()> {
        map(Value::parse, |x| stack.push(ExprElement::Value(x)))(input)
    }
    fn expr_rec(
        input: &'a str,
        stack: &'_ mut Vec<ExprElement<'a>>,
    ) -> IResult<&'a str, ()> {
        let (input, op) = opt(terminated(OpUnary::parse, empty_space0))(input)?;

        let input = if let Ok((input, _)) = Self::value(input, stack) {
            input
        } else {
            delimited(
                pair(tag("("), empty_space0),
                |x| Self::expr(x, stack, true),
                pair(empty_space0, tag(")")),
            )(input)?
            .0
        };
        if let Some(op) = op {
            stack.push(ExprElement::OpUnary(op));
        }
        Ok((input, ()))
    }
    fn op_field(
        input: &'a str,
        stack: &'_ mut Vec<ExprElement<'a>>,
        safe: bool,
    ) -> IResult<&'a str, ()> {
        let (input, op) = terminated(
            |x| Op::parse(x, safe),
            pair(empty_space0, |x| Self::expr_rec(x, stack)),
        )(input)?;
        stack.push(ExprElement::Op(op));
        Ok((input, ()))
    }
    //Expr is just a Vec with reverse polish notion of values, ops and unary ops
    fn expr(
        input: &'a str,
        stack: &'_ mut Vec<ExprElement<'a>>,
        safe: bool,
    ) -> IResult<&'a str, ()> {
        //TODO op precedence
        //first element is required
        let input = Self::expr_rec(input, stack)?.0;
        //then we get 0 or more Op Field after that.
        value(
            (),
            many0(preceded(empty_space0, |input| {
                Self::op_field(input, stack, safe)
            })),
        )(input)
    }
    pub fn parse(input: &'a str, safe: bool) -> IResult<&'a str, Self> {
        let mut rpn = vec![];
        let (input, _) = Self::expr(input, &mut rpn, safe)?;
        Ok((input, Self { rpn }))
    }
}

#[derive(Clone, Debug)]
pub struct GlobalSet<'a> {
    pub address: Value<'a>,
    pub context: &'a str,
}
impl<'a> GlobalSet<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            delimited(
                pair(tag("globalset("), empty_space0),
                separated_pair(
                    Value::parse,
                    tuple((empty_space0, tag(","), empty_space0)),
                    ident,
                ),
                tuple((empty_space0, tag(")"), empty_space0, tag(";"))),
            ),
            |(address, context)| Self { address, context },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Assignment<'a> {
    pub left: &'a str,
    pub right: Expr<'a>,
}

impl<'a> Assignment<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            terminated(
                separated_pair(
                    ident,
                    tuple((empty_space0, tag("="), empty_space0)),
                    |x| Expr::parse(x, true),
                ),
                pair(empty_space0, tag(";")),
            ),
            |(left, right)| Self { left, right },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub enum Assertation<'a> {
    GlobalSet(GlobalSet<'a>),
    Assignment(Assignment<'a>),
}

impl<'a> Assertation<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(GlobalSet::parse, Self::GlobalSet),
            map(Assignment::parse, Self::Assignment),
        ))(input)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Disassembly<'a> {
    pub assertations: Vec<Assertation<'a>>,
}

impl<'a> IntoIterator for Disassembly<'a> {
    type Item = Assertation<'a>;
    type IntoIter = std::vec::IntoIter<Assertation<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.assertations.into_iter()
    }
}

impl<'a> Disassembly<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            many0(preceded(empty_space0, Assertation::parse)),
            |assertations| Self { assertations },
        )(input)
    }
}

#[cfg(test)]
mod test {
    //    use nom::{combinator::eof, sequence::terminated};
    //
    //    use crate::processor::table::semantic::Value;
    //
    //    use crate::processor::table::dissasembly::{
    //        dissasembly_action, Assignment, DissasemblyParser, ExprElement, Op,
    //        OpUnary,
    //    };
    //
    //    #[test]
    //    fn test_dissassembly_action1() {
    //        use Assignment::*;
    //        use ExprElement as Expr;
    //        use Op::*;
    //        use OpUnary::*;
    //        use Value::*;
    //        let test =
    //            "A=B<< 1; c=(d);  e=(1+2); X=-y; W=~(1+(A>>2)); globalset(X,Y);globalset(A,B);";
    //        let (_, action) = terminated(dissasembly_action, eof)(test).unwrap();
    //        assert_eq!(
    //            DissasemblyParser {
    //                assignments: vec![
    //                    Local {
    //                        left: "A".into(),
    //                        right: vec![
    //                            Expr::Value(Var("B".into())),
    //                            Expr::Value(Int(1)),
    //                            Expr::Op(Lsl)
    //                        ],
    //                    },
    //                    Local {
    //                        left: "c".into(),
    //                        right: vec![Expr::Value(Var("d".into())),],
    //                    },
    //                    Local {
    //                        left: "e".into(),
    //                        right: vec![
    //                            Expr::Value(Int(1)),
    //                            Expr::Value(Int(2)),
    //                            Expr::Op(Add),
    //                        ],
    //                    },
    //                    Local {
    //                        left: "X".into(),
    //                        right: vec![
    //                            Expr::Value(Var("y".into())),
    //                            Expr::OpUnary(Negative),
    //                        ],
    //                    },
    //                    Local {
    //                        left: "W".into(),
    //                        right: vec![
    //                            Expr::Value(Int(1)),
    //                            Expr::Value(Value::Var("A".into())),
    //                            Expr::Value(Int(2)),
    //                            Expr::Op(Asr),
    //                            Expr::Op(Add),
    //                            Expr::OpUnary(Negation),
    //                        ],
    //                    },
    //                    Global("X".into(), "Y".into()),
    //                    Global("A".into(), "B".into()),
    //                ]
    //            },
    //            action,
    //        );
    //    }
}
