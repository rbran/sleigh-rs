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

#[derive(Debug)]
enum InnerExprOperators {
    Op(Op),
    OpUnary(OpUnary),
    OpenParenthesis,
}

#[derive(Default, Debug)]
struct InnerExpr<'a> {
    output: Vec<ExprElement<'a>>,
    operators: Vec<InnerExprOperators>,
}

impl<'a> InnerExpr<'a> {
    fn open_parenthesis(&mut self, input: &'a str) -> IResult<&'a str, ()> {
        let (input, _) = pair(tag("("), empty_space0)(input)?;
        //open parentessis goes on top of all ops
        self.operators.push(InnerExprOperators::OpenParenthesis);
        Ok((input, ()))
    }
    fn close_parenthesis(&mut self, input: &'a str) -> IResult<&'a str, ()> {
        let (input, _) = pair(empty_space0, tag(")"))(input)?;
        //pop until we find the open parenthesis
        loop {
            let op = self.operators.pop().ok_or_else(|| {
                nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Fix,
                ))
            })?;
            match op {
                InnerExprOperators::OpenParenthesis => break,
                InnerExprOperators::Op(op) => {
                    self.output.push(ExprElement::Op(op))
                }
                InnerExprOperators::OpUnary(op) => {
                    self.output.push(ExprElement::OpUnary(op))
                }
            }
        }
        Ok((input, ()))
    }
    fn parse_op_unary(&mut self, input: &'a str) -> IResult<&'a str, ()> {
        map(opt(terminated(OpUnary::parse, empty_space0)), |op| {
            if let Some(op) = op {
                self.operators.push(InnerExprOperators::OpUnary(op));
            }
            ()
        })(input)
    }
    fn parse_op(&mut self, input: &'a str, safe: bool) -> IResult<&'a str, ()> {
        let (input, op) = Op::parse(input, safe)?;
        //unstack the operation until it is able to put this op to the stack
        loop {
            let Some(last_op) = self.operators.pop() else {
                break
            };
            match last_op {
                //only if the op have a smaller precedence that this one
                InnerExprOperators::Op(last_op) => {
                    if last_op < op {
                        //stack on top of this one
                        self.operators.push(InnerExprOperators::Op(last_op));
                        break;
                    } else {
                        //remove this op from the op stack
                        self.output.push(ExprElement::Op(last_op));
                    }
                }
                //we can never stack on top of unary, unstack it
                InnerExprOperators::OpUnary(op) => {
                    self.output.push(ExprElement::OpUnary(op));
                }
                //we can always stack on top of parenthesis
                InnerExprOperators::OpenParenthesis => {
                    self.operators.push(InnerExprOperators::OpenParenthesis);
                    break;
                }
            }
        }
        self.operators.push(InnerExprOperators::Op(op));

        let (input, _) = pair(empty_space0, |x| self.expr_rec(x))(input)?;
        Ok((input, ()))
    }
    fn value(&mut self, input: &'a str) -> IResult<&'a str, ()> {
        map(Value::parse, |x| self.output.push(ExprElement::Value(x)))(input)
    }
    fn expr_rec(&mut self, input: &'a str) -> IResult<&'a str, ()> {
        let (input, ()) = self.parse_op_unary(input).unwrap();

        let input = if let Ok((input, _)) = self.value(input) {
            input
        } else {
            let (input, ()) = self.open_parenthesis(input)?;
            let (input, ()) = self.expr(input, true)?;
            let (input, ()) = self.close_parenthesis(input)?;
            input
        };
        Ok((input, ()))
    }
    fn expr(&mut self, input: &'a str, safe: bool) -> IResult<&'a str, ()> {
        //first element is required
        let (input, _) = self.expr_rec(input)?;
        //then we get 0 or more Op Field after that.
        value(
            (),
            many0(preceded(empty_space0, |input| self.parse_op(input, safe))),
        )(input)
    }
    fn parse(
        input: &'a str,
        safe: bool,
    ) -> IResult<&'a str, Vec<ExprElement<'a>>> {
        let mut rpn = Self::default();
        let (input, _) = rpn.expr(input, safe)?;
        for ele in rpn.operators.into_iter() {
            let ele = match ele {
                InnerExprOperators::Op(op) => ExprElement::Op(op),
                InnerExprOperators::OpUnary(op) => ExprElement::OpUnary(op),
                InnerExprOperators::OpenParenthesis => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Fix,
                    )))
                }
            };
            rpn.output.push(ele);
        }
        Ok((input, rpn.output))
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
    pub fn parse(input: &'a str, safe: bool) -> IResult<&'a str, Self> {
        let (input, inner_expr) = InnerExpr::parse(input, safe)?;
        Ok((input, Self { rpn: inner_expr }))
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
