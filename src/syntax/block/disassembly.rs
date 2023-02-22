use nom::branch::alt;
use nom::combinator::{map, opt, value};
use nom::multi::many0;
use nom::sequence::{separated_pair, terminated, tuple};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::semantic::disassembly::{Op, OpUnary};
use crate::syntax::parser::{ident, this_ident};
use crate::syntax::Value;
use crate::{SleighError, Span};

impl OpUnary {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        use OpUnary::*;
        alt((value(Negation, tag!("~")), value(Negative, tag!("-"))))(input)
    }
}
impl Op {
    fn parse(
        input: &[Token],
        safe: bool,
    ) -> IResult<&[Token], Self, SleighError> {
        if safe {
            Self::parse_safe(input)
        } else {
            Self::parse_unsafe(input)
        }
    }
    fn parse_unsafe(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        alt((
            value(Self::Add, tag!("+")),
            value(Self::Sub, tag!("-")),
            value(Self::Mul, tag!("*")),
            value(Self::Div, tag!("/")),
            value(Self::Asr, tag!(">>")),
            value(Self::Lsl, tag!("<<")),
            value(Self::And, tag!("$and")),
            value(Self::Or, tag!("$or")),
            value(Self::Xor, tag!("$xor")),
        ))(input)
    }
    fn parse_safe(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        alt((
            Self::parse_unsafe,
            value(Self::And, tag!("&")),
            value(Self::Or, tag!("|")),
            value(Self::Xor, tag!("^")),
        ))(input)
    }
}

#[derive(Debug)]
enum InnerExprOperators<'a> {
    Op(Op),
    OpUnary(OpUnary),
    OpenParenthesis(&'a Span),
}

#[derive(Default, Debug)]
struct InnerExpr<'a> {
    output: Vec<ExprElement>,
    operators: Vec<InnerExprOperators<'a>>,
}

impl<'a> InnerExpr<'a> {
    fn open_parenthesis(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], (), SleighError> {
        let (input, span) = tag!("(")(input)?;
        //open parentessis goes on top of all ops
        self.operators
            .push(InnerExprOperators::OpenParenthesis(span));
        Ok((input, ()))
    }
    fn close_parenthesis(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], (), SleighError> {
        let (input, location) = tag!(")")(input)?;
        //pop until we find the open parenthesis
        loop {
            let op = self.operators.pop().ok_or_else(|| {
                SleighError::StatementInvalid(location.clone())
            })?;
            match op {
                InnerExprOperators::OpenParenthesis(_span) => break,
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
    fn parse_op_unary(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], (), SleighError> {
        map(opt(OpUnary::parse), |op| {
            if let Some(op) = op {
                self.operators.push(InnerExprOperators::OpUnary(op));
            }
            ()
        })(input)
    }
    fn parse_op(
        &mut self,
        input: &'a [Token],
        safe: bool,
    ) -> IResult<&'a [Token], (), SleighError> {
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
                open @ InnerExprOperators::OpenParenthesis(_) => {
                    self.operators.push(open);
                    break;
                }
            }
        }
        self.operators.push(InnerExprOperators::Op(op));

        let (input, _) = self.expr_rec(input)?;
        Ok((input, ()))
    }
    fn value(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], (), SleighError> {
        map(Value::parse_signed, |x| self.output.push(ExprElement::Value(x)))(input)
    }
    fn expr_rec(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], (), SleighError> {
        let (input, ()) = self.parse_op_unary(input).unwrap();

        let input = if let Ok((input, _span)) = self.value(input) {
            input
        } else {
            let (input, ()) = self.open_parenthesis(input)?;
            let (input, ()) = self.expr(input, true)?;
            let (input, ()) = self.close_parenthesis(input)?;
            input
        };
        Ok((input, ()))
    }
    fn expr(
        &mut self,
        input: &'a [Token],
        safe: bool,
    ) -> IResult<&'a [Token], (), SleighError> {
        //first element is required
        let (input, _) = self.expr_rec(input)?;
        //then we get 0 or more Op Field after that.
        value((), many0(|input| self.parse_op(input, safe)))(input)
    }
    fn parse(
        input: &'a [Token],
        safe: bool,
    ) -> IResult<&'a [Token], Vec<ExprElement>, SleighError> {
        let mut rpn = Self::default();
        let (input, _) = rpn.expr(input, safe)?;
        //pop all operators into the output
        for ele in rpn.operators.into_iter().rev() {
            let ele = match ele {
                InnerExprOperators::Op(op) => ExprElement::Op(op),
                InnerExprOperators::OpUnary(op) => ExprElement::OpUnary(op),
                InnerExprOperators::OpenParenthesis(span) => {
                    return Err(nom::Err::Error(SleighError::StatementInvalid(
                        span.clone(),
                    )))
                }
            };
            rpn.output.push(ele);
        }
        Ok((input, rpn.output))
    }
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub rpn: Vec<ExprElement>,
}
#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(Value),
    Op(Op),
    OpUnary(OpUnary),
}
impl IntoIterator for Expr {
    type Item = ExprElement;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.rpn.into_iter()
    }
}
impl Expr {
    pub fn parse(
        input: &[Token],
        safe: bool,
    ) -> IResult<&[Token], Self, SleighError> {
        let (input, inner_expr) = InnerExpr::parse(input, safe)?;
        Ok((input, Self { rpn: inner_expr }))
    }
}

#[derive(Clone, Debug)]
pub struct GlobalSet {
    pub src: Span,
    pub address: Value,
    pub context: String,
}
impl GlobalSet {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            tuple((
                terminated(this_ident("globalset"), tag!("(")),
                terminated(Value::parse_unsigned, tag!(",")),
                terminated(ident, tag!(")")),
                tag!(";"),
            )),
            |(start, address, (context, _), end)| Self {
                src: Span::combine(start.clone().start(), end.clone().end()),
                address,
                context,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub left_span: Span,
    pub left: String,
    pub right: Expr,
}

impl Assignment {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            terminated(
                separated_pair(ident, tag!("="), |x| Expr::parse(x, true)),
                tag!(";"),
            ),
            |((left, left_span), right)| Self {
                left_span: left_span.clone(),
                left,
                right,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub enum Assertation {
    GlobalSet(GlobalSet),
    Assignment(Assignment),
}

impl Assertation {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        alt((
            map(GlobalSet::parse, Self::GlobalSet),
            map(Assignment::parse, Self::Assignment),
        ))(input)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Disassembly {
    pub assertations: Vec<Assertation>,
}

impl IntoIterator for Disassembly {
    type Item = Assertation;
    type IntoIter = std::vec::IntoIter<Assertation>;

    fn into_iter(self) -> Self::IntoIter {
        self.assertations.into_iter()
    }
}

impl Disassembly {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(many0(Assertation::parse), |assertations| Self {
            assertations,
        })(input)
    }
}
