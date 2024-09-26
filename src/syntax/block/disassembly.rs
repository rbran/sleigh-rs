use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, pair, separated_pair, terminated, tuple};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::semantic::disassembly::{Op, OpUnary};
use crate::syntax::parser::{ident, this_ident};
use crate::syntax::Value;
use crate::{SleighError, Span};

#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(Span, Op, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(Value),
    Op(Span, OpUnary, Box<Expr>),
}

macro_rules! op_parser {
    ($name:ident, $op:ident, $tag:tt) => {
        pub fn $name(
            input: &[Token],
        ) -> IResult<&[Token], (Self, &Span), Box<SleighError>> {
            map(tag!($tag), |span| (Self::$op, span))(input)
        }
    };
}

//used to define precedence
macro_rules! op_parser_levels {
    ($level:ident, $(($name:ident, $op:ident, $tag:tt)),* $(,)? ) => {
        $(op_parser!($name, $op, $tag);)*
        pub fn $level(input: &[Token]) -> IResult<&[Token], (Self, &Span), Box<SleighError>> {
            alt(($(Self::$name),*))(input)
        }
    };
}

macro_rules! declare_expr_level {
    ($name:ident, $next:ident, $op:path) => {
        fn $name(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
            Self::parse_op(Self::$name, Self::$next, $op)(input)
        }
    };
}

impl OpUnary {
    fn parse(
        input: &[Token],
    ) -> IResult<&[Token], (Self, &Span), Box<SleighError>> {
        use OpUnary::*;
        alt((
            map(tag!("~"), |x| (Negation, x)),
            map(tag!("-"), |x| (Negative, x)),
        ))(input)
    }
}

impl Op {
    op_parser_levels!(parse_level1, (add, Add, "+"), (sub, Sub, "-"));
    op_parser_levels!(parse_level2, (mul, Mul, "*"), (div, Div, "/"));
    op_parser_levels!(parse_level3, (asr, Asr, ">>"), (lsl, Lsl, "<<"));

    // logical operations in the pattern value part, AKA not in brackets
    op_parser_levels!(
        parse_unsafe_level4,
        (and_unsafe, And, "$and"),
        (or_unsafe, Or, "$or"),
        (xor_unsafe, Xor, "$xor")
    );
    // logical operations inside the square and round brackets
    op_parser_levels!(
        parse_safe_level4,
        (and, And, "&"),
        (or, Or, "|"),
        (xor, Xor, "^"),
    );
}

impl ExprElement {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        alt((
            map(
                pair(OpUnary::parse, Expr::parse_rec_or_ele),
                |((op, op_span), ele)| {
                    // TODO span shold combine the start of op and end of ele
                    Self::Op(op_span.clone(), op, Box::new(ele))
                },
            ),
            map(Value::parse_unsigned, Self::Value),
        ))(input)
    }
}

impl Expr {
    fn parse_rec_or_ele(
        input: &[Token],
    ) -> IResult<&[Token], Self, Box<SleighError>> {
        alt((
            delimited(tag!("("), Self::parse_safe, tag!(")")),
            map(ExprElement::parse, Self::Value),
        ))(input)
    }

    #[allow(clippy::type_complexity)]
    fn parse_op(
        this_level: fn(&[Token]) -> IResult<&[Token], Expr, Box<SleighError>>,
        next_level: fn(&[Token]) -> IResult<&[Token], Expr, Box<SleighError>>,
        op: fn(&[Token]) -> IResult<&[Token], (Op, &Span), Box<SleighError>>,
    ) -> impl FnMut(&[Token]) -> IResult<&[Token], Expr, Box<SleighError>> {
        move |input: &[Token]| {
            let (input, (left, rest)) =
                pair(next_level, opt(pair(op, this_level)))(input)?;
            let expr = if let Some(((op, op_src), rest)) = rest {
                Self::Op(op_src.clone(), op, Box::new(left), Box::new(rest))
            } else {
                left
            };
            Ok((input, expr))
        }
    }

    // parse with precedence
    // safe, things around brackets
    declare_expr_level!(parse_safe_lv1, parse_lv2, Op::parse_safe_level4);
    // unsafe, things not around brackets
    declare_expr_level!(parse_unsafe_lv1, parse_lv2, Op::parse_unsafe_level4);
    declare_expr_level!(parse_lv2, parse_lv3, Op::parse_level3);
    declare_expr_level!(parse_lv3, parse_lv4, Op::parse_level2);
    declare_expr_level!(parse_lv4, parse_rec_or_ele, Op::parse_level1);

    pub fn parse_safe(
        input: &[Token],
    ) -> IResult<&[Token], Self, Box<SleighError>> {
        Self::parse_safe_lv1(input)
    }

    pub fn parse_unsafe(
        input: &[Token],
    ) -> IResult<&[Token], Self, Box<SleighError>> {
        Self::parse_unsafe_lv1(input)
    }
}

#[derive(Clone, Debug)]
pub struct GlobalSet {
    pub src: Span,
    pub address: Value,
    pub context: String,
}
impl GlobalSet {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
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
    fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            terminated(
                separated_pair(ident, tag!("="), |x| Expr::parse_safe(x)),
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
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
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
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(many0(Assertation::parse), |assertations| Self {
            assertations,
        })(input)
    }
}
