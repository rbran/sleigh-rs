use nom::branch::alt;
use nom::combinator::{map, map_opt, opt};
use nom::multi::separated_list0;
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::IResult;

use super::op::{ByteRangeLsb, Unary};
use super::UserCall;
use crate::preprocessor::token::Token;
use crate::semantic::execution::Binary;
use crate::syntax::parser::{ident, number, this_ident};
use crate::syntax::Value;
use crate::{NumberUnsigned, SleighError, Span};

#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(Span, Binary, Box<Expr>, Box<Expr>),
}

macro_rules! declare_expr_level {
    ($name:ident, $next:ident, $op:path) => {
        fn $name(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
            Self::parse_op(Self::$name, Self::$next, $op)(input)
        }
    };
}
impl Expr {
    fn parse_call(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            pair(
                Binary::parse_call_name,
                delimited(
                    tag!("("),
                    separated_pair(Expr::parse, tag!(","), Expr::parse),
                    tag!(")"),
                ),
            ),
            |((op, op_src), (param1, param2))| {
                Self::Op(op_src.clone(), op, Box::new(param1), Box::new(param2))
            },
        )(input)
    }
    fn value_or_rec(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        alt((
            Self::parse_rec,
            map(ExprElement::parse_ele, |value| Self::Value(value)),
        ))(input)
    }
    fn eleme_rec(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        alt((
            Self::parse_rec,
            map(ExprElement::parse, |value| Self::Value(value)),
        ))(input)
    }
    pub fn parse_rec(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        alt((
            delimited(tag!("("), Self::parse, tag!(")")),
            Self::parse_call,
        ))(input)
    }
    fn parse_op(
        this_level: fn(&[Token]) -> IResult<&[Token], Expr, SleighError>,
        next_level: fn(&[Token]) -> IResult<&[Token], Expr, SleighError>,
        op: fn(&[Token]) -> IResult<&[Token], (Binary, &Span), SleighError>,
    ) -> impl FnMut(&[Token]) -> IResult<&[Token], Expr, SleighError> {
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
    //parse with precedence
    declare_expr_level!(parse_lv1, parse_lv2, Binary::level6);
    declare_expr_level!(parse_lv2, parse_lv3, Binary::level5);
    declare_expr_level!(parse_lv3, parse_lv4, Binary::level4);
    declare_expr_level!(parse_lv4, parse_lv5, Binary::level3);
    declare_expr_level!(parse_lv5, parse_lv6, Binary::level2);
    declare_expr_level!(parse_lv6, eleme_rec, Binary::level1);
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        Self::parse_lv1(input)
    }
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(Value),
    Reference(Span, Option<ByteRangeLsb>, String),
    Op(Span, Unary, Box<Expr>),
    New(Span, Box<Expr>, Option<Box<Expr>>),
    CPool(Span, Vec<Expr>),
    UserCall(UserCall),
    //unable to differ a UserCall(one param)/Op(Msb) eg: `ident(1)`,
    //so ambiguous1
    Ambiguous1 {
        name: String,
        param: NumberUnsigned,
        param_src: Span,
    },
}

impl ExprElement {
    fn parse_value(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(Value::parse_signed, Self::Value)(input)
    }
    fn parse_op_call(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            pair(
                Unary::parse_call_name,
                delimited(tag!("("), Expr::parse, tag!(")")),
            ),
            |((op, src), param)| Self::Op(src.clone(), op, Box::new(param)),
        )(input)
    }
    fn parse_new_call(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            tuple((
                terminated(this_ident("newobject"), tag!("(")),
                pair(Expr::parse, opt(preceded(tag!(","), Expr::parse))),
                tag!(")"),
            )),
            |(start, (param0, param1), end)| {
                let location =
                    Span::combine(start.clone().start(), end.clone().end());
                Self::New(location, Box::new(param0), param1.map(Box::new))
            },
        )(input)
    }
    fn parse_cpool_call(
        input: &[Token],
    ) -> IResult<&[Token], Self, SleighError> {
        map_opt(
            pair(
                this_ident("cpool"),
                delimited(
                    tag!("("),
                    separated_list0(tag!(","), Expr::parse),
                    tag!(")"),
                ),
            ),
            |(src, params)| {
                (params.len() >= 2).then(|| Self::CPool(src.clone(), params))
            },
        )(input)
    }
    fn parse_amb1(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        map(
            pair(ident, delimited(tag!("("), number, tag!(")"))),
            |((name, _name_src), (param, param_src))| Self::Ambiguous1 {
                name,
                param,
                param_src: param_src.clone(),
            },
        )(input)
    }
    fn parse_user_call(
        input: &[Token],
    ) -> IResult<&[Token], Self, SleighError> {
        map(UserCall::parse_expr, Self::UserCall)(input)
    }
    fn parse_reference(
        input: &[Token],
    ) -> IResult<&[Token], Self, SleighError> {
        map(
            tuple((tag!("&"), opt(ByteRangeLsb::parse), ident)),
            |(src, ref_size, (value, _))| {
                Self::Reference(src.clone(), ref_size, value)
            },
        )(input)
    }
    fn parse_user_call_or_amb1(
        input: &[Token],
    ) -> IResult<&[Token], Self, SleighError> {
        //NOTE order is importante here
        alt((Self::parse_amb1, Self::parse_user_call))(input)
    }
    fn parse_ele(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        //NOTE order is importante here
        alt((
            Self::parse_op_call,
            Self::parse_new_call,
            Self::parse_cpool_call,
            Self::parse_user_call_or_amb1,
            Self::parse_value,
        ))(input)
    }
    fn parse(input: &[Token]) -> IResult<&[Token], Self, SleighError> {
        //this can be a reference or op_before/ele/op_after
        if let Ok((input, reference)) = Self::parse_reference(input) {
            return Ok((input, reference));
        }

        let (input, (op_before, value, op_after)) = tuple((
            opt(Unary::parse_before),
            Expr::value_or_rec,
            opt(Unary::parse_after),
        ))(input)?;
        //op after have higher precedence, so it goes first
        let mut expr = value;
        for (unary, src) in [op_after, op_before].into_iter().filter_map(|x| x)
        {
            expr = Expr::Value(Self::Op(src, unary, Box::new(expr)));
        }

        //in case we got an value an not op, simplify the expr to only a value
        //otherwise have the Self::Op(Expr) returned
        let expr = match expr {
            Expr::Value(value @ Self::Value(_)) => value,
            Expr::Value(op) => op,
            _ => unreachable!(),
        };
        Ok((input, expr))
    }
}
