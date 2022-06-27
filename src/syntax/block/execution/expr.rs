use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{consumed, map, map_opt, opt};
use nom::multi::separated_list0;
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::IResult;

use super::{op, UserCall};
use crate::base::{
    empty_space0, empty_space1, ident, number_unsig, IntTypeU, Value,
};

#[derive(Clone, Debug)]
pub enum Expr<'a> {
    Value(ExprElement<'a>),
    Op(&'a str, op::Binary, Box<Expr<'a>>, Box<Expr<'a>>),
}

macro_rules! declare_expr_level {
    ($name:ident, $next:ident, $op:path) => {
        fn $name(input: &'a str) -> IResult<&'a str, Self> {
            Self::parse_op(Self::$name, Self::$next, $op)(input)
        }
    };
}
impl<'a> Expr<'a> {
    fn parse_call(input: &'a str) -> IResult<&'a str, Self> {
        map(
            pair(
                consumed(op::Binary::parse_call_name),
                delimited(
                    tuple((empty_space0, tag("("), empty_space0)),
                    separated_pair(
                        Expr::parse,
                        tuple((empty_space0, tag(","), empty_space0)),
                        Expr::parse,
                    ),
                    pair(empty_space0, tag(")")),
                ),
            ),
            |((src, op), (param1, param2))| {
                Self::Op(src, op, Box::new(param1), Box::new(param2))
            },
        )(input)
    }
    fn value_or_rec(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            Self::parse_rec,
            map(ExprElement::parse_ele, |value| Self::Value(value)),
        ))(input)
    }
    fn eleme_rec(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            Self::parse_rec,
            map(ExprElement::parse, |value| Self::Value(value)),
        ))(input)
    }
    pub fn parse_rec(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            delimited(
                pair(tag("("), empty_space0),
                Self::parse,
                pair(empty_space0, tag(")")),
            ),
            Self::parse_call,
        ))(input)
    }
    fn parse_op(
        this_level: fn(&'a str) -> IResult<&'a str, Expr<'a>>,
        next_level: fn(&'a str) -> IResult<&'a str, Expr<'a>>,
        op: fn(&'a str) -> IResult<&'a str, op::Binary>,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, Expr<'a>> {
        move |input: &'a str| {
            let (input, (left, rest)) = pair(
                next_level,
                opt(preceded(
                    empty_space0,
                    separated_pair(consumed(op), empty_space0, this_level),
                )),
            )(input)?;
            let expr = if let Some(((op_src, op), rest)) = rest {
                Self::Op(op_src, op, Box::new(left), Box::new(rest))
            } else {
                left
            };
            Ok((input, expr))
        }
    }
    //parse with precedence
    declare_expr_level!(parse_lv1, parse_lv2, op::Binary::level6);
    declare_expr_level!(parse_lv2, parse_lv3, op::Binary::level5);
    declare_expr_level!(parse_lv3, parse_lv4, op::Binary::level4);
    declare_expr_level!(parse_lv4, parse_lv5, op::Binary::level3);
    declare_expr_level!(parse_lv5, parse_lv6, op::Binary::level2);
    declare_expr_level!(parse_lv6, eleme_rec, op::Binary::level1);
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        Self::parse_lv1(input)
    }
}

#[derive(Clone, Debug)]
pub enum ExprElement<'a> {
    Value(Value<'a>),
    Reference(&'a str, Option<op::ByteRangeLsb<'a>>, &'a str),
    Op(&'a str, op::Unary<'a>, Box<Expr<'a>>),
    New(&'a str, Box<Expr<'a>>, Option<Box<Expr<'a>>>),
    CPool(&'a str, Vec<Expr<'a>>),
    UserCall(UserCall<'a>),
    //unable to differ a UserCall(one param)/Op(Msb) eg: `ident(1)`,
    //so ambiguous1
    Ambiguous1 {
        name: &'a str,
        param: IntTypeU,
        param_src: &'a str,
    },
}

impl<'a> ExprElement<'a> {
    fn parse_value(input: &'a str) -> IResult<&'a str, Self> {
        map(Value::parse, Self::Value)(input)
    }
    fn parse_op_call(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(pair(
                op::Unary::parse_call_name,
                delimited(
                    tuple((empty_space0, tag("("), empty_space0)),
                    Expr::parse,
                    pair(empty_space0, tag(")")),
                ),
            )),
            |(src, (op, param))| Self::Op(src, op, Box::new(param)),
        )(input)
    }
    fn parse_new_call(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(delimited(
                tuple((tag("newobject"), empty_space0, tag("("), empty_space0)),
                pair(
                    Expr::parse,
                    opt(preceded(
                        tuple((empty_space0, tag(","), empty_space0)),
                        Expr::parse,
                    )),
                ),
                pair(empty_space0, tag(")")),
            )),
            |(src, (param0, param1))| {
                Self::New(src, Box::new(param0), param1.map(Box::new))
            },
        )(input)
    }
    fn parse_cpool_call(input: &'a str) -> IResult<&'a str, Self> {
        map_opt(
            pair(
                tag("cpool"),
                delimited(
                    tuple((empty_space0, tag("("), empty_space0)),
                    separated_list0(
                        tuple((empty_space0, tag(","), empty_space0)),
                        Expr::parse,
                    ),
                    pair(empty_space0, tag(")")),
                ),
            ),
            |(src, params)| {
                (params.len() >= 2).then(|| Self::CPool(src, params))
            },
        )(input)
    }
    fn parse_amb1(input: &'a str) -> IResult<&'a str, Self> {
        map(
            pair(
                ident,
                delimited(
                    tuple((empty_space0, tag("("), empty_space0)),
                    consumed(number_unsig),
                    pair(empty_space0, tag(")")),
                ),
            ),
            |(name, (param_src, param))| Self::Ambiguous1 {
                name,
                param,
                param_src,
            },
        )(input)
    }
    fn parse_user_call(input: &'a str) -> IResult<&'a str, Self> {
        map(UserCall::parse_expr, Self::UserCall)(input)
    }
    fn parse_reference(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(separated_pair(
                preceded(
                    tag("&"),
                    opt(terminated(op::ByteRangeLsb::parse, empty_space1)),
                ),
                empty_space0,
                ident,
            )),
            |(src, (ref_size, value))| Self::Reference(src, ref_size, value),
        )(input)
    }
    fn parse_user_call_or_amb1(input: &'a str) -> IResult<&'a str, Self> {
        //NOTE order is importante here
        alt((Self::parse_amb1, Self::parse_user_call))(input)
    }
    fn parse_ele(input: &'a str) -> IResult<&'a str, Self> {
        //NOTE order is importante here
        alt((
            Self::parse_op_call,
            Self::parse_new_call,
            Self::parse_cpool_call,
            Self::parse_user_call_or_amb1,
            Self::parse_value,
        ))(input)
    }
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        //this can be a reference or op_before/ele/op_after
        if let Ok((input, reference)) = Self::parse_reference(input) {
            return Ok((input, reference));
        }

        let (input, (op_before, value, op_after)) = tuple((
            opt(terminated(consumed(op::Unary::parse_before), empty_space0)),
            Expr::value_or_rec,
            opt(preceded(empty_space0, consumed(op::Unary::parse_after))),
        ))(input)?;
        //op after have higher precedence, so it goes first
        let mut expr = value;
        for (src, unary) in [op_after, op_before].into_iter().filter_map(|x| x)
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
