use nom::branch::alt;
use nom::combinator::{map, map_res, opt, value};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, pair, tuple};
use nom::IResult;

use super::disassembly;
use crate::preprocessor::token::Token;
use crate::semantic::pattern::{CmpOp, Ellipsis};
use crate::syntax::parser::ident;
use crate::{SleighError, Span};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Op {
    And,
    Or,
}

impl Op {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Op, Box<SleighError>> {
        alt((value(Op::Or, tag!("|")), value(Op::And, tag!("&"))))(input)
    }
}

impl CmpOp {
    pub fn parse(
        input: &[Token],
    ) -> IResult<&[Token], CmpOp, Box<SleighError>> {
        alt((
            value(CmpOp::Le, tag!("<=")),
            value(CmpOp::Ge, tag!(">=")),
            value(CmpOp::Ne, tag!("!=")),
            //TODO order is important, so ">=" is not consumed as ">"
            value(CmpOp::Eq, tag!("=")),
            value(CmpOp::Lt, tag!("<")),
            value(CmpOp::Gt, tag!(">")),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub struct ConstraintValue {
    pub expr: disassembly::Expr,
}

impl ConstraintValue {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            alt((
                delimited(
                    tag!("("),
                    |x| disassembly::Expr::parse_safe(x),
                    tag!(")"),
                ),
                |x| disassembly::Expr::parse_unsafe(x),
            )),
            |expr| ConstraintValue { expr },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Constraint {
    pub op: CmpOp,
    pub value: ConstraintValue,
}

impl Constraint {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(pair(CmpOp::parse, ConstraintValue::parse), |(op, value)| {
            Self { op, value }
        })(input)
    }
}

#[derive(Clone, Debug)]
pub enum Field {
    Field {
        field: String,
        src: Span,
        constraint: Option<Constraint>,
    },
    SubPattern(Pattern),
}
impl Field {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        alt((
            map(delimited(tag!("("), Pattern::parse, tag!(")")), |sub| {
                Field::SubPattern(sub)
            }),
            map(
                pair(ident, opt(Constraint::parse)),
                |((field, field_src), constraint)| Field::Field {
                    field,
                    src: field_src.clone(),
                    constraint,
                },
            ),
        ))(input)
    }
}

#[derive(Clone, Debug)]
pub struct Element {
    pub field: Field,
    pub ellipsis: Option<Ellipsis>,
}

impl Element {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map_res(
            tuple((
                opt(map(tag!("..."), |span| (Ellipsis::Left, span))),
                Field::parse,
                opt(map(tag!("..."), |span| (Ellipsis::Right, span))),
            )),
            |(left, field, right)| {
                let ellipsis = match (left, right) {
                    (Some((_, left)), Some((_, right))) => {
                        let location = Span::combine(
                            left.clone().start(),
                            right.clone().end(),
                        );
                        return Err(Box::new(SleighError::DualEllipsis(
                            location,
                        )));
                    }
                    (left, right) => left.or(right).map(|(e, _)| e),
                };
                Ok(Element { field, ellipsis })
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub src: Span,
    pub first: Element,
    pub elements: Vec<(Op, Element)>,
}

impl Block {
    pub fn op(&self) -> Op {
        self.elements.first().map(|(op, _)| *op).unwrap_or(Op::And)
    }
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            tuple((Element::parse, many0(pair(Op::parse, Element::parse)))),
            |(first, elements)| {
                //TODO improve this src
                let src = match &first.field {
                    Field::SubPattern(pat) => pat.src.clone(),
                    Field::Field { src, .. } => src.clone(),
                };
                Self {
                    first,
                    elements,
                    src,
                }
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub struct Pattern {
    //NOTE point after the `is` in the constructor, pattern itself don't have a
    //start, thats because it could be mixed with the `with_block` pattern
    pub src: Span,
    //NOTE: point after the `is`
    pub blocks: Vec<Block>,
}
impl IntoIterator for Pattern {
    type Item = Block;
    type IntoIter = std::vec::IntoIter<Block>;

    fn into_iter(self) -> Self::IntoIter {
        self.blocks.into_iter()
    }
}

impl Pattern {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        let (rest, pattern) =
            map(separated_list0(tag!(";"), Block::parse), |blocks| Pattern {
                //TODO improve the src here
                src: input.first().unwrap().location.clone(),
                blocks,
            })(input)?;
        Ok((rest, pattern))
    }
}
