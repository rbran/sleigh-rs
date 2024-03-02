use nom::combinator::{cut, map};
use nom::multi::many0;
use nom::sequence::{preceded, terminated, tuple};
use nom::IResult;

use crate::preprocessor::token::Token;
use crate::syntax::parser::{ident, this_ident};
use crate::syntax::BitRangeLsbLen;
use crate::{SleighError, Span};

#[derive(Clone, Debug)]
pub struct BitRangeDef {
    ranges: Vec<VarnodeField>,
}

impl BitRangeDef {
    pub fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            preceded(this_ident("bitrange"), cut(many0(VarnodeField::parse))),
            |ranges| BitRangeDef { ranges },
        )(input)
    }
}

impl IntoIterator for BitRangeDef {
    type Item = VarnodeField;
    type IntoIter = std::vec::IntoIter<VarnodeField>;

    fn into_iter(self) -> Self::IntoIter {
        self.ranges.into_iter()
    }
}

#[derive(Clone, Debug)]
pub struct VarnodeField {
    pub src: Span,
    pub name: String,
    pub varnode_name: String,
    pub varnode_name_span: Span,
    pub range: BitRangeLsbLen,
}

impl VarnodeField {
    fn parse(input: &[Token]) -> IResult<&[Token], Self, Box<SleighError>> {
        map(
            tuple((terminated(ident, tag!("=")), ident, BitRangeLsbLen::parse)),
            |((name, name_span), (varnode_name, varnode_name_span), range)| {
                VarnodeField {
                    name,
                    src: name_span.clone(),
                    varnode_name,
                    varnode_name_span: varnode_name_span.clone(),
                    range,
                }
            },
        )(input)
    }
}
