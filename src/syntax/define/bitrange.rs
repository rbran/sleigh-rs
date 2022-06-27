use nom::bytes::complete::tag;
use nom::combinator::{cut, map};
use nom::multi::many0;
use nom::sequence::{pair, preceded, terminated, tuple};
use nom::IResult;

use crate::base::{empty_space0, empty_space1, ident};
use crate::syntax::BitRange;

#[derive(Clone, Debug)]
pub struct BitRangeDef<'a> {
    ranges: Vec<VarnodeField<'a>>,
}

impl<'a> BitRangeDef<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            preceded(
                pair(tag("bitrange"), empty_space1),
                cut(many0(preceded(empty_space0, VarnodeField::parse))),
            ),
            |bitranges| BitRangeDef { ranges: bitranges },
        )(input)
    }
}

impl<'a> IntoIterator for BitRangeDef<'a> {
    type Item = VarnodeField<'a>;
    type IntoIter = std::vec::IntoIter<VarnodeField<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.ranges.into_iter()
    }
}

#[derive(Clone, Debug)]
pub struct VarnodeField<'a> {
    pub name: &'a str,
    pub varnode_name: &'a str,
    pub range: BitRange<'a>,
}

impl<'a> VarnodeField<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            tuple((
                terminated(
                    ident,
                    tuple((empty_space0, tag("="), empty_space0)),
                ),
                terminated(ident, empty_space0),
                BitRange::parse,
            )),
            |(name, varnode_name, range)| VarnodeField {
                name,
                varnode_name,
                range,
            },
        )(input)
    }
}
