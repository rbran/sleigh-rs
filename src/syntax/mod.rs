pub mod attach;
pub mod block;
pub mod define;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{consumed, eof, map};
use nom::multi::many0;
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::IResult;

use crate::base::{empty_space0, number_unsig, IntTypeU};
use crate::preprocessor::PreProcOutput;
use crate::InputSource;

use self::attach::Attach;
use self::block::pcode_macro::PcodeMacro;
use self::block::table::Constructor;
use self::block::with_block::WithBlock;
use self::define::Define;

//TODO move this
#[derive(Clone, Copy, Debug)]
pub struct BitRange<'a> {
    pub src: &'a str,
    pub lsb_bit: IntTypeU,
    pub n_bits: IntTypeU,
}
impl<'a> BitRange<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            consumed(delimited(
                pair(tag("["), empty_space0),
                separated_pair(
                    number_unsig,
                    tuple((empty_space0, tag(","), empty_space0)),
                    number_unsig,
                ),
                pair(empty_space0, tag("]")),
            )),
            |(src, (lsb, n_bits))| Self {
                src,
                lsb_bit: lsb,
                n_bits,
            },
        )(input)
    }
}

#[derive(Clone, Debug)]
pub enum Assertation<'a> {
    Define(Define<'a>),
    Attach(Attach<'a>),
    TableConstructor(Constructor<'a>),
    PcodeMacro(PcodeMacro<'a>),
    WithBlock(WithBlock<'a>),
}
impl<'a> Assertation<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(Define::parse, |x| Self::Define(x)),
            map(Attach::parse, |x| Self::Attach(x)),
            map(PcodeMacro::parse, |x| Self::PcodeMacro(x)),
            map(WithBlock::parse, |x| Self::WithBlock(x)),
            map(Constructor::parse, |x| Self::TableConstructor(x)),
        ))(input)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Syntax<'a> {
    assertations: Vec<Assertation<'a>>,
}
impl<'a> Syntax<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            many0(preceded(empty_space0, Assertation::parse)),
            |assertations| Self { assertations },
        )(input)
    }
}
impl<'a> IntoIterator for Syntax<'a> {
    type Item = Assertation<'a>;
    type IntoIter = std::vec::IntoIter<Assertation<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.assertations.into_iter()
    }
}

pub fn parse_syntax<'a>(
    input: &PreProcOutput,
) -> Result<Syntax, nom::Err<nom::error::Error<InputSource>>> {
    terminated(Syntax::parse, pair(empty_space0, eof))(input.as_str())
        .map_err(|err| {
            err.map_input(|err_pos| {
                input.source_data_start(err_pos).unwrap().clone()
            })
        })
        .map(|(_, result)| result)
}
