use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::sequence::{delimited, terminated, tuple};
use nom::IResult;

use crate::base::{
    empty_space0, empty_space1, ident, number_unsig, registerlist, IntTypeU,
};

#[derive(Clone, Debug)]
pub struct Varnode<'a> {
    pub space_name: &'a str,
    pub offset: IntTypeU,
    pub varnode_size: IntTypeU,
    pub names: Vec<Option<&'a str>>,
}

impl<'a> Varnode<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            tuple((
                terminated(ident, empty_space1),
                delimited(
                    tuple((
                        tag("offset"),
                        empty_space0,
                        tag("="),
                        empty_space0,
                    )),
                    number_unsig,
                    empty_space1,
                ),
                delimited(
                    tuple((tag("size"), empty_space0, tag("="), empty_space0)),
                    number_unsig,
                    empty_space0,
                ),
                registerlist,
            )),
            |(space_name, offset, size, names)| Varnode {
                space_name,
                offset,
                varnode_size: size,
                names,
            },
        )(input)
    }
}
