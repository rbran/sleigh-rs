use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{cut, map};
use nom::sequence::{pair, preceded, separated_pair, terminated};
use nom::IResult;

use crate::base::{
    empty_space0, empty_space1, fieldlist, integerlist, registerlist,
    stringlist, IntTypeS,
};

#[derive(Clone, Debug)]
pub struct Attach<'a> {
    pub fields: Vec<&'a str>,
    pub meaning: Meaning<'a>,
}

#[derive(Clone, Debug)]
pub enum Meaning<'a> {
    Variable(Vec<Option<&'a str>>),
    Name(Vec<Option<String>>),
    Number(Vec<Option<IntTypeS>>),
}
impl<'a> Attach<'a> {
    pub fn parse(input: &'a str) -> IResult<&'a str, Attach<'a>> {
        map(
            preceded(
                pair(tag("attach"), empty_space1),
                cut(terminated(
                    alt((
                        Attach::attach_lists(
                            "variables",
                            map(registerlist, |x| Meaning::Variable(x)),
                        ),
                        Attach::attach_lists(
                            "names",
                            map(stringlist, |x| Meaning::Name(x)),
                        ),
                        Attach::attach_lists(
                            "values",
                            map(integerlist, |x| Meaning::Number(x)),
                        ),
                    )),
                    pair(empty_space0, tag(";")),
                )),
            ),
            |(varnodes, value)| Attach {
                fields: varnodes,
                meaning: value,
            },
        )(input)
    }
    fn attach_lists<Output>(
        name: &'a str,
        element: impl FnMut(&'a str) -> IResult<&'a str, Output>,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, (Vec<&'a str>, Output)> {
        preceded(
            pair(tag(name), empty_space0),
            separated_pair(fieldlist, empty_space0, element),
        )
    }
}
