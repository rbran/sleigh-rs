use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{consumed, eof, map, opt, recognize, value};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;

use super::ifs::{if_cond, if_cond_owned, IfCheck, IfCheckOwned};
use super::{
    MACRO_DEFINE, MACRO_ELIF, MACRO_ELSE, MACRO_ENDIF, MACRO_IF, MACRO_IFDEF,
    MACRO_IFNDEF, MACRO_INCLUDE, MACRO_UNDEFINE,
};
use super::parser::{empty_line, end_of_line, ident, number, string};

fn macro_include(input: &str) -> IResult<&str, String> {
    delimited(
        tuple((space0, tag("@include"), space0)),
        string,
        end_of_line,
    )(input)
}

fn macro_define(input: &str) -> IResult<&str, Block> {
    let x = map(
        consumed(preceded(
            tuple((space0, tag("@define"), space1)),
            pair(
                ident,
                delimited(
                    space1,
                    opt(alt((
                        map(ident, DefineData::Alias),
                        //TODO number to string? What about a Number type???
                        map(
                            alt((
                                string,
                                map(recognize(number), str::to_string),
                            )),
                            DefineData::Value,
                        ),
                    ))),
                    end_of_line,
                ),
            ),
        )),
        |(src, (name, value))| Block::Define { src, name, value },
    )(input)?;
    Ok(x)
}
fn macro_undef(input: &str) -> IResult<&str, &str> {
    delimited(tuple((space0, tag("@undef"), space1)), ident, end_of_line)(input)
}
fn macro_ifdef(input: &str) -> IResult<&str, IfCheck> {
    map(
        delimited(tuple((space0, tag("@ifdef"), space1)), ident, end_of_line),
        IfCheck::Defined,
    )(input)
}
fn macro_ifndef(input: &str) -> IResult<&str, IfCheck> {
    map(
        delimited(tuple((space0, tag("@ifndef"), space1)), ident, end_of_line),
        IfCheck::NotDefined,
    )(input)
}
fn macro_if(input: &str) -> IResult<&str, IfCheck> {
    preceded(tuple((space0, tag("@if"), space1)), if_cond)(input)
}
fn macro_elif(input: &str) -> IResult<&str, IfCheck> {
    preceded(tuple((space0, tag("@elif"), space1)), if_cond)(input)
}
fn macro_endif(input: &str) -> IResult<&str, ()> {
    value((), tuple((space0, tag("@endif"), end_of_line)))(input)
}
fn macro_else(input: &str) -> IResult<&str, ()> {
    value((), tuple((space0, tag("@else"), end_of_line)))(input)
}
pub(crate) fn expansion(input: &str) -> IResult<&str, &str> {
    delimited(pair(tag("$("), space0), ident, pair(space0, tag(")")))(input)
}
fn empty_lines(input: &str) -> IResult<&str, ()> {
    // lines with nothing but comments
    //value((), many0(empty_line))(input)
    let mut input = empty_line(input)?.0;
    loop {
        if eof::<_, ()>(input).is_ok() {
            break;
        }
        input = match empty_line(input) {
            Ok((tmp, _)) => tmp,
            _ => break,
        };
    }
    Ok((input, ()))
}
fn data(input: &str) -> IResult<&str, &str> {
    //take line by line
    let start = input;
    let mut end = None;
    for line in start.lines() {
        //line start with a macro
        if pair::<_, _, _, (), _, _>(space0, tag("@"))(line).is_ok() {
            end = Some(line);
            break;
        }
        //empty line with comment, remove full line comments
        if empty_line(line).is_ok() {
            end = Some(line);
            break;
        }
        //search for expansion
        if let Some(found) = line.find("$(") {
            end = Some(&line[found..]);
            break;
        }
    }

    let (rest, data) = nom::InputTake::take_split(
        &input,
        end.map(|x| x.as_ptr() as usize - start.as_ptr() as usize)
            .unwrap_or(input.len()),
    );
    if data.len() == 0 {
        Err(nom::Err::Error(nom::error::Error {
            input,
            code: nom::error::ErrorKind::Fix,
        }))
    } else {
        Ok((rest, data))
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DefineDataOwned {
    Alias(String),
    Value(String),
}
impl DefineDataOwned {
    fn parse(input: &str) -> IResult<&str, Self> {
        alt((
            map(ident, |ident| Self::Alias(ident.to_owned())),
            //TODO number to string? What about a Number type???
            map(
                alt((string, map(recognize(number), str::to_string))),
                Self::Value,
            ),
        ))(input)
    }
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum DefineData<'a> {
    Alias(&'a str),
    Value(String),
}
impl<'a> DefineData<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(ident, Self::Alias),
            //TODO number to string? What about a Number type???
            map(
                alt((string, map(recognize(number), str::to_string))),
                Self::Value,
            ),
        ))(input)
    }
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Block<'a> {
    Data(&'a str),
    Define {
        src: &'a str,
        name: &'a str,
        value: Option<DefineData<'a>>,
    },
    Undefine(&'a str),
    Expand(&'a str),
    Include(String),
    Cond(CondBlock<'a>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MacroLine {
    Define {
        name: String,
        value: Option<DefineDataOwned>,
    },
    Undefine(String),
    Include(String),
    IfDef(String),
    IfNDef(String),
    If(IfCheckOwned),
    ElIf(IfCheckOwned),
    Else,
    EndIf,
}
impl MacroLine {
    pub fn parse_define(input: &str) -> IResult<&str, Self> {
        map(
            delimited(
                pair(tag(MACRO_DEFINE), space1),
                pair(ident, opt(preceded(space1, DefineDataOwned::parse))),
                end_of_line,
            ),
            |(name, value)| Self::Define {
                name: name.to_owned(),
                value,
            },
        )(input)
    }
    fn parse_undefine(input: &str) -> IResult<&str, Self> {
        map(
            delimited(pair(tag(MACRO_UNDEFINE), space1), ident, end_of_line),
            |x| Self::Undefine(x.to_owned()),
        )(input)
    }
    fn parse_include(input: &str) -> IResult<&str, Self> {
        map(
            delimited(pair(tag(MACRO_INCLUDE), space0), string, end_of_line),
            |x| Self::Include(x.to_owned()),
        )(input)
    }
    fn parse_if(input: &str) -> IResult<&str, Self> {
        map(
            preceded(pair(tag(MACRO_IF), space1), if_cond_owned),
            Self::If,
        )(input)
    }
    fn parse_ifdef(input: &str) -> IResult<&str, Self> {
        map(
            delimited(pair(tag(MACRO_IFDEF), space1), ident, end_of_line),
            |x| Self::IfDef(x.to_owned()),
        )(input)
    }
    fn parse_ifndef(input: &str) -> IResult<&str, Self> {
        map(
            delimited(pair(tag(MACRO_IFNDEF), space1), ident, end_of_line),
            |x| Self::IfNDef(x.to_owned()),
        )(input)
    }
    fn parse_elif(input: &str) -> IResult<&str, Self> {
        map(
            preceded(pair(tag(MACRO_ELIF), space1), if_cond_owned),
            Self::ElIf,
        )(input)
    }
    fn parse_else(input: &str) -> IResult<&str, Self> {
        value(Self::Else, pair(tag(MACRO_ELSE), end_of_line))(input)
    }
    fn parse_endif(input: &str) -> IResult<&str, Self> {
        value(Self::EndIf, pair(tag(MACRO_ENDIF), end_of_line))(input)
    }
    pub fn parse(input: &str) -> IResult<&str, Self> {
        preceded(
            space0,
            alt((
                Self::parse_define,
                Self::parse_undefine,
                Self::parse_include,
                Self::parse_if,
                Self::parse_ifdef,
                Self::parse_ifndef,
                Self::parse_elif,
                Self::parse_else,
                Self::parse_endif,
            )),
        )(input)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CondBlock<'a> {
    pub blocks: Vec<(IfCheck<'a>, Vec<Block<'a>>)>,
    pub else_block: Vec<Block<'a>>,
}

fn macro_cond_block(input: &str) -> IResult<&str, Block> {
    let x = map(
        tuple((
            pair(alt((macro_if, macro_ifdef, macro_ifndef)), blocks_or_empty),
            many0(pair(macro_elif, blocks_or_empty)),
            opt(preceded(macro_else, blocks_or_empty)),
            macro_endif,
        )),
        |(if_block, elif_blocks, else_block, _)| {
            let blocks = [if_block].into_iter().chain(elif_blocks).collect();
            let else_block = else_block.unwrap_or_default();
            Block::Cond(CondBlock { blocks, else_block })
        },
    )(input)?;
    Ok(x)
}

//never fails, only return empty block, consumes empty lines, before and after
//the blocks
fn blocks_or_empty(mut input: &str) -> IResult<&str, Vec<Block>> {
    let mut blocks = vec![];
    loop {
        //match the empty lines, if any
        input = match empty_lines(input) {
            Ok((input, _)) => input,
            _ => input,
        };

        input = match alt((
            map(data, Block::Data),
            map(expansion, Block::Expand),
            macro_define,
            map(macro_undef, Block::Undefine),
            map(macro_include, Block::Include),
            macro_cond_block,
        ))(input)
        {
            Ok((input, block)) => {
                blocks.push(block);
                input
            }
            _ => break,
        }
    }
    Ok((input, blocks))
}

pub fn parse_blocks(input: &str) -> IResult<&str, Vec<Block>> {
    terminated(blocks_or_empty, pair(empty_lines, eof))(input)
}

#[cfg(test)]
mod tests {
    use crate::preprocessor::macros;

    #[test]
    fn empty_lines() {
        assert_eq!(macros::empty_lines("\n").unwrap(), ("", ()));
    }
    //    use crate::preprocessor::ifs::IfCheck;
    //    use crate::preprocessor::macros;
    //    use crate::preprocessor::macros::{Blocks, CondBlock};
    //    use nom::multi::many0;
    //
    //    #[test]
    //    fn data_include() {
    //        assert_eq!(
    //            macros::macro_include(&"@include \"6502.slaspec\"").unwrap(),
    //            ("", "6502.slaspec".to_string())
    //        );
    //    }
    //    #[test]
    //    fn data_block() {
    //        assert_eq!(
    //            macros::data(&"asdf\n  123\n@asdf\n").unwrap(),
    //            ("@asdf\n", "asdf\n  123\n")
    //        );
    //        assert_eq!(
    //            macros::data(&"123\n  asd\n  @asdf\n").unwrap(),
    //            ("  @asdf\n", "123\n  asd\n")
    //        );
    //        assert_eq!(
    //            many0(macros::data)(
    //                &"test1\n\n \n  #test test\n## test\n ####### test\n\ntest2\n"
    //            )
    //            .unwrap(),
    //            ("", vec!["test1\n", "test2\n"])
    //        );
    //        assert_eq!(
    //            many0(macros::data)(
    //                &"test1\n\n \ntest2  #test test\ntest3 ## test\n ####### test\n\ntest4\n"
    //            )
    //            .unwrap(),
    //            ("", vec!["test1\n", "test2  #test test\ntest3 ## test\n", "test4\n"])
    //        );
    //    }
    //    #[test]
    //    fn expand_block() {
    //        assert_eq!(
    //            macros::expansion(&"$(asdf)\n  $(123)\n@asdf\n").unwrap(),
    //            ("\n  $(123)\n@asdf\n", "asdf")
    //        );
    //    }
    //    #[test]
    //    fn macros_block() {
    //        assert_eq!(
    //            macros::macro_define(&"@define A A\n").unwrap(),
    //            (
    //                "",
    //                macros::Define {
    //                    ident: "A".into(),
    //                    value: macros::DefineData::Alias("A".into())
    //                }
    //            )
    //        );
    //        assert_eq!(
    //            macros::macro_define(&" @define A \"A\"\n").unwrap(),
    //            (
    //                "",
    //                macros::Define {
    //                    ident: "A",
    //                    value: macros::DefineData::Value("A".into())
    //                }
    //            )
    //        );
    //        assert_eq!(
    //            macros::macro_define(&"  @define A # test\n").unwrap(),
    //            (
    //                "",
    //                macros::Define {
    //                    ident: "A",
    //                    value: macros::DefineData::Empty,
    //                }
    //            )
    //        );
    //    }
    //    #[test]
    //    fn undefine_block() {
    //        assert_eq!(
    //            macros::macro_undef(&"@undef A  \n @def").unwrap(),
    //            (" @def", "A")
    //        );
    //    }
    //    #[test]
    //    fn include_block() {
    //        assert_eq!(
    //            macros::macro_include(&"  @include \"A\"   \nasdf").unwrap(),
    //            ("asdf", "A".to_string())
    //        );
    //    }
    //    #[test]
    //    fn cond_block1() {
    //        assert_eq!(
    //            macros::macro_cond_block(&"@ifndef A\nblock1\n@endif\nblock2\n")
    //                .unwrap(),
    //            (
    //                "block2\n",
    //                Blocks::Cond(CondBlock {
    //                    blocks: vec![(
    //                        IfCheck::NotDefined("A"),
    //                        vec![Blocks::Data("block1\n")]
    //                    ),],
    //                    else_block: vec![],
    //                })
    //            )
    //        );
    //    }
    //    #[test]
    //    fn cond_block2() {
    //        let x = macros::macro_cond_block(
    //            &r#"@ifndef A
    //block1
    //@elif defined(B)
    //block2
    //@else
    //block3
    //@endif
    //block4
    //"#,
    //        );
    //        dbg!(&x);
    //        assert_eq!(
    //            x.unwrap(),
    //            (
    //                "block4\n",
    //                Blocks::Cond(CondBlock {
    //                    blocks: vec![
    //                        (
    //                            IfCheck::NotDefined("A"),
    //                            vec![Blocks::Data("block1\n")]
    //                        ),
    //                        (IfCheck::Defined("B"), vec![Blocks::Data("block2\n")]),
    //                    ],
    //                    else_block: vec![Blocks::Data("block3\n")],
    //                })
    //            )
    //        );
    //    }
}
