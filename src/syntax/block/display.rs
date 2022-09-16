use std::borrow::Cow;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::anychar;
use nom::combinator::peek;
use nom::combinator::recognize;
use nom::sequence::tuple;
use nom::IResult;

use crate::base::{empty_space1, ident, string};

#[derive(Clone, Debug)]
pub enum DisplayElement<'a> {
    Ident(&'a str),
    Literal(Cow<'a, str>),
}

#[derive(Clone, Debug)]
pub struct Display<'a>(pub Vec<DisplayElement<'a>>);

impl<'a> Display<'a> {
    pub fn parse(input_ori: &'a str) -> IResult<&'a str, Self> {
        use DisplayElement::*;
        let mut input = input_ori;
        let mut display: Vec<DisplayElement> = vec![];
        let mut str_start = input;

        loop {
            let mut flush_acc = |str_end: &'a str| {
                let len =
                    str_end.as_ptr() as usize - str_start.as_ptr() as usize;
                let str_current = &str_start[..len];
                if str_current.len() > 0 {
                    display.push(Literal(Cow::Borrowed(str_current)));
                }
            };

            //found the end
            input = if let Ok((input_new, _)) = is_word(input) {
                flush_acc(input);
                input = input_new;
                break;
            } else if let Ok((input_new, str_end)) = tag::<_, _, ()>("^")(input)
            {
                //used to concat strings, flush the current acc string and
                //start a new after the `^`
                flush_acc(str_end);
                str_start = input_new;
                input_new
            } else if let Ok((input_new, value)) = string(input) {
                //found a string, flush the current acc string, push the parsed
                //string and and start a new one after the parsed string (`"`)
                flush_acc(input);
                display.push(Literal(Cow::Owned(value)));
                str_start = input_new;
                input_new
            } else if let Ok((input_new, ident)) = ident(input) {
                //push the accumulated string, push the parsed ident
                flush_acc(input);
                str_start = input_new;
                display.push(Ident(ident));
                input_new
            } else if let Ok((input_new, _)) = anychar::<_, ()>(input) {
                //let the char be part of the acc string
                input_new
            } else {
                unreachable!()
            };
        }
        //trim spaces
        let start = find_not_whitespace(display.iter().enumerate());
        let mut display = match start {
            Some(start) => {
                let end = find_not_whitespace(display.iter().enumerate().rev())
                    .map(|x| x + 1)
                    .unwrap_or(display.len());
                display.drain(start..end).collect()
            }
            None => display,
        };
        //trim spaces from first and last element
        trim_space(display.last_mut(), str::trim_end);
        trim_space(display.first_mut(), str::trim_start);

        return Ok((input, Display(display)));
    }
}

fn is_word(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        empty_space1,
        tag("is"),
        alt((empty_space1, peek(tag("(")))),
    )))(input)
}

fn find_not_whitespace<'a>(
    mut iter: impl Iterator<Item = (usize, &'a DisplayElement<'a>)>,
) -> Option<usize> {
    iter.find(|(_, x)| match x {
        DisplayElement::Ident(_) => true,
        DisplayElement::Literal(x) => {
            x.chars().any(|x| !x.is_ascii_whitespace())
        }
    })
    .map(|(i, _)| i)
}

fn trim_space<'a>(
    input: Option<&mut DisplayElement<'a>>,
    trim: fn(&str) -> &str,
) {
    match input {
        Some(DisplayElement::Literal(ref mut old)) => {
            let new = trim(old);
            if new.len() != old.len() {
                *old = Cow::Owned(new.to_string());
            }
        }
        _ => (),
    }
}
