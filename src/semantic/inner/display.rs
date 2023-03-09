use std::rc::Rc;

use crate::semantic::display::DisplayError;
use crate::semantic::inner::disassembly;
use crate::semantic::inner::table::Table;
use crate::semantic::inner::token::TokenField;
use crate::semantic::varnode::Varnode;
use crate::semantic::{GlobalReference, InstNext, InstStart};
use crate::syntax::block;
use crate::Span;

use super::varnode::Context;
use super::{Pattern, Sleigh};

#[derive(Clone, Debug)]
pub enum DisplayElement {
    Varnode(GlobalReference<Varnode>),
    Context(GlobalReference<Context>),
    //Bitrange(GlobalReference<Bitrange>),
    TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Table(GlobalReference<Table>),
    Dissasembly(Rc<disassembly::Variable>),
    Literal(String),
    Space,
}

impl DisplayElement {
    fn is_space(&self) -> bool {
        matches!(self, Self::Space)
    }
}

impl From<DisplayElement> for crate::semantic::display::DisplayScope {
    fn from(value: DisplayElement) -> Self {
        match value {
            DisplayElement::Space => Self::Space,
            DisplayElement::TokenField(x) => {
                Self::TokenField(x.convert_reference())
            }
            DisplayElement::Varnode(x) => Self::Varnode(x),
            //DisplayElement::Bitrange(x) => Self::Bitrange(x),
            DisplayElement::Literal(x) => Self::Literal(x),
            DisplayElement::Table(x) => Self::Table(x.convert_reference()),
            DisplayElement::Dissasembly(x) => Self::Disassembly(x.convert()),
            DisplayElement::Context(x) => Self::Context(x.convert_reference()),
            DisplayElement::InstStart(x) => Self::InstStart(x),
            DisplayElement::InstNext(x) => Self::InstNext(x),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Display {
    pub mneumonic: Option<String>,
    pub elements: Vec<DisplayElement>,
}

impl From<Display> for crate::semantic::display::Display {
    fn from(value: Display) -> Self {
        Self::new(
            value.mneumonic,
            value.elements.into_iter().map(|x| x.into()).collect(),
        )
    }
}

fn get_display_ref(
    sleigh: &Sleigh,
    pattern: &mut Pattern,
    name: &str,
    src: &Span,
) -> Result<DisplayElement, DisplayError> {
    use crate::semantic::inner::GlobalScope::*;
    pattern
        .disassembly_vars
        .get(name)
        .map(|var| Ok(DisplayElement::Dissasembly(Rc::clone(var))))
        .unwrap_or_else(|| {
            match sleigh
                .get_global(name)
                .ok_or(DisplayError::MissingRef(src.clone()))?
            {
                TokenField(x) => {
                    let token_field =
                        GlobalReference::from_element(x, src.clone());
                    if pattern
                        .produce_token_field(&token_field)
                        .map(|block_num| block_num.is_none())
                        .unwrap()
                    {
                        //TODO error here
                        todo!()
                    }
                    Ok(DisplayElement::TokenField(token_field))
                }
                Varnode(x) => Ok(DisplayElement::Varnode(
                    GlobalReference::from_element(x, src.clone()),
                )),
                Table(x) => Ok(DisplayElement::Table(
                    GlobalReference::from_element(x, src.clone()),
                )),
                Context(x) => Ok(DisplayElement::Context(
                    GlobalReference::from_element(x, src.clone()),
                )),
                _ => Err(DisplayError::InvalidRef(src.clone())),
            }
        })
}

impl Display {
    pub fn new<'a>(
        display: block::display::Display,
        sleigh: &Sleigh,
        pattern: &mut Pattern,
        is_root: bool,
    ) -> Result<Self, DisplayError> {
        use block::display::DisplayElement::*;
        //solve the idents, plus condensate multiple literals, plus split said
        //literals with spaces
        let mut out = vec![];
        let mut str_acc = String::new();
        let mut iter = display.0.into_iter();
        //in root table first element is the mneumonic
        let mneumonic = if is_root {
            let Some(ele) = iter.next() else {
                return Ok(Self::default());
            };
            match ele {
                Concat => None,
                Ident(_src, ident) => {
                    //ident can include ".", but mneumonic can't
                    if let Some((mneumonic, rest)) = ident.split_once('.') {
                        str_acc.push_str(&rest);
                        Some(mneumonic.to_string())
                    } else {
                        Some(ident)
                    }
                }
                Literal(_src, x) => {
                    str_acc.push_str(&x);
                    None
                }
            }
        } else {
            None
        };
        for ele in iter {
            match ele {
                Concat => (),
                Literal(_str, x) => str_acc.push_str(&x),
                Ident(src, name) => {
                    if !str_acc.is_empty() {
                        split_spaces_add_literals(&mut out, &str_acc);
                        str_acc.clear();
                    }
                    out.push(get_display_ref(sleigh, pattern, &name, &src)?)
                }
            }
        }
        if !str_acc.is_empty() {
            split_spaces_add_literals(&mut out, &str_acc);
        }

        //remove duplicated spaces in the middle
        let mut found_first_non_space = false;
        let mut last_was_space = false;
        let mut elements: Vec<_> = out
            .into_iter()
            //first first spaces
            .filter(|x| {
                //if mneumonic, don't remove spaces from the start
                if mneumonic.is_some() {
                    return true;
                }
                match (found_first_non_space, x.is_space()) {
                    //filter this starting space
                    (false, true) => false,
                    //found first non space, mark it, and pass it
                    (false, false) => {
                        found_first_non_space = true;
                        true
                    }
                    //after the first non starting space all others are allowed
                    (true, _) => true,
                }
            })
            //filter duplicated middle spaces
            .filter(|x| {
                match (last_was_space, x.is_space()) {
                    //last was space, remove duplicated
                    (true, true) => false,
                    //non space, after other non space, just let it pass
                    (false, false) => true,
                    //non space, after one or more spaces, let it pass, clean mark
                    (true, false) => {
                        last_was_space = false;
                        true
                    }
                    //found first space, let it pass, but mark so next dont
                    (false, true) => {
                        last_was_space = true;
                        true
                    }
                }
            })
            .collect();
        //remove the space at the end
        if elements
            .last()
            .map(DisplayElement::is_space)
            .unwrap_or(false)
        {
            elements.pop();
        }
        Ok(Display {
            mneumonic,
            elements,
        })
    }
}

fn split_spaces_add_literals(acc: &mut Vec<DisplayElement>, literal: &str) {
    //if the str start with space add it
    if literal.starts_with(char::is_whitespace) {
        acc.push(DisplayElement::Space);
    }
    //this may add an extra space at the end, but that will be removed next
    for literal in literal.split_whitespace() {
        acc.push(DisplayElement::Literal(literal.to_owned()));
        acc.push(DisplayElement::Space);
    }
    //if the str DONT ends with space, remove the one added previously
    if !literal.ends_with(char::is_whitespace) {
        acc.pop();
    }
}
