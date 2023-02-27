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
}

impl From<DisplayElement> for crate::semantic::display::DisplayScope {
    fn from(value: DisplayElement) -> Self {
        match value {
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
pub struct Display(pub Vec<DisplayElement>);

impl From<Display> for crate::semantic::display::Display {
    fn from(value: Display) -> Self {
        let elements = value.0.into_iter().map(|x| x.into()).collect();
        Self::new(elements)
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

        let mut out = vec![];
        let mut iter = display.0.into_iter();

        //in root table first element is the mneumonic
        if is_root {
            if let Some(ele) = iter.next() {
                match ele {
                    //no empty mneumonic
                    Ident(_src, x) => {
                        out.push(DisplayElement::Literal(x.to_owned()))
                    }
                    Literal(_src, x) => {
                        out.push(DisplayElement::Literal(x.to_string()))
                    }
                }
            } else {
                return Ok(Self(out));
            }
        }

        for ele in iter {
            match ele {
                Literal(_src, x) => out.push(DisplayElement::Literal(x)),
                Ident(src, name) => {
                    out.push(get_display_ref(sleigh, pattern, &name, &src)?)
                }
            }
        }
        Ok(Display(out))
    }
}
