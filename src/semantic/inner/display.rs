use std::rc::Rc;

use crate::semantic::display::DisplayError;
use crate::semantic::inner::disassembly;
use crate::semantic::inner::table::Table;
use crate::semantic::inner::token::TokenField;
use crate::semantic::varnode::Varnode;
use crate::semantic::{GlobalReference, InstNext, InstStart};
use crate::syntax::block;

use super::disassembly::Disassembly;
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

fn get_display_ref<'a>(
    sleigh: &Sleigh<'a>,
    pattern: &mut Pattern,
    disassembly: &Disassembly,
    name: &'a str,
) -> Result<DisplayElement, DisplayError> {
    use crate::semantic::inner::GlobalScope::*;
    let src = sleigh.input_src(name);
    disassembly
        .variable(name)
        .map(|var| Ok(DisplayElement::Dissasembly(Rc::clone(var))))
        .unwrap_or_else(|| {
            match sleigh
                .get_global(name)
                .ok_or(DisplayError::MissingRef(src.clone()))?
            {
                TokenField(x) => {
                    //TODO error here
                    let token_field = GlobalReference::from_element(x, src);
                    if !pattern.add_implicit_token_field(&token_field).unwrap()
                    {
                        todo!()
                    }
                    Ok(DisplayElement::TokenField(token_field))
                }
                Varnode(x) => Ok(DisplayElement::Varnode(
                    GlobalReference::from_element(x, src),
                )),
                Table(x) => Ok(DisplayElement::Table(
                    GlobalReference::from_element(x, src),
                )),
                Context(x) => Ok(DisplayElement::Context(
                    GlobalReference::from_element(x, src),
                )),
                _ => Err(DisplayError::InvalidRef(src)),
            }
        })
}

impl Display {
    pub fn new<'a>(
        display: block::display::Display<'a>,
        sleigh: &Sleigh<'a>,
        pattern: &mut Pattern,
        disassembly: &Disassembly,
        is_root: bool,
    ) -> Result<Self, DisplayError> {
        let mut out = vec![];
        let mut iter = display.0.into_iter();

        //in root table first element is the mneumonic
        if is_root {
            if let Some(ele) = iter.next() {
                use block::display::DisplayElement::*;
                match ele {
                    //no empty mneumonic
                    Concat => (),
                    Ident(x) => out.push(DisplayElement::Literal(x.to_owned())),
                    Literal(x) => {
                        out.push(DisplayElement::Literal(x.to_string()))
                    }
                }
            } else {
                return Ok(Self(out));
            }
        }

        for ele in iter {
            use block::display::DisplayElement::*;
            match ele {
                Concat => (),
                Literal(x) => out.push(DisplayElement::Literal(x.into_owned())),
                Ident(name) => out.push(get_display_ref(
                    sleigh,
                    pattern,
                    disassembly,
                    name,
                )?),
            }
        }
        Ok(Display(out))
    }
}
