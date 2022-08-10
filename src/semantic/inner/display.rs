use std::rc::Rc;

use crate::semantic;
use crate::semantic::display::DisplayError;
use crate::semantic::inner::assembly::Assembly;
use crate::semantic::inner::disassembly;
use crate::semantic::inner::table::Table;
use crate::semantic::varnode::Varnode;
use crate::syntax::block;

use super::disassembly::Disassembly;
use super::Sleigh;

#[derive(Clone, Debug)]
pub enum DisplayElement {
    Assembly(Rc<Assembly>),
    Varnode(Rc<Varnode>),
    Table(Rc<Table>),
    Dissasembly(Rc<disassembly::Variable>),
    Literal(String),
}

impl From<DisplayElement> for semantic::display::Element {
    fn from(value: DisplayElement) -> Self {
        match value {
            DisplayElement::Assembly(x) => Self::Assembly(x),
            DisplayElement::Varnode(x) => Self::Varnode(x),
            DisplayElement::Literal(x) => Self::Literal(x),
            DisplayElement::Table(x) => Self::Table(x.reference()),
            DisplayElement::Dissasembly(x) => Self::Disassembly(x.convert()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Display(pub Vec<DisplayElement>);

impl From<Display> for semantic::display::Display {
    fn from(mut value: Display) -> Self {
        semantic::display::Display {
            elements: value.0.drain(..).map(|x| x.into()).collect(),
        }
    }
}

fn get_display_ref<'a>(
    sleigh: &Sleigh<'a>,
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
                Assembly(x) => Ok(DisplayElement::Assembly(Rc::clone(x))),
                Varnode(x) => Ok(DisplayElement::Varnode(Rc::clone(x))),
                Table(x) => Ok(DisplayElement::Table(Rc::clone(x))),
                _ => Err(DisplayError::InvalidRef(src)),
            }
        })
}

impl Display {
    pub fn new<'a>(
        mut display: block::display::Display<'a>,
        sleigh: &Sleigh<'a>,
        disassembly: &Disassembly,
        is_root: bool,
    ) -> Result<Self, DisplayError> {
        let mut out = vec![];
        let mut iter = display.0.drain(..);

        //in root table first element is the mneumonic
        if is_root {
            if let Some(ele) = iter.next() {
                use block::display::DisplayElement::*;
                match ele {
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
                Literal(x) => out.push(DisplayElement::Literal(x.into_owned())),
                Ident(name) => {
                    out.push(get_display_ref(sleigh, disassembly, name)?)
                }
            }
        }
        Ok(Display(out))
    }
}
