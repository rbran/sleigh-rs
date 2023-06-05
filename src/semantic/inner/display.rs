use crate::semantic::display::{Display, DisplayElement};
use crate::syntax::block;
use crate::{DisplayError, Span};

use super::{Pattern, Sleigh};

impl Sleigh {
    fn get_display_ref(
        &self,
        pattern: &mut Pattern,
        name: &str,
        src: &Span,
    ) -> Result<DisplayElement, DisplayError> {
        use crate::semantic::inner::GlobalScope::*;
        if let Some(disassembly_var) =
            pattern.disassembly_variable_names.get(name)
        {
            return Ok(DisplayElement::Disassembly(*disassembly_var));
        }
        match self
            .get_global(name)
            .ok_or(DisplayError::MissingRef(src.clone()))?
        {
            TokenField(x) => {
                if pattern
                    .produce_token_field(self, x)
                    .map(|block_num| block_num.is_none())
                    .unwrap()
                {
                    //TODO error here
                    todo!()
                }
                Ok(DisplayElement::TokenField(x))
            }
            Varnode(x) => Ok(DisplayElement::Varnode(x)),
            Table(x) => Ok(DisplayElement::Table(x)),
            Context(x) => Ok(DisplayElement::Context(x)),
            _ => Err(DisplayError::InvalidRef(src.clone())),
        }
    }

    pub fn new_display(
        &self,
        display: block::display::Display,
        pattern: &mut Pattern,
        is_root: bool,
    ) -> Result<Display, DisplayError> {
        use block::display::DisplayElement::*;
        //solve the idents
        let mut elements = vec![];
        let mut iter = display.0.into_iter();
        //in root table first element is the mneumonic
        let mneumonic = if !is_root {
            None
        } else {
            let Some(ele) = iter.next() else {
                return Ok(Display::default());
            };
            match ele {
                Concat => None,
                Ident(_src, mneumonic) => Some(mneumonic),
                //TODO: a single char can be a mneumonic?
                Other(c) => Some(c.to_string()),
                Literal(x) => {
                    elements.push(DisplayElement::Literal(x));
                    None
                }
            }
        };

        for ele in iter {
            match ele {
                Concat => (),
                Ident(src, name) => {
                    elements.push(self.get_display_ref(pattern, &name, &src)?)
                }
                // Multiple spaces should be combined into a single one
                // NOTE: a literal with a space in the end, followed by
                // Other(' ') should NOT be condensated.
                Other(x) if x.is_whitespace() => match elements.last() {
                    // spaces should be trimmed at the start
                    None if mneumonic.is_none() => {}
                    // if last was space, don't add other one
                    Some(DisplayElement::Space) => {}
                    Some(_) | None => elements.push(DisplayElement::Space),
                },
                // if the previous entry was a literal, just add to it.
                Literal(value) => match elements.last_mut() {
                    Some(DisplayElement::Literal(lit)) => lit.push_str(&value),
                    _ => elements.push(DisplayElement::Literal(value)),
                },
                Other(c) => match elements.last_mut() {
                    Some(DisplayElement::Literal(lit)) => lit.push(c),
                    _ => elements.push(DisplayElement::Literal(c.into())),
                },
            }
        }

        //remove the space at the end
        if matches!(elements.last().map(DisplayElement::is_space), Some(true)) {
            elements.pop();
        }
        Ok(Display {
            mneumonic,
            elements: elements.into(),
        })
    }
}
