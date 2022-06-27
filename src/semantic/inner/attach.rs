use std::rc::Rc;

use crate::semantic::{Meaning, SemanticError};
use crate::syntax::attach;

use super::{Sleigh, Varnode};

impl<'a> Sleigh<'a> {
    pub fn create_attach(
        &mut self,
        attach: attach::Attach<'a>,
    ) -> Result<(), SemanticError> {
        let meaning = match attach.meaning {
            attach::Meaning::Variable(mut x) => {
                let vars: Vec<Option<Rc<Varnode>>> = x
                    .drain(..)
                    .map(|var| {
                        var.map(|var| {
                            self.get_global(var)
                                .ok_or(SemanticError::VarnodeMissing)?
                                .unwrap_varnode()
                                .ok_or(SemanticError::VarnodeInvalid)
                        })
                        .transpose()
                    })
                    .collect::<Result<_, _>>()?;
                //all vars need to have the same size
                let mut size_iter = vars
                    .iter()
                    .filter_map(|x| x.as_ref().map(|x| x.value_bits()));
                let size = size_iter.next().ok_or(SemanticError::Satanic)?;
                for current_size in size_iter {
                    if current_size != size {
                        return Err(SemanticError::Satanic);
                    }
                }
                Meaning::Variable { vars, size }
            }
            attach::Meaning::Name(x) => Meaning::Name(x),
            attach::Meaning::Number(x) => Meaning::Value(x),
        };
        let meaning = Rc::new(meaning);
        for field_name in attach.fields.iter() {
            let attach = match self
                .get_global(field_name)
                .ok_or(SemanticError::TokenFieldMissing)?
            {
                super::GlobalScope::Assembly(assembly) => {
                    let field = assembly
                        .field()
                        .ok_or(SemanticError::TokenFieldInvalid)?;
                    &field.attach
                }
                super::GlobalScope::Varnode(var) if var.context().is_some() => {
                    let context = var.context().unwrap();
                    &context.attach
                }
                _ => return Err(SemanticError::TokenFieldInvalid),
            };
            attach
                .borrow_mut()
                .replace(Rc::clone(&meaning))
                .map(|_| Err(SemanticError::TokenFieldAttachDup))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
}
