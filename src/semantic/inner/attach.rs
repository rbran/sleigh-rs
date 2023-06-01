use crate::semantic::meaning::{
    AttachLiteral, AttachNumber, AttachVarnode, Meaning,
};
use crate::semantic::{
    AttachLiteralId, AttachNumberId, AttachVarnodeId, PrintBase, VarnodeId,
};
use crate::{syntax, SleighError, Span};

use super::{GlobalScope, Sleigh};

impl Sleigh {
    fn attach_meaning_fields(
        &mut self,
        fields: Vec<(String, Span)>,
        meaning: Meaning,
    ) -> Result<(), SleighError> {
        for (field_name, field_src) in fields.into_iter() {
            match self.get_global(&field_name) {
                Some(GlobalScope::TokenField(token_field)) => {
                    let token_field = self.token_field_mut(token_field);
                    token_field.attach(meaning)?;
                }
                Some(GlobalScope::Context(context)) => {
                    let context = self.context_mut(context);
                    context.attach(meaning)?;
                }
                _ => {
                    return Err(SleighError::AttachInvalidVariable(
                        field_src.clone(),
                    ))
                }
            }
        }
        Ok(())
    }
    pub fn attach_meaning(
        &mut self,
        attach: syntax::attach::Attach,
    ) -> Result<(), SleighError> {
        let syntax::attach::Attach {
            src,
            fields,
            meaning,
        } = attach;
        match meaning {
            syntax::attach::Meaning::Variable(input_vars) => {
                let vars: Box<[(usize, VarnodeId)]> = input_vars
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, (name, s))| Some((i, name?, s)))
                    .map(
                        |(index, var_name, var_src)| -> Result<_, SleighError> {
                            let var_id = self
                                .get_global(&var_name)
                                .ok_or_else(|| {
                                    SleighError::VarnodeUndefined(
                                        var_src.clone(),
                                    )
                                })?
                                .varnode()
                                .ok_or_else(|| {
                                    SleighError::VarnodeInvalid(var_src.clone())
                                })?;
                            Ok((index, var_id))
                        },
                    )
                    .collect::<Result<_, _>>()?;
                // all varnodes need to have the same size
                let mut var_iter =
                    vars.iter().map(|(_i, var)| self.varnode(*var).len_bytes);
                let varnode_len = var_iter.next().unwrap();
                for var in var_iter {
                    if var != varnode_len {
                        return Err(SleighError::VarnodeInvalid(src.clone()));
                    }
                }
                self.attach_varnodes.push(AttachVarnode(vars));
                let meaning = Meaning::Varnode(AttachVarnodeId(
                    self.attach_varnodes.len() - 1,
                ));
                self.attach_meaning_fields(fields, meaning)?;
            }
            syntax::attach::Meaning::Name(x) => {
                let names = x
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, (x, _s))| Some((i, x?)))
                    .collect();
                self.attach_literals.push(AttachLiteral(names));
                let meaning = Meaning::Literal(AttachLiteralId(
                    self.attach_literals.len() - 1,
                ));
                self.attach_meaning_fields(fields, meaning)?;
            }
            syntax::attach::Meaning::Number(x) => {
                let values = x
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, (x, _s))| Some((i, x?)))
                    .collect();
                self.attach_numbers.push(AttachNumber(values));
                let meaning = Meaning::Number(
                    PrintBase::Hex,
                    AttachNumberId(self.attach_numbers.len() - 1),
                );
                self.attach_meaning_fields(fields, meaning)?;
            }
        }
        Ok(())
    }
}
