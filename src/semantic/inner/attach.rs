use std::rc::Rc;

use crate::semantic::meaning::Meaning;
use crate::semantic::{GlobalReference, SemanticError};
use crate::{syntax, Number, Span};

use super::{GlobalScope, PrintFlags, Sleigh, Varnode};

impl Sleigh {
    fn get_attach_from_field<'a>(
        &'a self,
        field: &str,
        src: &Span,
    ) -> Result<
        (std::cell::RefMut<'a, Option<Meaning>>, &PrintFlags),
        SemanticError,
    > {
        let (attach, flags) = match self
            .get_global(field)
            .ok_or(SemanticError::TokenFieldMissing)?
        {
            GlobalScope::TokenField(token_field) => {
                let attach = token_field.meaning_mut();
                let flags = &token_field.print_flags;
                (attach, flags)
            }
            GlobalScope::Context(context) => {
                let attach = context.meaning_mut();
                let flags = &context.print_flags;
                (attach, flags)
            }
            _ => return Err(SemanticError::TokenFieldInvalid),
        };
        if attach.is_some() {
            return Err(SemanticError::TokenFieldAttachDup(src.clone()));
        } else {
            Ok((attach, flags))
        }
    }
    fn attach_meaning_name(
        &self,
        names: Rc<[(usize, String)]>,
        fields: Vec<(String, Span)>,
    ) -> Result<(), SemanticError> {
        for (field_name, field_src) in fields.into_iter() {
            let (mut attach, flags) =
                self.get_attach_from_field(&field_name, &field_src)?;
            let meaning = Meaning::new_name(flags, &names)
                .ok_or(SemanticError::AttachWithPrintFlags(field_src))?;
            *attach = Some(meaning);
        }
        Ok(())
    }
    fn attach_meaning_number(
        &mut self,
        values: Rc<[(usize, Number)]>,
        fields: Vec<(String, Span)>,
    ) -> Result<(), SemanticError> {
        for (field_name, field_src) in fields.into_iter() {
            let (mut attach, flags) =
                self.get_attach_from_field(&field_name, &field_src)?;
            let meaning = Meaning::new_value(flags, &values)
                .ok_or(SemanticError::AttachWithPrintFlags(field_src))?;
            *attach = Some(meaning);
        }
        Ok(())
    }
    fn attach_meaning_variable(
        &mut self,
        variables: Rc<[(usize, GlobalReference<Varnode>)]>,
        fields: Vec<(String, Span)>,
    ) -> Result<(), SemanticError> {
        for (field_name, field_src) in fields.into_iter() {
            let (mut attach, flags) =
                self.get_attach_from_field(&field_name, &field_src)?;
            let meaning = Meaning::new_variable(flags, &variables)
                .ok_or(SemanticError::AttachWithPrintFlags(field_src))?;
            *attach = Some(meaning);
        }
        Ok(())
    }
    pub fn attach_meaning(
        &mut self,
        attach: syntax::attach::Attach,
    ) -> Result<(), SemanticError> {
        let syntax::attach::Attach { fields, meaning } = attach;
        match meaning {
            syntax::attach::Meaning::Variable(input_vars) => {
                let mut first_var = None;
                let vars: Vec<(usize, GlobalReference<Varnode>)> = input_vars
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, (name, s))| Some((i, name?, s)))
                    .map(|(index, var_name, var_src)| -> Result<_, SemanticError> {
                        let var_ele = self
                            .get_global(&var_name)
                            .ok_or(SemanticError::VarnodeMissing)?
                            .unwrap_varnode()
                            .ok_or(SemanticError::VarnodeInvalid)?;
                        //all vars need to have the same size
                        match first_var {
                            None => first_var = Some(var_ele.len_bytes),
                            Some(first) if first != var_ele.len_bytes => {
                                return Err(SemanticError::Satanic);
                            }
                            Some(_first) => (),
                        }
                        let var_ref =
                            GlobalReference::from_element(var_ele, var_src);
                        Ok((index, var_ref))
                    })
                    .collect::<Result<_, _>>()?;
                self.attach_meaning_variable(Rc::from(vars), fields)?;
            }
            syntax::attach::Meaning::Name(x) => {
                let names: Vec<(usize, String)> = x
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, (x, _s))| Some((i, x?)))
                    .collect();
                self.attach_meaning_name(Rc::from(names), fields)?;
            }
            syntax::attach::Meaning::Number(x) => {
                let values = x
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, (x, _s))| Some((i, x?)))
                    .collect::<Vec<_>>();
                self.attach_meaning_number(Rc::from(values), fields)?;
            }
        }
        Ok(())
    }
}
