use crate::semantic::meaning::Meaning;
use crate::semantic::varnode::ContextAttach;
use crate::semantic::Context as FinalContext;
use crate::semantic::{
    syntax, Bitrange, BitrangeId, ContextId, GlobalScope, UserFunction,
    UserFunctionId, Varnode, VarnodeId,
};
use crate::{FieldBits, NumberNonZeroUnsigned, NumberUnsigned, SleighError};

use super::execution::FieldSize;
use super::{PrintFlags, Sleigh};

#[derive(Debug)]
pub struct Context {
    pub name: String,
    pub bitrange: Bitrange,
    pub noflow_set: bool,
    pub print_flags: PrintFlags,
    pub attach: Option<ContextAttach>,
}

impl Context {
    pub fn attach(&mut self, meaning: Meaning) -> Result<(), Box<SleighError>> {
        if self.attach.is_some() {
            //once attached, can't attach again
            return Err(Box::new(SleighError::AttachMultiple(
                self.bitrange.location.clone(),
            )));
        }
        if self.print_flags.signed_set {
            todo!("Is allowed to attach to signed value?");
        }
        if meaning.is_number() && self.print_flags.base.is_some() {
            todo!("Is allowed to attach varnode/literal if base is set?");
        }
        let attach = match meaning {
            Meaning::NoAttach(_) => unreachable!(),
            Meaning::Varnode(x) => ContextAttach::Varnode(x),
            Meaning::Literal(x) => ContextAttach::Literal(x),
            Meaning::Number(_, _x) => {
                // attach to number is uncessary, probably a mistake
                return Err(Box::new(SleighError::ContextAttachNumber(
                    self.bitrange.location.clone(),
                )));
            }
        };
        //TODO what if print_flags are set?
        self.attach = Some(attach);
        Ok(())
    }

    pub fn exec_out_value_bits(&self, sleigh: &Sleigh) -> FieldSize {
        match self.attach {
            //don't have speacial meaning, or the meaning just use the raw value
            Some(ContextAttach::NoAttach(_)) => unreachable!(),
            Some(ContextAttach::Literal(_)) | None => FieldSize::default()
                .set_min_bits(self.bitrange.bits.len())
                .unwrap(),
            Some(ContextAttach::Varnode(attach_id)) => {
                let varnodes = sleigh.attach_varnode(attach_id);
                //all varnodes have the same len
                varnodes.execution_len(sleigh)
            }
        }
    }

    pub fn convert(self) -> FinalContext {
        //default to literal if unset
        let attach = self
            .attach
            .unwrap_or(ContextAttach::NoAttach(self.print_flags.into()));
        FinalContext {
            name: self.name.into(),
            bitrange: self.bitrange,
            noflow: self.noflow_set,
            attach,
        }
    }
}

impl Sleigh {
    pub fn create_memory(
        &mut self,
        varnode: syntax::define::Varnode,
    ) -> Result<(), Box<SleighError>> {
        let space = self
            .get_global(&varnode.space_name)
            .ok_or_else(|| {
                Box::new(SleighError::SpaceUndefined(
                    varnode.space_span.clone(),
                ))
            })?
            .space()
            .ok_or_else(|| {
                Box::new(SleighError::SpaceInvalid(varnode.space_span.clone()))
            })?;
        let varnode_bytes = NumberNonZeroUnsigned::new(varnode.value_bytes)
            .ok_or_else(|| {
                Box::new(SleighError::VarnodeInvalidSize(
                    varnode.space_span.clone(),
                ))
            })?;

        if varnode.names.is_empty() {
            //TODO verify that on syntax parsing?
            todo!("TODO ERROR here")
        }
        for (index, (varnode_name, location)) in
            varnode.names.into_iter().enumerate()
        {
            let Some(varnode_name) = varnode_name else {
                // no varnode at this address, next one
                continue;
            };
            let address = index as NumberUnsigned * varnode_bytes.get();
            let location = location.clone();
            let varnode = Varnode {
                name: varnode_name.clone().into(),
                location,
                address,
                len_bytes: varnode_bytes,
                space,
            };
            self.varnodes.push(varnode);
            let varnode_id = VarnodeId(self.varnodes.len() - 1);
            self.global_scope
                .insert(varnode_name, GlobalScope::Varnode(varnode_id))
                .map(|_| Err(Box::new(SleighError::NameDuplicated)))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
    pub fn create_bitrange(
        &mut self,
        bitrange: syntax::define::BitRangeDef,
    ) -> Result<(), Box<SleighError>> {
        for field in bitrange.into_iter() {
            let varnode_id = self
                .get_global(&field.varnode_name)
                .ok_or_else(|| {
                    Box::new(SleighError::VarnodeUndefined(field.src.clone()))
                })?
                .varnode()
                .ok_or_else(|| {
                    Box::new(SleighError::VarnodeInvalid(field.src.clone()))
                })?;
            let varnode = self.varnode(varnode_id);
            let bits: FieldBits = field.range.try_into()?;

            //bitrange need to point to a varnode, we allow anything that have a
            //value_size, but tecnicatly we need to verify if this point to a
            //varnode, but I'm not doing this right now...
            let varnode_size = varnode.len_bytes.get() * 8;
            //bitrange can't be bigger than the varnode
            if bits.field_min_len().get() > varnode_size {
                return Err(Box::new(SleighError::VarnodeInvalidSize(
                    field.src.clone(),
                )));
            }

            let bitrange = Bitrange {
                location: field.src,
                bits,
                varnode: varnode_id,
            };

            self.bitranges.push(bitrange);
            let bitrange_id = BitrangeId(self.bitranges.len() - 1);
            self.global_scope
                .insert(field.name, GlobalScope::Bitrange(bitrange_id))
                .map(|_| Err(Box::new(SleighError::NameDuplicated)))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
    pub fn create_user_function(
        &mut self,
        input: syntax::define::UserFunction,
    ) -> Result<(), Box<SleighError>> {
        let user_function =
            UserFunction::new(input.name.clone().into(), input.src);
        self.user_functions.push(user_function);
        let user_function_id = UserFunctionId(self.user_functions.len() - 1);
        self.global_scope
            .insert(input.name, GlobalScope::UserFunction(user_function_id))
            .map(|_| Err(Box::new(SleighError::NameDuplicated)))
            .unwrap_or(Ok(()))
    }
    pub fn create_context(
        &mut self,
        input: syntax::define::Context,
    ) -> Result<(), Box<SleighError>> {
        let varnode_id = self
            .get_global(&input.varnode_name)
            .ok_or_else(|| {
                Box::new(SleighError::VarnodeUndefined(
                    input.varnode_span.clone(),
                ))
            })?
            .varnode()
            .ok_or_else(|| {
                Box::new(SleighError::VarnodeInvalid(
                    input.varnode_span.clone(),
                ))
            })?;
        let varnode_len_bits = self.varnode(varnode_id).len_bytes.get() * 8;
        for field in input.fields.into_iter() {
            //check for valid range
            let bits: FieldBits = field.range.try_into()?;
            //don't need make checked add/sub, don't question it
            //range can't be bigger than the varnode
            if bits.field_min_len().get() > varnode_len_bits {
                return Err(Box::new(SleighError::ContextInvalidSize(
                    field.src.clone(),
                )));
            }
            let print_flags = PrintFlags::from_token_att(
                &field.src,
                field.attributes.iter().filter_map(|att| match att {
                    syntax::define::ContextFieldAttribute::Token(att) => {
                        Some(att)
                    }
                    syntax::define::ContextFieldAttribute::Noflow => None,
                }),
            )?;
            //make sure that multiple noflow declaration is detected
            let noflow = field
                .attributes
                .iter()
                .filter(|att| {
                    matches!(att, syntax::define::ContextFieldAttribute::Noflow)
                })
                .count();
            let noflow_set = match noflow {
                0 => false,
                1 => true,
                _ => {
                    return Err(Box::new(SleighError::ContextAttDup(
                        field.src.clone(),
                    )))
                }
            };

            //default to hex print fmt
            let context = Context {
                name: field.name.clone(),
                bitrange: Bitrange {
                    location: field.src,
                    bits,
                    varnode: varnode_id,
                },
                noflow_set,
                print_flags,
                attach: None,
            };
            self.contexts.push(context);
            let context_id = ContextId(self.contexts.len() - 1);
            self.global_scope
                .insert(field.name, GlobalScope::Context(context_id))
                .map(|_| Err(Box::new(SleighError::NameDuplicated)))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
}
