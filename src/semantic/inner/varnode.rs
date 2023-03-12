use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::semantic::meaning::Meaning;
use crate::semantic::varnode::Varnode;
use crate::semantic::{GlobalConvert, GlobalElement, SemanticError};
use crate::{syntax, NumberNonZeroUnsigned, NumberUnsigned, SleighError};
use crate::{BitRange, Span};

use super::{FieldSize, GlobalScope, PrintFlags, Sleigh};

#[derive(Debug)]
pub struct Context {
    pub src: Span,
    pub range: BitRange,
    pub varnode: GlobalElement<Varnode>,
    pub noflow_set: bool,
    pub attach_finish: Cell<bool>,
    pub print_flags: PrintFlags,
    meaning: RefCell<Option<Meaning>>,
    result: RefCell<Option<Rc<<Context as GlobalConvert>::FinalType>>>,
}

impl GlobalElement<Context> {
    pub(crate) fn new_context(
        name: &str,
        src: Span,
        range: BitRange,
        varnode: GlobalElement<Varnode>,
        noflow: bool,
        print_flags: PrintFlags,
    ) -> Self {
        Self::new_from(
            name,
            Context {
                src,
                range,
                varnode,
                noflow_set: noflow,
                attach_finish: Cell::new(false),
                print_flags,
                meaning: RefCell::new(None),
                result: RefCell::new(None),
            },
        )
    }
}

impl Context {
    //after the attach was read, it can't be modified again
    pub fn meaning(&self) -> std::cell::Ref<Option<Meaning>> {
        self.attach_finish.set(true);
        self.meaning.borrow()
    }
    pub fn meaning_mut(&self) -> std::cell::RefMut<Option<Meaning>> {
        assert!(!self.attach_finish.get());
        self.meaning.borrow_mut()
    }
    pub fn exec_out_value_bits(&self) -> FieldSize {
        match self.meaning().as_ref().map(Meaning::exec_len_bytes) {
            //have special meaning, and the meaning have a defined len
            Some(Some(len)) => FieldSize::new_bytes(len),
            //don't have speacial meaning, or the meaning just use the raw value
            Some(None) | None => {
                FieldSize::default().set_min(self.range.len()).unwrap()
            }
        }
    }
}

impl GlobalConvert for Context {
    type FinalType = crate::semantic::Context;

    fn convert(&self) -> Rc<Self::FinalType> {
        let result = self.result.borrow();
        match result.as_ref() {
            Some(result) => Rc::clone(result),
            None => {
                drop(result);
                let mut result = self.result.borrow_mut();
                //default to literal if unset
                let meaning = self.meaning.borrow();
                let meaning = meaning
                    .clone()
                    .unwrap_or(Meaning::Literal(self.print_flags.into()));
                let final_value = Self::FinalType {
                    location: self.src.clone(),
                    range: self.range.clone(),
                    varnode: self.varnode.clone(),
                    noflow: self.noflow_set,
                    meaning,
                };
                let final_element = Rc::new(final_value);
                *result = Some(Rc::clone(&final_element));
                final_element
            }
        }
    }
}

impl<'a> Sleigh {
    pub fn create_memory(
        &mut self,
        varnode: syntax::define::Varnode,
    ) -> Result<(), SleighError> {
        let space_ele = self
            .get_global(&varnode.space_name)
            .ok_or_else(|| {
                SleighError::SpaceUndefined(varnode.space_span.clone())
            })?
            .space_or(SleighError::SpaceInvalid(varnode.space_span.clone()))?
            .clone();
        let varnode_size = NumberNonZeroUnsigned::new(varnode.value_bytes)
            .ok_or_else(|| {
                SleighError::VarnodeInvalidSize(varnode.space_span.clone())
            })?;

        if varnode.names.is_empty() {
            //TODO verify that on syntax parsing?
            todo!("TODO ERROR here")
        }
        varnode
            .names
            .into_iter()
            .enumerate()
            .filter_map(|(i, (v, s))| Some((i, v?, s)))
            .try_for_each(|(index, name, src)| {
                let location = src.clone();
                let varnode = GlobalElement::new_varnode(
                    &name,
                    src,
                    index as NumberUnsigned * varnode_size.get(),
                    varnode_size,
                    space_ele.clone(),
                );
                self.insert_global(GlobalScope::Varnode(varnode), &location)
            })
    }
    pub fn create_bitrange(
        &mut self,
        bitrange: syntax::define::BitRangeDef,
    ) -> Result<(), SleighError> {
        bitrange.into_iter().try_for_each(|field| {
            let varnode = self
                .get_global(&field.varnode_name)
                .ok_or_else(|| {
                    SleighError::VarnodeUndefined(field.src.clone())
                })?
                .varnode_or(SleighError::VarnodeInvalid(field.src.clone()))?;
            let range: BitRange = field.range.try_into()?;

            //bitrange need to point to a varnode, we allow anything that have a
            //value_size, but tecnicatly we need to verify if this point to a
            //varnode, but I'm not doing this right now...
            let varnode_size = varnode.len_bytes.get() * 8;
            //bitrange can't be bigger than the varnode
            if range.field_min_len().get() > varnode_size {
                return Err(SleighError::VarnodeInvalidSize(field.src.clone()));
            }

            let bitrange = GlobalElement::new_bitrange(
                &field.name,
                field.src.clone(),
                range,
                varnode.clone(),
            );

            self.insert_global(GlobalScope::Bitrange(bitrange), &field.src)
        })
    }
    pub fn create_user_function(
        &mut self,
        input: syntax::define::UserFunction,
    ) -> Result<(), SleighError> {
        let user = GlobalScope::UserFunction(GlobalElement::new_user_function(
            &input.name,
            input.src.clone(),
        ));
        self.insert_global(user, &input.src)
    }
    pub fn create_context(
        &mut self,
        input: syntax::define::Context,
    ) -> Result<(), SleighError> {
        let varnode = self
            .get_global(&input.varnode_name)
            .ok_or(SemanticError::VarnodeMissing)?
            .varnode_or(SemanticError::VarnodeInvalid)?
            .clone();
        input.fields.into_iter().try_for_each(|field| {
            //check for valid range
            let range: BitRange = field.range.try_into()?;
            //don't need make checked add/sub, don't question it
            //range can't be bigger than the varnode
            let varnode_size = varnode.len_bytes.get() * 8;
            if range.field_min_len().get() > varnode_size {
                return Err(SleighError::ContextInvalidSize(field.src.clone()));
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
                _ => return Err(SleighError::ContextAttDup(field.src.clone())),
            };
            //default to hex print fmt
            let context = GlobalElement::new_context(
                &field.name,
                field.src.clone(),
                range,
                varnode.clone(),
                noflow_set,
                print_flags,
            );
            self.insert_global(GlobalScope::Context(context), &field.src)
        })
    }
}
