use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::base::NonZeroTypeU;
use crate::semantic::meaning::Meaning;
use crate::semantic::{GlobalConvert, GlobalElement, SemanticError};
use crate::syntax::define;
use crate::{InputSource, IntTypeU, RangeBits, Varnode};

use super::{FieldSize, GlobalScope, PrintFlags, Sleigh};

#[derive(Debug)]
pub struct Context {
    pub src: InputSource,
    pub range: RangeBits,
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
        src: InputSource,
        range: RangeBits,
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
                FieldSize::default().set_min(self.range.n_bits).unwrap()
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
                    range: self.range,
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

impl<'a> Sleigh<'a> {
    pub fn create_memory(
        &mut self,
        varnode: define::Varnode<'a>,
    ) -> Result<(), SemanticError> {
        let space_ele = self
            .get_global(varnode.space_name)
            .ok_or(SemanticError::SpaceMissing)?
            .space_or(SemanticError::SpaceInvalid)?
            .clone();
        //let offset_start = varnode.offset;
        let varnode_size = NonZeroTypeU::new(varnode.value_bytes)
            .ok_or(SemanticError::VarnodeInvalidVarnodeSize)?;

        if varnode.names.is_empty() {
            //TODO verify that on syntax parsing?
            todo!("TODO ERROR here")
        }
        varnode
            .names
            .into_iter()
            .enumerate()
            .filter_map(|(i, v)| Some((i, v?)))
            .try_for_each(|(index, name)| {
                let varnode = GlobalElement::new_varnode(
                    name,
                    self.input_src(name),
                    index as IntTypeU * varnode_size.get(),
                    varnode_size,
                    space_ele.clone(),
                );
                self.insert_global(GlobalScope::Varnode(varnode))
            })
    }
    pub fn create_bitrange(
        &mut self,
        bitrange: define::BitRangeDef<'a>,
    ) -> Result<(), SemanticError> {
        bitrange.into_iter().try_for_each(|field| {
            let src = self.input_src(field.name);
            let varnode = self
                .get_global(&field.varnode_name)
                .ok_or(SemanticError::VarnodeMissing)?
                .varnode_or(SemanticError::VarnodeInvalid)?;
            let range = RangeBits::from_syntax(field.range)
                .ok_or(SemanticError::BitrangeInvalidSize)?;

            //bitrange need to point to a varnode, we allow anything that have a
            //value_size, but tecnicatly we need to verify if this point to a
            //varnode, but I'm not doing this right now...
            let varnode_size = varnode.len_bytes.get() * 8;
            //bitrange can't be bigger than the varnode
            let last_bit = range
                .lsb_bit
                .checked_add(range.n_bits.get())
                .ok_or(SemanticError::BitrangeInvalidSize)?;
            if last_bit > varnode_size {
                return Err(SemanticError::BitrangeInvalidVarnodeSize);
            }

            let bitrange = GlobalElement::new_bitrange(
                &field.name,
                src,
                range,
                varnode.clone(),
            );

            self.insert_global(GlobalScope::Bitrange(bitrange))
        })
    }
    pub fn create_user_function(
        &mut self,
        input: define::UserFunction<'a>,
    ) -> Result<(), SemanticError> {
        let src = self.input_src(input.0);
        self.insert_global(GlobalScope::UserFunction(
            GlobalElement::new_user_function(input.0, src),
        ))
    }
    pub fn create_context(
        &mut self,
        input: define::Context<'a>,
    ) -> Result<(), SemanticError> {
        let varnode = self
            .get_global(input.varnode_name)
            .ok_or(SemanticError::VarnodeMissing)?
            .varnode_or(SemanticError::VarnodeInvalid)?
            .clone();
        input.fields.into_iter().try_for_each(|field| {
            let src = self.input_src(&field.name);
            //check for valid range
            if field.start > field.end {
                return Err(SemanticError::ContextInvalidSize);
            }
            let lsb_bit = field.start;
            //don't need make checked add/sub, don't question it
            let n_bits = NonZeroTypeU::new((field.end - field.start) + 1)
                .ok_or(SemanticError::ContextInvalidSize)?;
            //range can't be bigger than the varnode
            let varnode_size = varnode.len_bytes.get() * 8;
            if field.end > varnode_size {
                return Err(SemanticError::ContextInvalidSize);
            }
            let print_flags = PrintFlags::from_token_att(
                &src,
                field.attributes.iter().filter_map(|att| match att {
                    define::ContextFieldAttribute::Token(att) => Some(att),
                    define::ContextFieldAttribute::Noflow => None,
                }),
            )?;
            //make sure that multiple noflow declaration is detected
            let noflow = field
                .attributes
                .iter()
                .filter(|att| {
                    matches!(att, define::ContextFieldAttribute::Noflow)
                })
                .count();
            let noflow_set = match noflow {
                0 => false,
                1 => true,
                _ => return Err(SemanticError::ContextInvalidAtt),
            };
            //default to hex print fmt
            let context = GlobalElement::new_context(
                &field.name,
                src,
                RangeBits::new(lsb_bit, n_bits),
                varnode.clone(),
                noflow_set,
                print_flags,
            );
            self.insert_global(GlobalScope::Context(context))
        })
    }
}
