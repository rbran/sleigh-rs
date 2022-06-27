use std::rc::Rc;

use crate::base::NonZeroTypeU;
use crate::semantic::{inner, space, varnode, PrintFmt, SemanticError};
use crate::syntax::define;
use crate::BitRange;

use super::Sleigh;

impl<'a> Sleigh<'a> {
    pub fn create_memory(
        &mut self,
        mut varnode: define::Varnode<'a>,
    ) -> Result<(), SemanticError> {
        let space: Rc<space::Space> = self
            .get_global(varnode.space_name)
            .ok_or(SemanticError::SpaceMissing)?
            .space_or(SemanticError::SpaceInvalid)?;
        let offset_start = varnode.offset;
        let varnode_size = varnode.varnode_size;
        if varnode_size == 0 {
            return Err(SemanticError::VarnodeInvalidVarnodeSize);
        }
        let mem_size = varnode_size
            .checked_mul(varnode.names.len() as u64)
            .ok_or(SemanticError::VarnodeInvalidMemorySize)?;
        let _offset_end = offset_start
            .checked_add(mem_size)
            .ok_or(SemanticError::VarnodeInvalidMemoryEnd)?;

        if varnode.names.is_empty() {
            //TODO verify that on syntax parsing?
            todo!("TODO ERROR here")
        }
        (offset_start..)
            .into_iter()
            .step_by(varnode_size.try_into().unwrap(/*TODO interate on u64*/))
            .zip(varnode.names.drain(..))
            .filter_map(|(offset, name)| Some((offset, name?)))
            .map(|(offset, name)| {
                varnode::Varnode::new_memory(
                    name,
                    //calculate using u64 to avoid unwrap???
                    offset.try_into().unwrap(),
                    varnode_size.try_into().unwrap(),
                    &space,
                )
            })
            .find_map(|varnode| {
                self.idents.insert(
                    Rc::clone(&varnode.name),
                    inner::GlobalScope::Varnode(varnode),
                )
            })
            .map(|_| Err(SemanticError::NameDuplicated))
            .unwrap_or(Ok(()))
    }
    pub fn create_bitrange(
        &mut self,
        bitrange: define::BitRangeDef<'a>,
    ) -> Result<(), SemanticError> {
        for field in bitrange.into_iter() {
            let varnode: Rc<varnode::Varnode> = self
                .get_global(field.varnode_name)
                .ok_or(SemanticError::VarnodeMissing)?
                .varnode_or(SemanticError::VarnodeInvalid)?;
            let range = BitRange::from_syntax(field.range)
                .ok_or(SemanticError::BitrangeInvalidSize)?;

            //bitrange need to point to a varnode, we allow anything that have a
            //value_size, but tecnicatly we need to verify if this point to a
            //varnode, but I'm not doing this right now...
            let varnode_size = varnode.value_bits();
            //bitrange can't be bigger than the varnode
            let last_bit = range
                .lsb_bit
                .checked_add(range.n_bits.get())
                .ok_or(SemanticError::BitrangeInvalidSize)?;
            if last_bit > varnode_size.get() {
                return Err(SemanticError::BitrangeInvalidVarnodeSize);
            }

            let bitrange =
                varnode::Varnode::new_bitrange(&field.name, range, varnode);

            self.idents
                .insert(
                    Rc::clone(&bitrange.name),
                    inner::GlobalScope::Varnode(bitrange),
                )
                .map(|_| Err(SemanticError::NameDuplicated))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
    pub fn create_context(
        &mut self,
        mut context: define::Context<'a>,
    ) -> Result<(), SemanticError> {
        let varnode = self
            .get_global(context.varnode_name)
            .ok_or(SemanticError::VarnodeMissing)?
            .varnode_or(SemanticError::VarnodeInvalid)?;
        for mut field in context.fields.drain(..) {
            //check for valid range
            if field.start > field.end {
                return Err(SemanticError::ContextInvalidSize);
            }
            let lsb_bit = field.start;
            //don't need make checked add/sub, don't question it
            let n_bits = NonZeroTypeU::new((field.end - field.start) + 1)
                .ok_or(SemanticError::ContextInvalidSize)?;
            //range can't be bigger than the varnode
            let varnode_size = varnode.value_bits();
            if field.end > varnode_size.get() {
                return Err(SemanticError::ContextInvalidSize);
            }
            let (mut fmt, mut signed, mut noflow) = (None, false, false);
            for att in field.attributes.drain(..) {
                use define::ContextFieldAttribute::*;
                match att {
                    PrintFmt(x) if fmt.is_none() => fmt = Some(x),
                    Signed if !signed => signed = true,
                    Noflow if !noflow => noflow = true,
                    _ => return Err(SemanticError::ContextInvalidAtt),
                }
            }
            let context = varnode::Varnode::new_context(
                &field.name,
                BitRange::new(lsb_bit, n_bits),
                Rc::clone(&varnode),
                fmt.unwrap_or(PrintFmt::Hex),
                signed,
                noflow,
            );
            self.idents
                .insert(
                    Rc::clone(&context.name),
                    inner::GlobalScope::Varnode(context),
                )
                .map(|_| Err(SemanticError::NameDuplicated))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
}
