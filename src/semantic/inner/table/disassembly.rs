use std::rc::Rc;

use crate::semantic::inner::disassembly::{
    AddrScope, Assertation, Disassembly, ReadScope, Variable, WriteScope,
};
use crate::semantic::inner::{disassembly, Sleigh};
use crate::semantic::varnode::{Varnode, VarnodeType};
use crate::semantic::DisassemblyError;

#[derive(Debug)]
pub struct Builder<'a, 'b> {
    sleigh: &'b Sleigh<'a>,
    disassembly: &'b mut Disassembly,
}

impl<'a, 'b> Builder<'a, 'b> {
    pub fn new(
        sleigh: &'b Sleigh<'a>,
        disassembly: &'b mut Disassembly,
    ) -> Self {
        Self {
            sleigh,
            disassembly,
        }
    }
}

impl<'a, 'b> disassembly::DisassemblyBuilder<'a> for Builder<'a, 'b> {
    fn addr_scope(&self, name: &'a str) -> Result<AddrScope, DisassemblyError> {
        use super::GlobalScope::*;
        //get from local, otherwise get from global
        let src = self.sleigh.input_src(name);
        self.disassembly
            .vars
            .get(name)
            .map(|local| Ok(AddrScope::Local(Rc::clone(local))))
            .unwrap_or_else(|| {
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(DisassemblyError::MissingRef(src.clone()))?
                {
                    Table(x) => Ok(AddrScope::Table(Rc::clone(x))),
                    Assembly(x) => Ok(AddrScope::Assembly(Rc::clone(x))),
                    Varnode(x) => Ok(AddrScope::Varnode(Rc::clone(x))),
                    _ => Err(DisassemblyError::InvalidRef(src)),
                }
            })
    }
    fn write_scope(
        &mut self,
        name: &'a str,
    ) -> Result<Rc<dyn WriteScope>, DisassemblyError> {
        //if variable exists, return it
        Ok(self
            .disassembly
            .vars
            .get(name)
            .map(|var| var.as_write())
            .or_else(|| {
                //check the global context, if context return it
                //NOTE if other thing with the same name, but is not context,
                //create the variable to shadow global context
                let varnode = self.sleigh.get_global(name)?.unwrap_varnode()?;
                match varnode.varnode_type {
                    VarnodeType::Memory(_) | VarnodeType::BitRange(_) => None,
                    VarnodeType::Context(_) => Some(varnode.as_write()),
                }
            })
            .unwrap_or_else(|| {
                //otherwise create the variable
                let src = self.sleigh.input_src(name);
                let var = Variable::new(name, src);
                self.disassembly
                    .vars
                    .insert(Rc::clone(var.name()), Rc::clone(&var));
                var.as_write()
            }))
    }
    fn context(&self, name: &'a str) -> Result<Rc<Varnode>, DisassemblyError> {
        let src = self.sleigh.input_src(name);
        let varnode = self
            .sleigh
            .get_global(name)
            .ok_or(DisassemblyError::MissingRef(src.clone()))?
            .varnode_or(DisassemblyError::InvalidRef(src.clone()))?;
        if !matches!(varnode.varnode_type, VarnodeType::Context(_)) {
            return Err(DisassemblyError::InvalidRef(src.clone()));
        }
        Ok(varnode)
    }

    fn insert_assertation(&mut self, ass: Assertation) {
        self.disassembly.assertations.push(ass)
    }
}

impl<'a, 'b> disassembly::ExprBuilder<'a> for Builder<'a, 'b> {
    fn read_scope(
        &self,
        name: &'b str,
    ) -> Result<Rc<dyn ReadScope>, DisassemblyError> {
        use super::GlobalScope::*;
        let src = self.sleigh.input_src(name);
        self.disassembly
            .vars
            .get(name)
            .map(|local| Ok(local.as_read()))
            .unwrap_or_else(|| {
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(DisassemblyError::MissingRef(src.clone()))?
                {
                    Assembly(x) => Ok(x.as_read()),
                    Varnode(x)
                        if matches!(
                            x.varnode_type,
                            VarnodeType::Context(_)
                        ) =>
                    {
                        Ok(x.as_read())
                    }
                    _ => Err(DisassemblyError::InvalidRef(src.clone())),
                }
            })
    }
}
