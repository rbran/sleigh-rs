use std::rc::Rc;

use crate::base::Value;
use crate::semantic::inner::disassembly::{
    AddrScope, Assertation, Assignment, Disassembly, ExprBuilder, GlobalSet,
    ReadScope, Variable, WriteScope,
};
use crate::semantic::inner::{disassembly, Pattern, Sleigh};
use crate::semantic::varnode::{Varnode, VarnodeType};
use crate::semantic::DisassemblyError;
use crate::syntax::block;

impl<'a> Sleigh<'a> {
    pub(crate) fn table_disassembly(
        &self,
        pattern: &mut Pattern,
        disassembly: block::disassembly::Disassembly<'a>,
    ) -> Result<Disassembly, DisassemblyError> {
        let mut builder = Builder {
            sleigh: self,
            pattern,
            output: Disassembly::default(),
        };
        builder.build(disassembly)?;
        Ok(builder.output)
    }
}

#[derive(Debug)]
pub struct Builder<'a, 'b> {
    sleigh: &'b Sleigh<'a>,
    pattern: &'a mut Pattern,
    output: Disassembly,
}

impl<'a, 'b> Builder<'a, 'b> {
    fn addr_scope(
        &mut self,
        name: &'a str,
    ) -> Result<AddrScope, DisassemblyError> {
        use super::GlobalScope::*;
        //get from local, otherwise get from global
        let src = self.sleigh.input_src(name);
        self.output
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
        let var = self
            .output
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
                self.output
                    .vars
                    .insert(Rc::clone(var.name()), Rc::clone(&var));
                var.as_write()
            });
        Ok(var)
    }
    fn context(
        &mut self,
        name: &'a str,
    ) -> Result<Rc<Varnode>, DisassemblyError> {
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
    fn new_globalset(
        &mut self,
        input: block::disassembly::GlobalSet<'a>,
    ) -> Result<GlobalSet, DisassemblyError> {
        let address = match input.address {
            Value::Number(_, int) => AddrScope::Int(int),
            Value::Ident(ident) => self.addr_scope(ident)?,
        };
        let context = self.context(input.context)?;
        Ok(GlobalSet { address, context })
    }
    fn new_assignment(
        &mut self,
        input: block::disassembly::Assignment<'a>,
    ) -> Result<Assignment, DisassemblyError> {
        let left = self.write_scope(input.left)?;
        let right = self.new_expr(input.right)?;
        Ok(Assignment { left, right })
    }
    fn new_assertation(
        &mut self,
        input: block::disassembly::Assertation<'a>,
    ) -> Result<Assertation, DisassemblyError> {
        match input {
            block::disassembly::Assertation::GlobalSet(globalset) => self
                .new_globalset(globalset)
                .map(|global| Assertation::GlobalSet(global)),
            block::disassembly::Assertation::Assignment(assignment) => self
                .new_assignment(assignment)
                .map(|ass| Assertation::Assignment(ass)),
        }
    }
    fn build(
        &mut self,
        mut input: block::disassembly::Disassembly<'a>,
    ) -> Result<(), DisassemblyError> {
        input
            .assertations
            .drain(..)
            .map(|input| {
                self.new_assertation(input)
                    .map(|ass| self.output.assertations.push(ass))
            })
            .collect::<Result<_, _>>()
    }
}

impl<'a, 'b> disassembly::ExprBuilder<'a> for Builder<'a, 'b> {
    fn read_scope(
        &mut self,
        name: &'b str,
    ) -> Result<Rc<dyn ReadScope>, DisassemblyError> {
        use super::GlobalScope::*;
        let src = self.sleigh.input_src(name);
        self.output
            .vars
            .get(name)
            .map(|local| Ok(local.as_read()))
            .unwrap_or_else(|| {
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(DisassemblyError::MissingRef(src.clone()))?
                {
                    Assembly(x) => {
                        //if is a field, check the produce_assembly in pattern
                        if x.field().is_some() {
                            //check the pattern will produce this field
                            if !self.pattern.include_produced_assembly(x) {
                                return Err(DisassemblyError::InvalidRef(src));
                            }
                        }
                        Ok(x.as_read())
                    }
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
