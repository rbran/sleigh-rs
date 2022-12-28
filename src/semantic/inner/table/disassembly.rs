use std::rc::Rc;

use crate::semantic::inner::disassembly::{
    AddrScope, Assertation, Disassembly, DisassemblyBuilder, ReadScope,
    Variable, WriteScope,
};
use crate::semantic::inner::varnode::Context;
use crate::semantic::inner::{disassembly, Pattern, Sleigh};
use crate::semantic::{DisassemblyError, GlobalReference};
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

impl<'a, 'b> DisassemblyBuilder<'a> for Builder<'a, 'b> {
    fn new_assignment(
        &mut self,
        input: block::disassembly::Assignment<'a>,
    ) -> Result<disassembly::Assignment, DisassemblyError> {
        let left = self.write_scope(input.left)?;
        let right = disassembly::ExprBuilder::new_expr(self, input.right)?;
        Ok(disassembly::Assignment { left, right })
    }
    fn insert_assertation(&mut self, ass: Assertation) {
        self.output.assertations.push(ass)
    }
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
                    //TODO make sure the pattern will produce this table
                    Table(x) => Ok(AddrScope::Table(
                        GlobalReference::from_element(x, src),
                    )),
                    InstStart(x) => Ok(AddrScope::InstStart(
                        GlobalReference::from_element(x, src),
                    )),
                    InstNext(x) => Ok(AddrScope::InstNext(
                        GlobalReference::from_element(x, src),
                    )),
                    //TokenField(x) => Ok(AddrScope::TokenField(
                    //    GlobalReference::from_element(x, src),
                    //)),
                    Varnode(x) => Ok(AddrScope::Varnode(
                        GlobalReference::from_element(x, src),
                    )),
                    _ => Err(DisassemblyError::InvalidRef(src)),
                }
            })
    }
    fn write_scope(
        &mut self,
        name: &'a str,
    ) -> Result<WriteScope, DisassemblyError> {
        //if variable exists, return it
        let var = self
            .output
            .vars
            .get(name)
            .map(|var| WriteScope::Local(Rc::clone(var)))
            .or_else(|| {
                //check the global context, if context return it
                //NOTE if other thing with the same name, but is not context,
                //create the variable to shadow global context
                let src = self.sleigh.input_src(name);
                let context = self.sleigh.get_global(name)?.unwrap_context()?;
                Some(WriteScope::Context(GlobalReference::from_element(
                    context, src,
                )))
            })
            .unwrap_or_else(|| {
                //otherwise create the variable
                let src = self.sleigh.input_src(name);
                let var = Variable::new(name, src);
                self.output
                    .vars
                    .insert(Rc::clone(var.name()), Rc::clone(&var));
                WriteScope::Local(var)
            });
        Ok(var)
    }
    fn context(
        &mut self,
        name: &'a str,
    ) -> Result<GlobalReference<Context>, DisassemblyError> {
        let src = self.sleigh.input_src(name);
        let context = self
            .sleigh
            .get_global(name)
            .ok_or(DisassemblyError::MissingRef(src.clone()))?
            .context_or(DisassemblyError::InvalidRef(src.clone()))?;
        Ok(GlobalReference::from_element(context, src))
    }
}

impl<'a, 'b> disassembly::ExprBuilder<'a> for Builder<'a, 'b> {
    fn read_scope(
        &mut self,
        name: &'b str,
    ) -> Result<ReadScope, DisassemblyError> {
        use super::GlobalScope::*;
        let src = self.sleigh.input_src(name);
        self.output
            .vars
            .get(name)
            .map(|local| Ok(ReadScope::Local(Rc::clone(local))))
            .unwrap_or_else(|| {
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(DisassemblyError::MissingRef(src.clone()))?
                {
                    InstNext(x) => Ok(ReadScope::InstNext(
                        GlobalReference::from_element(x, src),
                    )),
                    InstStart(x) => Ok(ReadScope::InstStart(
                        GlobalReference::from_element(x, src),
                    )),
                    TokenField(x) => {
                        //check the pattern will produce this field
                        match self.pattern.add_implicit_token_field(
                            &GlobalReference::from_element(x, src.clone()),
                        ) {
                            Ok(true) => (),
                            Err(_) | Ok(false) => {
                                return Err(DisassemblyError::InvalidRef(src))
                            }
                        }
                        Ok(ReadScope::TokenField(
                            GlobalReference::from_element(x, src),
                        ))
                    }
                    Context(x) => Ok(ReadScope::Context(
                        GlobalReference::from_element(x, src),
                    )),
                    _ => Err(DisassemblyError::InvalidRef(src.clone())),
                }
            })
    }
}
