use std::rc::Rc;

use crate::semantic::inner::disassembly::Disassembly;
use crate::semantic::inner::execution::{
    Block, Execution, ExecutionBuilder, ExprValue, WriteValue,
};
use crate::semantic::inner::{FieldSize, Sleigh};
use crate::semantic::table::ExecutionError;
use crate::semantic::varnode::VarnodeType;
use crate::InputSource;

#[derive(Clone, Debug)]
pub struct Builder<'a, 'b, 'c> {
    execution: Execution,
    current_block: Rc<Block>,

    sleigh: &'b Sleigh<'a>,
    disassembly: &'c Disassembly,
}

impl<'a, 'b, 'c> Builder<'a, 'b, 'c> {
    pub fn new(
        sleigh: &'b Sleigh<'a>,
        disassembly: &'c Disassembly,
        src: &InputSource,
    ) -> Self {
        let execution = Execution::new_empty(src);
        let current_block = Rc::clone(&execution.entry_block);
        Self {
            execution,
            current_block,
            sleigh,
            disassembly,
        }
    }
}

impl<'a, 'b, 'c> From<Builder<'a, 'b, 'c>> for Execution {
    fn from(input: Builder<'a, 'b, 'c>) -> Self {
        input.execution
    }
}

impl<'a, 'b, 'c> ExecutionBuilder<'a> for Builder<'a, 'b, 'c> {
    fn sleigh(&self) -> &Sleigh<'a> {
        self.sleigh
    }
    fn execution(&self) -> &Execution {
        &self.execution
    }
    fn execution_mut(&mut self) -> &mut Execution {
        &mut self.execution
    }
    fn read_scope(
        &mut self,
        name: &'a str,
    ) -> Result<ExprValue, ExecutionError> {
        let src = || self.sleigh.input_src(name);
        self.variable(name)
            .map(|var| ExprValue::ExeVar(src(), Rc::clone(var)))
            .or_else(|| {
                //check the disassembly scope
                self.disassembly.variable(name).map(|var| {
                    let size = FieldSize::new_unsized();
                    size.set_min(1.try_into().unwrap());
                    ExprValue::DisVar(
                        src(),
                        size, //TODO var min size here, like in assembly
                        Rc::clone(var),
                    )
                })
            })
            .map(Result::Ok)
            .unwrap_or_else(|| {
                //lastly check the global scope
                use super::GlobalScope::*;
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(ExecutionError::MissingRef(src()))?
                {
                    //TODO filter out epsilon
                    Assembly(x) => {
                        Ok(ExprValue::new_assembly(src(), Rc::clone(x)))
                    }
                    Varnode(x) => {
                        Ok(ExprValue::new_varnode(src(), Rc::clone(x)))
                    }
                    //only if this table have at least one constructor that
                    //export any kind of value
                    Table(table)
                        if table
                            .export()
                            .borrow()
                            .as_ref()
                            .map(|x| !x.export_nothing())
                            .unwrap_or(false) =>
                    {
                        Ok(ExprValue::Table(src(), Rc::clone(table)))
                    }
                    _ => Err(ExecutionError::InvalidRef(src())),
                }
            })
    }

    fn write_scope(
        &mut self,
        name: &'a str,
    ) -> Result<WriteValue, ExecutionError> {
        let src = || self.sleigh.input_src(name);
        self.variable(name)
            .map(|var| Ok(WriteValue::ExeVar(src(), Rc::clone(var))))
            .unwrap_or_else(|| {
                use super::GlobalScope;
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(ExecutionError::MissingRef(src()))?
                {
                    GlobalScope::Varnode(varnode)
                        if matches!(
                            varnode.varnode_type,
                            VarnodeType::Memory(_) | VarnodeType::BitRange(_)
                        ) =>
                    {
                        Ok(WriteValue::Varnode(src(), Rc::clone(varnode)))
                    }
                    //TODO: filter field with meaning to variable
                    GlobalScope::Assembly(assembly) => {
                        Ok(WriteValue::Assembly(src(), Rc::clone(assembly)))
                    }
                    GlobalScope::Table(table) => {
                        Ok(WriteValue::Table(src(), Rc::clone(table)))
                    }
                    _ => Err(ExecutionError::InvalidRef(src())),
                }
            })
    }

    fn current_block(&self) -> Rc<Block> {
        Rc::clone(&self.current_block)
    }
    fn current_block_mut(&mut self) -> &mut Rc<Block> {
        &mut self.current_block
    }
}
