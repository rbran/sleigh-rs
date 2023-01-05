use std::rc::Rc;

use crate::semantic::inner::execution::{
    Block, Execution, ExecutionBuilder, ExprValue, WriteValue,
};
use crate::semantic::inner::{FieldSize, Pattern, Sleigh};
use crate::semantic::meaning::Meaning;
use crate::semantic::table::ExecutionError;
use crate::semantic::GlobalReference;
use crate::InputSource;

#[derive(Clone, Debug)]
pub struct Builder<'a, 'b, 'c> {
    execution: Execution,
    current_block: Rc<Block>,

    sleigh: &'b Sleigh<'a>,
    pattern: &'c Pattern,
}

impl<'a, 'b, 'c> Builder<'a, 'b, 'c> {
    pub fn new(
        sleigh: &'b Sleigh<'a>,
        pattern: &'c Pattern,
        src: &InputSource,
    ) -> Self {
        let execution = Execution::new_empty(src);
        let current_block = Rc::clone(&execution.entry_block);
        Self {
            execution,
            current_block,
            sleigh,
            pattern,
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
                self.pattern.disassembly_vars.get(name).map(|var| {
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
                    //TODO make sure all fields used on execution can be
                    //produced by the pattern
                    TokenField(x) => Ok(ExprValue::new_token_field(src(), x)),
                    InstStart(x) => {
                        //TODO error
                        let len = self.sleigh().exec_addr_size().unwrap();
                        Ok(ExprValue::new_inst_start(src(), *len, x))
                    }
                    InstNext(x) => {
                        //TODO error
                        let len = self.sleigh().exec_addr_size().unwrap();
                        Ok(ExprValue::new_inst_next(src(), *len, x))
                    }
                    Varnode(x) => Ok(ExprValue::new_varnode(src(), x)),
                    Bitrange(x) => Ok(ExprValue::new_bitrange(src(), x)),
                    Context(x) => Ok(ExprValue::new_context(src(), x)),
                    //only if table export some kind of value
                    Table(table)
                        if table
                            .export()
                            .borrow()
                            .as_ref()
                            .map(|x| !x.export_nothing())
                            .unwrap_or(false) =>
                    {
                        Ok(ExprValue::Table(GlobalReference::from_element(
                            table,
                            src(),
                        )))
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
                    GlobalScope::Varnode(varnode) => Ok(WriteValue::Varnode(
                        GlobalReference::from_element(varnode, src()),
                    )),
                    GlobalScope::TokenField(token_field) => {
                        let meaning = token_field.meaning();
                        //filter field with meaning to variable
                        if !matches!(
                            meaning.as_ref(),
                            Some(Meaning::Variable(_))
                        ) {
                            return Err(ExecutionError::InvalidRef(src()));
                        }
                        Ok(WriteValue::TokenField(
                            GlobalReference::from_element(token_field, src()),
                        ))
                    }
                    GlobalScope::Table(table) => Ok(WriteValue::Table(
                        GlobalReference::from_element(table, src()),
                    )),
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
