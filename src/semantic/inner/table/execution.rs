use std::rc::Rc;

use crate::semantic::inner::execution::{
    Block, Execution, ExecutionBuilder, ExprValue, WriteValue,
};
use crate::semantic::inner::{FieldSize, Pattern, Sleigh};
use crate::semantic::meaning::Meaning;
use crate::semantic::table::ExecutionError;
use crate::semantic::GlobalReference;
use crate::Span;

#[derive(Clone, Debug)]
pub struct Builder<'b, 'c> {
    execution: Execution,
    current_block: Rc<Block>,

    sleigh: &'b Sleigh,
    pattern: &'c Pattern,
}

impl<'b, 'c> Builder<'b, 'c> {
    pub fn new(sleigh: &'b Sleigh, pattern: &'c Pattern, src: &Span) -> Self {
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

impl<'b, 'c> From<Builder<'b, 'c>> for Execution {
    fn from(input: Builder<'b, 'c>) -> Self {
        input.execution
    }
}

impl<'b, 'c> ExecutionBuilder for Builder<'b, 'c> {
    fn sleigh(&self) -> &Sleigh {
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
        name: &str,
        src: &Span,
    ) -> Result<ExprValue, ExecutionError> {
        self.variable(name)
            .map(|var| ExprValue::ExeVar(src.clone(), Rc::clone(var)))
            .or_else(|| {
                //check the disassembly scope
                self.pattern.base.disassembly_vars.get(name).map(|var| {
                    let size = FieldSize::new_unsized();
                    size.set_min(1.try_into().unwrap());
                    ExprValue::DisVar(
                        src.clone(),
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
                    .ok_or(ExecutionError::MissingRef(src.clone()))?
                {
                    //TODO filter out epsilon
                    //TODO make sure all fields used on execution can be
                    //produced by the pattern
                    TokenField(x) => {
                        Ok(ExprValue::new_token_field(src.clone(), x))
                    }
                    InstStart(x) => {
                        //TODO error
                        let len = self.sleigh().exec_addr_size().unwrap();
                        Ok(ExprValue::new_inst_start(src.clone(), *len, x))
                    }
                    InstNext(x) => {
                        //TODO error
                        let len = self.sleigh().exec_addr_size().unwrap();
                        Ok(ExprValue::new_inst_next(src.clone(), *len, x))
                    }
                    Varnode(x) => Ok(ExprValue::new_varnode(src.clone(), x)),
                    Bitrange(x) => Ok(ExprValue::new_bitrange(src.clone(), x)),
                    Context(x) => Ok(ExprValue::new_context(src.clone(), x)),
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
                            src.clone(),
                        )))
                    }
                    _ => Err(ExecutionError::InvalidRef(src.clone())),
                }
            })
    }

    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteValue, ExecutionError> {
        self.variable(name)
            .map(|var| Ok(WriteValue::ExeVar(src.clone(), Rc::clone(var))))
            .unwrap_or_else(|| {
                use super::GlobalScope;
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(ExecutionError::MissingRef(src.clone()))?
                {
                    GlobalScope::Varnode(varnode) => Ok(WriteValue::Varnode(
                        GlobalReference::from_element(varnode, src.clone()),
                    )),
                    GlobalScope::TokenField(token_field) => {
                        let meaning = token_field.meaning();
                        //filter field with meaning to variable
                        if !matches!(
                            meaning.as_ref(),
                            Some(Meaning::Variable(_))
                        ) {
                            return Err(ExecutionError::InvalidRef(
                                src.clone(),
                            ));
                        }
                        Ok(WriteValue::TokenField(
                            GlobalReference::from_element(
                                token_field,
                                src.clone(),
                            ),
                        ))
                    }
                    GlobalScope::Table(table) => Ok(WriteValue::Table(
                        GlobalReference::from_element(table, src.clone()),
                    )),
                    _ => Err(ExecutionError::InvalidRef(src.clone())),
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
