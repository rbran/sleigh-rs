use crate::execution::DynamicValueType;
use crate::semantic::execution::BlockId;
use crate::semantic::inner::execution::{
    Execution, ExecutionBuilder, ReadScope, WriteValue,
};
use crate::semantic::inner::pattern::Pattern;
use crate::semantic::inner::Sleigh;
use crate::semantic::token::TokenFieldAttach;
use crate::varnode::ContextAttach;
use crate::{ExecutionError, Span};

#[derive(Debug)]
pub struct Builder<'a> {
    execution: Execution,
    current_block: BlockId,

    sleigh: &'a Sleigh,
    pattern: &'a mut Pattern,
}

impl<'a> Builder<'a> {
    pub fn new(
        sleigh: &'a Sleigh,
        pattern: &'a mut Pattern,
        src: Span,
    ) -> Self {
        let execution = Execution::new_empty(src);
        let current_block = execution.entry_block;
        Self {
            execution,
            current_block,
            sleigh,
            pattern,
        }
    }
}

impl<'a> From<Builder<'a>> for Execution {
    fn from(input: Builder<'a>) -> Self {
        input.execution
    }
}

impl ExecutionBuilder for Builder<'_> {
    fn sleigh(&self) -> &Sleigh {
        self.sleigh
    }
    fn execution(&self) -> &Execution {
        &self.execution
    }
    fn execution_mut(&mut self) -> &mut Execution {
        &mut self.execution
    }
    fn pattern(&self) -> &Pattern {
        self.pattern
    }
    fn read_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ReadScope, Box<ExecutionError>> {
        //check local scope
        if let Some(var) = self.execution().variable_by_name(name) {
            return Ok(ReadScope::ExeVar(var));
        }

        //check the disassembly scope
        if let Some(var) = self.pattern.disassembly_variable_names.get(name) {
            return Ok(ReadScope::DisVar(*var));
        }

        //lastly check the global scope
        use super::GlobalScope::*;
        match self
            .sleigh
            .get_global(name)
            .ok_or_else(|| Box::new(ExecutionError::MissingRef(src.clone())))?
        {
            //TODO make sure all fields used on execution can be
            //produced by the pattern
            TokenField(x) => Ok(ReadScope::TokenField(x)),
            InstStart(_) => Ok(ReadScope::InstStart),
            InstNext(_) => Ok(ReadScope::InstNext),
            Varnode(x) => Ok(ReadScope::Varnode(x)),
            Bitrange(x) => Ok(ReadScope::Bitrange(x)),
            Context(x) => Ok(ReadScope::Context(x)),
            //only if table export some kind of value
            Table(table_id)
                if self
                    .sleigh()
                    .table(table_id)
                    .export
                    .borrow()
                    .as_ref()
                    .map(|x| !x.export_nothing())
                    .unwrap_or(false) =>
            {
                Ok(ReadScope::Table(table_id))
            }
            _ => Err(Box::new(ExecutionError::InvalidRef(src.clone()))),
        }
    }

    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteValue, Box<ExecutionError>> {
        if let Some(var) = self.execution().variable_by_name(name) {
            return Ok(WriteValue::Local {
                id: var,
                creation: false,
            });
        }
        use super::GlobalScope;
        match self
            .sleigh
            .get_global(name)
            .ok_or_else(|| Box::new(ExecutionError::MissingRef(src.clone())))?
        {
            GlobalScope::Varnode(varnode) => Ok(WriteValue::Varnode(varnode)),
            GlobalScope::TokenField(token_field_id) => {
                //filter field with meaning to variable
                let meaning = self.sleigh().token_field(token_field_id).attach;
                let Some(TokenFieldAttach::Varnode(attach_id)) = meaning else {
                    return Err(Box::new(ExecutionError::InvalidRef(
                        src.clone(),
                    )));
                };
                Ok(WriteValue::DynVarnode {
                    value_id: DynamicValueType::TokenField(token_field_id),
                    attach_id,
                })
            }
            GlobalScope::Context(context_id) => {
                //filter field with meaning to variable
                let meaning = self.sleigh().context(context_id).attach;
                let Some(ContextAttach::Varnode(attach_id)) = meaning else {
                    return Err(Box::new(ExecutionError::InvalidRef(
                        src.clone(),
                    )));
                };
                Ok(WriteValue::DynVarnode {
                    value_id: DynamicValueType::Context(context_id),
                    attach_id,
                })
            }
            GlobalScope::Table(table_id) => {
                Ok(WriteValue::TableExport(table_id))
            }
            _ => Err(Box::new(ExecutionError::InvalidRef(src.clone()))),
        }
    }

    fn current_block(&self) -> BlockId {
        self.current_block
    }
    fn inner_set_curent_block(&mut self, block: BlockId) {
        self.current_block = block
    }
}
