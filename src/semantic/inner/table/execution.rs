use crate::semantic::execution::{
    BlockId, ExprBitrange, ExprContext, ExprExeVar, ExprInstNext,
    ExprInstStart, ExprTable, ExprTokenField, ExprVarnode, WriteValue,
};
use crate::semantic::inner::execution::{
    Execution, ExecutionBuilder, ExprDisVar, FieldSize, ReadValue,
};
use crate::semantic::inner::pattern::Pattern;
use crate::semantic::inner::Sleigh;
use crate::semantic::token::TokenFieldAttach;
use crate::{ExecutionError, Span};

#[derive(Clone, Debug)]
pub struct Builder<'b, 'c> {
    execution: Execution,
    current_block: BlockId,

    sleigh: &'b Sleigh,
    pattern: &'c Pattern,
}

impl<'b, 'c> Builder<'b, 'c> {
    pub fn new(sleigh: &'b Sleigh, pattern: &'c Pattern, src: Span) -> Self {
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
    ) -> Result<ReadValue, ExecutionError> {
        //check local scope
        if let Some(var) = self.execution().variable_by_name(name) {
            return Ok(ReadValue::ExeVar(ExprExeVar {
                location: src.clone(),
                id: var,
            }));
        }

        //check the disassembly scope
        if let Some(var) = self.pattern.disassembly_variable_names.get(name) {
            return Ok(ReadValue::DisVar(ExprDisVar {
                location: src.clone(),
                id: *var,
                size: FieldSize::new_unsized(),
            }));
        }

        //lastly check the global scope
        use super::GlobalScope::*;
        match self
            .sleigh
            .get_global(name)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
        {
            //TODO make sure all fields used on execution can be
            //produced by the pattern
            TokenField(x) => Ok(ReadValue::TokenField(ExprTokenField {
                location: src.clone(),
                id: x,
            })),
            InstStart(x) => Ok(ReadValue::InstStart(ExprInstStart {
                location: src.clone(),
                data: x,
            })),
            InstNext(x) => Ok(ReadValue::InstNext(ExprInstNext {
                location: src.clone(),
                data: x,
            })),
            Varnode(x) => Ok(ReadValue::Varnode(ExprVarnode {
                location: src.clone(),
                id: x,
            })),
            Bitrange(x) => Ok(ReadValue::Bitrange(ExprBitrange {
                location: src.clone(),
                id: x,
            })),
            Context(x) => Ok(ReadValue::Context(ExprContext {
                location: src.clone(),
                id: x,
            })),
            //only if table export some kind of value
            Table(table)
                if self
                    .sleigh()
                    .table(table)
                    .export
                    .borrow()
                    .as_ref()
                    .map(|x| !x.export_nothing())
                    .unwrap_or(false) =>
            {
                Ok(ReadValue::Table(ExprTable {
                    location: src.clone(),
                    id: table,
                }))
            }
            _ => Err(ExecutionError::InvalidRef(src.clone())),
        }
    }

    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteValue, ExecutionError> {
        self.execution()
            .variable_by_name(name)
            .map(|var| {
                Ok(WriteValue::Local(ExprExeVar {
                    location: src.clone(),
                    id: var,
                }))
            })
            .unwrap_or_else(|| {
                use super::GlobalScope;
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(ExecutionError::MissingRef(src.clone()))?
                {
                    GlobalScope::Varnode(varnode) => {
                        Ok(WriteValue::Varnode(ExprVarnode {
                            location: src.clone(),
                            id: varnode,
                        }))
                    }
                    GlobalScope::TokenField(id) => {
                        let meaning = &self.sleigh().token_field(id).attach;
                        //filter field with meaning to variable
                        if !matches!(
                            meaning.as_ref(),
                            Some(TokenFieldAttach::Varnode(_))
                        ) {
                            return Err(ExecutionError::InvalidRef(
                                src.clone(),
                            ));
                        }
                        Ok(WriteValue::TokenField(ExprTokenField {
                            location: src.clone(),
                            id,
                        }))
                    }
                    GlobalScope::Table(table) => {
                        Ok(WriteValue::TableExport(ExprTable {
                            location: src.clone(),
                            id: table,
                        }))
                    }
                    _ => Err(ExecutionError::InvalidRef(src.clone())),
                }
            })
    }

    fn current_block(&self) -> BlockId {
        self.current_block
    }
    fn inner_set_curent_block(&mut self, block: BlockId) {
        self.current_block = block
    }
}
