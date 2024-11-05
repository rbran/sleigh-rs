use crate::execution::WriteValue;
use crate::semantic::execution::{BlockId, Build, VariableId};
use crate::{syntax, ExecutionError, PcodeMacroError, SleighError, Span};

use super::execution::{Execution, ExecutionBuilder, ReadScope};
use super::pattern::Pattern;
use super::{GlobalScope, PcodeMacroId, Sleigh};

#[derive(Clone, Debug)]
pub struct PcodeMacro {
    pub name: String,
    pub params: Vec<VariableId>,
    pub execution: Execution,
    pub location: Span,
    //TODO: export macro is a thing?
}

impl PcodeMacro {
    pub fn new(
        name: String,
        src: Span,
        params: Vec<VariableId>,
        execution: Execution,
    ) -> Self {
        Self {
            name,
            params,
            execution,
            location: src,
        }
    }
}

pub struct Builder<'a, 'b> {
    execution: &'b mut Execution,
    current_block: BlockId,

    sleigh: &'a Sleigh,
}

impl<'a, 'b> Builder<'a, 'b> {
    fn parse(
        sleigh: &'a Sleigh,
        execution: &'b mut Execution,
        body: syntax::block::execution::Execution,
    ) -> Result<(), Box<PcodeMacroError>> {
        let current_block = execution.entry_block;
        let mut builder = Self {
            execution,
            current_block,
            sleigh,
        };
        builder.extend(body)?;
        Ok(())
    }
}

impl ExecutionBuilder for Builder<'_, '_> {
    fn sleigh(&self) -> &Sleigh {
        self.sleigh
    }
    fn pattern(&self) -> &Pattern {
        unreachable!()
    }
    fn execution(&self) -> &Execution {
        self.execution
    }
    fn execution_mut(&mut self) -> &mut Execution {
        self.execution
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

        // check global scope
        use super::GlobalScope::*;
        match self
            .sleigh
            .get_global(name)
            .ok_or_else(|| Box::new(ExecutionError::MissingRef(src.clone())))?
        {
            TokenField(x) => Ok(ReadScope::TokenField(x)),
            InstStart(_) => Ok(ReadScope::InstStart),
            InstNext(_) => Ok(ReadScope::InstNext),
            Varnode(x) => Ok(ReadScope::Varnode(x)),
            Context(x) => Ok(ReadScope::Context(x)),
            Bitrange(x) => Ok(ReadScope::Bitrange(x)),
            _ => Err(Box::new(ExecutionError::InvalidRef(src.clone()))),
        }
    }

    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteValue, Box<ExecutionError>> {
        //check local scope
        if let Some(var) = self.execution().variable_by_name(name) {
            return Ok(WriteValue::Local {
                id: var,
                creation: false,
            });
        }
        //
        //at last check the global scope
        use super::GlobalScope::*;
        match self
            .sleigh
            .get_global(name)
            .ok_or_else(|| Box::new(ExecutionError::MissingRef(src.clone())))?
        {
            Varnode(x) => Ok(WriteValue::Varnode(x)),
            Bitrange(x) => Ok(WriteValue::Bitrange(x)),
            _ => Err(Box::new(ExecutionError::InvalidRef(src.clone()))),
        }
    }

    fn current_block(&self) -> BlockId {
        self.current_block
    }

    fn set_current_block(&mut self, block: BlockId) {
        self.current_block = block
    }

    //macro have no build statement, so if try to parse it, is always error
    fn new_build(
        &mut self,
        _input: syntax::block::execution::Build,
    ) -> Result<Build, Box<ExecutionError>> {
        Err(Box::new(ExecutionError::MacroBuildInvalid))
    }

    fn inner_set_curent_block(&mut self, block: BlockId) {
        self.current_block = block
    }
}

impl Sleigh {
    pub fn create_pcode_macro(
        &mut self,
        pcode: syntax::block::pcode_macro::PcodeMacro,
    ) -> Result<(), Box<SleighError>> {
        let mut execution = Execution::new_empty(pcode.src.clone());
        //create variables for each param
        let params = pcode
            .params
            .into_iter()
            .map(|(name, src)| -> Result<_, Box<ExecutionError>> {
                Ok(execution.create_variable(name, src.clone(), None, false)?)
            })
            .collect::<Result<_, _>>()
            .map_err(|e| {
                Box::new(SleighError::new_pcode_macro(pcode.src.clone(), *e))
            })?;
        Builder::parse(self, &mut execution, pcode.body).map_err(|e| {
            Box::new(SleighError::new_pcode_macro(pcode.src.clone(), *e))
        })?;
        let pcode_macro = PcodeMacro::new(
            pcode.name.clone(),
            pcode.src.clone(),
            params,
            execution,
        );
        self.pcode_macros.push(pcode_macro);
        let pcode_macro_id = PcodeMacroId(self.pcode_macros.len() - 1);
        self.global_scope
            .insert(pcode.name, GlobalScope::PcodeMacro(pcode_macro_id))
            .map(|_| Err(Box::new(SleighError::NameDuplicated)))
            .unwrap_or(Ok(()))
    }
}
