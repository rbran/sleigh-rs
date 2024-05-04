use std::cell::RefCell;

use crate::semantic::execution::{BlockId, Build};
use crate::semantic::pcode_macro::{
    Parameter, PcodeMacro as FinalPcodeMacro,
    PcodeMacroInstance as FinalPcodeMacroInstance, PcodeMacroInstanceId,
};
use crate::semantic::{GlobalScope, PcodeMacroId};
use crate::{
    syntax, ExecutionError, NumberNonZeroUnsigned, PcodeMacroError,
    SleighError, Span,
};

use super::execution::{Execution, ExecutionBuilder, ReadScope, WriteScope};
use super::pattern::Pattern;
use super::{Sleigh, SolverStatus};

#[derive(Debug, Clone)]
pub struct PcodeMacroTmpInst {
    pub params: Vec<Parameter>,
    pub execution: Execution,
}

#[derive(Debug, Clone)]
pub struct PcodeMacroInstance {
    pub signature: Vec<NumberNonZeroUnsigned>,
    pub params: Vec<Parameter>,
    pub execution: Execution,
}

impl PcodeMacroInstance {
    pub fn new(params: Vec<Parameter>, execution: Execution) -> Self {
        let signature = params
            .iter()
            .map(|param| {
                let variable = execution.variable(param.variable_id);
                variable.size.get().final_value().unwrap()
            })
            .collect();
        Self {
            signature,
            params,
            execution,
        }
    }
    pub fn solve<T>(
        &mut self,
        _sleigh: &Sleigh,
        _solved: &mut T,
    ) -> Result<(), Box<ExecutionError>>
    where
        T: SolverStatus + Default,
    {
        // TODO improve that, create a step to solve macros, only then try solve
        // tables that call those macros using tmp instances.
        // Don't need to solve the macro, it's just used to create tmp instances
        // solve that instead
        //self.execution.solve(sleigh, solved)
        Ok(())
    }
    pub fn convert(self) -> FinalPcodeMacroInstance {
        FinalPcodeMacroInstance {
            parameters: self.params.into(),
            execution: self.execution.convert(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PcodeMacro {
    pub name: String,
    pub params: Vec<Parameter>,
    pub execution: Execution,
    pub(crate) instances: RefCell<Vec<PcodeMacroInstance>>,
    pub location: Span,
    //TODO: export macro is a thing?
}

impl PcodeMacro {
    pub fn new(
        name: String,
        src: Span,
        params: Vec<Parameter>,
        execution: Execution,
    ) -> Self {
        Self {
            name,
            instances: RefCell::new(vec![]),
            params,
            execution,
            location: src,
        }
    }

    pub fn tmp_inst(&self) -> PcodeMacroTmpInst {
        // create a new instance
        let execution = self.execution.clone();
        let params = self.params.clone();
        PcodeMacroTmpInst { execution, params }
    }

    pub fn specialize(
        &self,
        tmp_inst: PcodeMacroTmpInst,
        param_sizes: &[NumberNonZeroUnsigned],
    ) -> Result<PcodeMacroInstanceId, Box<PcodeMacroError>> {
        //check if this instance already exists
        if let Some(id) = self
            .instances
            .borrow()
            .iter()
            .position(|instance| instance.signature == param_sizes)
        {
            return Ok(PcodeMacroInstanceId(id));
        }
        // create a new instance
        let mut execution = tmp_inst.execution;
        let params = self.params.clone();
        //update the size of variables associated with params
        for (param, size) in params.iter().zip(param_sizes) {
            let var = execution.variable_mut(param.variable_id);
            let new_var =
                var.size.get().set_final_value(*size).ok_or_else(|| {
                    Box::new(PcodeMacroError::InvalidSpecialization(
                        param.location.clone(),
                    ))
                })?;
            var.size.set(new_var);
        }
        let instance = PcodeMacroInstance::new(params, execution);
        self.instances.borrow_mut().push(instance);
        Ok(PcodeMacroInstanceId(self.instances.borrow().len() - 1))
    }
    pub fn solve<T: SolverStatus>(
        &mut self,
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<(), Box<SleighError>>
    where
        T: SolverStatus + Default,
    {
        for instance in self.instances.borrow_mut().iter_mut() {
            instance.solve(sleigh, solved).map_err(|e| {
                Box::new(SleighError::new_pcode_macro(
                    self.location.clone(),
                    *e,
                ))
            })?;
        }
        Ok(())
    }

    pub fn convert(self) -> FinalPcodeMacro {
        let instances = self.instances.take();
        let instances = instances
            .into_iter()
            .map(|instance| instance.convert())
            .collect();
        FinalPcodeMacro {
            location: self.location,
            instances,
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
    ) -> Result<WriteScope, Box<ExecutionError>> {
        //check local scope
        if let Some(var) = self.execution().variable_by_name(name) {
            return Ok(WriteScope::Local(var));
        }
        //
        //at last check the global scope
        use super::GlobalScope::*;
        match self
            .sleigh
            .get_global(name)
            .ok_or_else(|| Box::new(ExecutionError::MissingRef(src.clone())))?
        {
            Varnode(x) => Ok(WriteScope::Varnode(x)),
            Bitrange(x) => Ok(WriteScope::Bitrange(x)),
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
                Ok(Parameter {
                    variable_id: execution.create_variable(
                        name,
                        src.clone(),
                        false,
                    )?,
                    location: src.clone(),
                })
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
