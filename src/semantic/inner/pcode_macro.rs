use std::cell::RefCell;

use crate::semantic::execution::{
    BlockId, Build, ExprBitrange, ExprContext, ExprExeVar, ExprInstNext,
    ExprInstStart, ExprTokenField, ExprVarnode, WriteValue,
};
use crate::semantic::pcode_macro::{
    Parameter, PcodeMacro as FinalPcodeMacro,
    PcodeMacroCallId as FinalPcodeMacroCallId,
    PcodeMacroInstance as FinalPcodeMacroInstance, PcodeMacroInstanceId,
};
use crate::semantic::{GlobalScope, PcodeMacroId};
use crate::{
    disassembly, syntax, ExecutionError, NumberNonZeroUnsigned,
    PcodeMacroError, SleighError, Span,
};

use super::execution::{Execution, ExecutionBuilder, ReadValue};
use super::{Sleigh, SolverStatus};

#[derive(Debug, Clone, Copy)]
pub struct PcodeMacroCallId {
    pub macro_id: PcodeMacroId,
    pub instance_id: Option<PcodeMacroInstanceId>,
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
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        self.execution.solve(sleigh, solved)
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
    instances: RefCell<Vec<PcodeMacroInstance>>,
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

    pub fn specialize(
        &self,
        param_sizes: &[NumberNonZeroUnsigned],
    ) -> Result<PcodeMacroInstanceId, PcodeMacroError> {
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
        let mut execution = self.execution.clone();
        let params = self.params.clone();
        //update the size of variables associated with params
        for (param, size) in params.iter().zip(param_sizes) {
            let var = execution.variable_mut(param.variable_id);
            let new_var =
                var.size.get().set_final_value(*size).ok_or_else(|| {
                    PcodeMacroError::InvalidSpecialization(
                        param.location.clone(),
                    )
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
    ) -> Result<(), SleighError>
    where
        T: SolverStatus + Default,
    {
        for instance in self.instances.borrow_mut().iter_mut() {
            instance.solve(sleigh, solved).map_err(|e| {
                SleighError::new_pcode_macro(self.location.clone(), e)
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
    ) -> Result<(), PcodeMacroError> {
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
    fn disassembly_var(
        &mut self,
        _id: disassembly::VariableId,
    ) -> &mut disassembly::Variable {
        unreachable!()
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

        // check global scope
        use super::GlobalScope::*;
        match self
            .sleigh
            .get_global(name)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
        {
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
            Context(x) => Ok(ReadValue::Context(ExprContext {
                location: src.clone(),
                id: x,
            })),
            Bitrange(x) => Ok(ReadValue::Bitrange(ExprBitrange {
                location: src.clone(),
                id: x,
            })),
            _ => Err(ExecutionError::InvalidRef(src.clone())),
        }
    }

    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteValue, ExecutionError> {
        //check local scope
        if let Some(var) = self.execution().variable_by_name(name) {
            return Ok(WriteValue::Local(ExprExeVar {
                location: src.clone(),
                id: var,
            }));
        }
        //
        //at last check the global scope
        use super::GlobalScope::*;
        match self
            .sleigh
            .get_global(name)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
        {
            Varnode(x) => Ok(WriteValue::Varnode(ExprVarnode {
                location: src.clone(),
                id: x,
            })),
            Bitrange(x) => Ok(WriteValue::Bitrange(ExprBitrange {
                location: src.clone(),
                id: x,
            })),
            _ => Err(ExecutionError::InvalidRef(src.clone())),
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
    ) -> Result<Build, ExecutionError> {
        Err(ExecutionError::MacroBuildInvalid)
    }

    fn inner_set_curent_block(&mut self, block: BlockId) {
        self.current_block = block
    }
}

impl Sleigh {
    pub fn create_pcode_macro(
        &mut self,
        pcode: syntax::block::pcode_macro::PcodeMacro,
    ) -> Result<(), SleighError> {
        let mut execution = Execution::new_empty(pcode.src.clone());
        //create variables for each param
        let params = pcode
            .params
            .into_iter()
            .map(|(name, src)| -> Result<_, ExecutionError> {
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
            .map_err(|e| SleighError::new_pcode_macro(pcode.src.clone(), e))?;
        Builder::parse(self, &mut execution, pcode.body)
            .map_err(|e| SleighError::new_pcode_macro(pcode.src.clone(), e))?;
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
            .map(|_| Err(SleighError::NameDuplicated))
            .unwrap_or(Ok(()))
    }
}

impl PcodeMacroCallId {
    pub fn convert(self) -> FinalPcodeMacroCallId {
        FinalPcodeMacroCallId {
            macro_id: self.macro_id,
            instance_id: self.instance_id.unwrap(),
        }
    }
}
