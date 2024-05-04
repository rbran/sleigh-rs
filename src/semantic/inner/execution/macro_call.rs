use crate::pcode_macro::{PcodeMacroCallId, PcodeMacroInstanceId};
use crate::semantic::inner::pcode_macro::{PcodeMacro, PcodeMacroTmpInst};
use crate::semantic::inner::Sleigh;
use crate::semantic::PcodeMacroId;
use crate::semantic::{
    execution::MacroCall as FinalMacroCall, inner::SolverStatus,
};
use crate::{ExecutionError, Span, VarSizeError};

use super::{len, Expr, Variable};

#[derive(Clone, Debug)]
pub struct MacroCall {
    location: Span,
    macro_id: PcodeMacroId,
    solved: MacroCallSolved,
    pub params: Vec<Expr>,
}

#[derive(Clone, Debug)]
enum MacroCallSolved {
    Unsolved(PcodeMacroTmpInst),
    Solved(PcodeMacroInstanceId),
}

impl MacroCall {
    pub fn new(
        params: Vec<Expr>,
        macro_id: PcodeMacroId,
        pcode_macro: &PcodeMacro,
        location: Span,
    ) -> Self {
        Self {
            location,
            params,
            macro_id,
            solved: MacroCallSolved::Unsolved(pcode_macro.tmp_inst()),
        }
    }
    pub fn solve<T: SolverStatus>(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut T,
    ) -> Result<(), Box<ExecutionError>> {
        // if already specialized, then finished
        let tmp_instance = match &mut self.solved {
            MacroCallSolved::Unsolved(tmp_instance) => tmp_instance,
            MacroCallSolved::Solved(id) => {
                let pcode_macro = sleigh.pcode_macro(self.macro_id);
                let mut instances = pcode_macro.instances.borrow_mut();
                let instance = &mut instances[id.0];
                return instance.execution.solve(sleigh, solved);
            }
        };

        //otherwise try to specialize the macro call
        //update the param expected size with the macro expected param size
        //and solved it
        let params_iter = tmp_instance
            .params
            .iter()
            .map(|param| tmp_instance.execution.variable(param.variable_id));
        for (param, macro_param) in self.params.iter_mut().zip(params_iter) {
            let mut macro_param_size = &macro_param.size;
            let modified = len::a_equivalent_b(
                &mut *param.size_mut(sleigh, variables),
                &mut macro_param_size,
            );
            if modified.ok_or_else(|| VarSizeError::MacroParamWrongSize {
                param: macro_param.size.get(),
                input: param.size(sleigh, variables),
                location: param.src().clone(),
            })? {
                solved.we_did_a_thing();
            }
            param.solve(sleigh, variables, solved)?;
        }
        tmp_instance.execution.solve(sleigh, solved)?;

        //try to specialize the macro call
        let params_size = self
            .params
            .iter()
            .map(|x| x.size(sleigh, variables).final_value())
            .collect::<Option<Vec<_>>>();
        if let Some(params_size) = params_size {
            let pcode_macro = sleigh.pcode_macro(self.macro_id);
            // TODO improve that
            let tmp_instance = tmp_instance.clone();
            self.solved = MacroCallSolved::Solved(
                pcode_macro
                    .specialize(tmp_instance, &params_size)
                    .map_err(|_| -> Box<ExecutionError> { todo!() })?,
            );
            solved.i_did_a_thing();
        } else {
            solved.iam_not_finished(&self.location, file!(), line!());
        }
        Ok(())
    }
    pub fn convert(self) -> FinalMacroCall {
        let params = self.params.into_iter().map(|x| x.convert()).collect();
        let MacroCallSolved::Solved(instance_id) = self.solved else {
            panic!();
        };
        FinalMacroCall {
            params,
            function: PcodeMacroCallId {
                macro_id: self.macro_id,
                instance_id,
            },
        }
    }
}
