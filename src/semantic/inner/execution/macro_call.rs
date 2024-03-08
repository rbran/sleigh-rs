use crate::semantic::inner::pcode_macro::PcodeMacroCallId;
use crate::semantic::inner::Sleigh;
use crate::semantic::PcodeMacroId;
use crate::semantic::{
    execution::MacroCall as FinalMacroCall, inner::SolverStatus,
};
use crate::ExecutionError;

use super::{Expr, Variable};

#[derive(Clone, Debug)]
pub struct MacroCall {
    instance: PcodeMacroCallId,
    pub params: Vec<Expr>,
}

impl MacroCall {
    pub fn new(params: Vec<Expr>, macro_id: PcodeMacroId) -> Self {
        Self {
            params,
            instance: PcodeMacroCallId {
                macro_id,
                instance_id: None,
            },
        }
    }
    pub fn solve<T>(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut T,
    ) -> Result<(), Box<ExecutionError>>
    where
        T: SolverStatus + Default,
    {
        // if already specialized, then finished
        if self.instance.instance_id.is_some() {
            return Ok(());
        }

        //otherwise try to specialize the macro call
        //update the param expected size with the macro expected param size
        //and solved it
        let pcode_macro = sleigh.pcode_macro(self.instance.macro_id);
        let params_iter = pcode_macro
            .params
            .iter()
            .map(|param| pcode_macro.execution.variable(param.variable_id));
        for (param, macro_param) in self.params.iter_mut().zip(params_iter) {
            let src = param.src().clone();
            if param
                .size_mut(sleigh, variables)
                .update_action(|size| size.intersection(macro_param.size.get()))
                .ok_or(ExecutionError::VarSize(src))?
            {
                solved.we_did_a_thing();
            }
            param.solve(sleigh, variables, solved)?;
        }

        //try to specialize the macro call
        let params_size = self
            .params
            .iter()
            .map(|x| x.size(sleigh, variables).final_value())
            .collect::<Option<Vec<_>>>();
        if let Some(params_size) = params_size {
            let pcode_macro = sleigh.pcode_macro(self.instance.macro_id);
            self.instance.instance_id = Some(
                pcode_macro
                    .specialize(&params_size)
                    .map_err(|_| -> Box<ExecutionError> { todo!() })?,
            );
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(self) -> FinalMacroCall {
        let params = self.params.into_iter().map(|x| x.convert()).collect();
        FinalMacroCall {
            params,
            function: self.instance.convert(),
        }
    }
}
