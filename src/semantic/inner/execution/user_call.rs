use crate::semantic::execution::UserCall as FinalUserCall;
use crate::semantic::UserFunctionId;
use crate::ExecutionError;
use crate::Span;

use super::{Execution, Expr, FieldSize, Sleigh, SolverStatus};

#[derive(Clone, Debug)]
pub struct UserCall {
    pub location: Span,
    pub output_size: FieldSize,
    pub function: UserFunctionId,
    pub params: Vec<Expr>,
}

impl UserCall {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut params: Vec<Expr>,
        function: UserFunctionId,
        location: Span,
    ) -> Self {
        //TODO how to handle user functions with variable number of parameter???
        //function.set_param_num(params.len()).unwrap(/*TODO*/);

        //params size is not very relevant
        params.iter_mut().for_each(|param| {
            //TODO improve the size speculation
            param
                .size_mut(sleigh, execution)
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        Self {
            params,
            output_size: FieldSize::new_unsized(),
            function,
            location,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        self.params
            .iter_mut()
            .try_for_each(|x| x.solve(sleigh, execution, solved))
    }
    pub fn convert(self) -> FinalUserCall {
        let params = self.params.into_iter().map(|x| x.convert()).collect();
        FinalUserCall {
            location: self.location,
            function: self.function,
            params,
        }
    }
}
