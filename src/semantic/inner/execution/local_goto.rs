use crate::semantic::execution::LocalGoto as FinalLocalGoto;
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::{semantic::execution::BlockId, ExecutionError};

use super::{Execution, Expr};

#[derive(Clone, Debug)]
pub struct LocalGoto {
    pub cond: Option<Expr>,
    pub dst: BlockId,
}

impl LocalGoto {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut cond: Option<Expr>,
        dst: BlockId,
    ) -> Result<Self, Box<ExecutionError>> {
        //condition can have any size, preferencially 1 bit for true/false
        cond.iter_mut().for_each(|cond| {
            cond.size_mut(sleigh, execution)
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        Ok(LocalGoto { cond, dst })
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        if let Some(cond) = self.cond.as_mut() {
            cond.solve(sleigh, execution, solved)?;
            if cond.size(sleigh, execution).is_undefined() {
                solved.iam_not_finished(cond.src(), file!(), line!());
            }
        }
        Ok(())
    }

    pub fn convert(self) -> FinalLocalGoto {
        let cond = self.cond.map(|x| x.convert());
        let dst = self.dst;
        FinalLocalGoto { cond, dst }
    }
}
