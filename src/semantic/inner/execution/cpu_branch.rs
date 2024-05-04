use crate::semantic::execution::BranchCall;
use crate::semantic::execution::CpuBranch as FinalCpuBranch;
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::ExecutionError;
use crate::VarSizeError;

use super::{Execution, Expr, Variable};

#[derive(Clone, Debug)]
pub struct CpuBranch {
    pub cond: Option<Expr>,
    pub call: BranchCall,
    //TODO delete direct?
    direct: bool,
    pub dst: Expr,
}

impl CpuBranch {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut cond: Option<Expr>,
        call: BranchCall,
        direct: bool,
        dst: Expr,
    ) -> Self {
        //condition can have any size, preferencially 1 bit for true/false
        cond.iter_mut().for_each(|cond| {
            cond.size_mut(sleigh, &execution.vars)
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        CpuBranch {
            cond,
            call,
            direct,
            dst,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        if let Some(cond) = self.cond.as_mut() {
            cond.solve(sleigh, variables, solved)?;
        }
        // jmp dst addr can be equal or smaller then space address size
        let modified =
            self.dst.size_mut(sleigh, variables).update_action(|size| {
                size.set_max_bytes(sleigh.addr_bytes().unwrap())
            });

        if modified.ok_or_else(|| VarSizeError::AddressTooBig {
            address_size: self.dst.size(sleigh, variables),
            space_bytes: sleigh.addr_bytes().unwrap(),
            location: self.dst.src().clone(),
        })? {
            solved.i_did_a_thing();
        }

        self.dst.solve(sleigh, variables, solved)?;
        Ok(())
    }
    pub fn convert(self) -> FinalCpuBranch {
        let cond = self.cond.map(|cond| cond.convert());
        let dst = self.dst.convert();
        FinalCpuBranch {
            cond,
            call: self.call,
            direct: self.direct,
            dst,
        }
    }
}
