use crate::semantic::execution::{MemWrite as FinalMemWrite, Unary};
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::{ExecutionError, Number, Span, VarSizeError};

use super::{
    Execution, Expr, ExprElement, ExprNumber, ExprValue, FieldSize,
    MemoryLocation,
};

#[derive(Clone, Debug)]
pub struct MemWrite {
    pub addr: Expr,
    pub(crate) mem: MemoryLocation,
    pub(crate) src: Span,
    pub right: Expr,
}

impl MemWrite {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut addr: Expr,
        mem: MemoryLocation,
        src: Span,
        mut right: Expr,
    ) -> Self {
        //HACK: unexplained exeptions:
        //if the mem size is 1byte and value produced by right is 1bit, auto
        //zext it
        if mem
            .size
            .final_value()
            .map(|size| size.get() == 8)
            .unwrap_or(false)
            && right
                .size(sleigh, &execution)
                .final_value()
                .map(|size| size.get() == 1)
                .unwrap_or(false)
        {
            right = Expr::Value(ExprElement::new_op(
                right.src().clone(),
                Unary::Zext,
                right,
            ))
        }

        //addr expr is the addr to access the space, so it need to be space
        //addr size
        let space = sleigh.space(mem.space);
        addr.size_mut(sleigh, &execution)
            .set(FieldSize::new_bytes(space.addr_bytes));
        Self {
            addr,
            mem,
            src,
            right,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        self.addr.solve(sleigh, execution, solved)?;
        self.right.solve(sleigh, execution, solved)?;
        // HACK: exception in case the right size is smaller then the left size,
        // truncate the right size with a msb(0)
        let left_size = self.mem.size.final_value();
        let right_size = self.right.size(sleigh, execution).final_value();
        if left_size
            .zip(right_size)
            .map(|(left, right)| left.get() < right.get())
            .unwrap_or(false)
        {
            //dummy value
            let dummy_src = Span::File(crate::FileSpan {
                start: crate::FileLocation::new_start(""),
                end_line: 0,
                end_column: 0,
            });
            let mut taken =
                Expr::Value(ExprElement::Value(ExprValue::Int(ExprNumber {
                    location: dummy_src,
                    size: FieldSize::default(),
                    number: Number::Positive(0),
                })));
            std::mem::swap(&mut self.right, &mut taken);
            self.right = Expr::Value(ExprElement::new_trunk_lsb(
                taken.src().clone(),
                0,
                taken,
            ));
            self.right.solve(sleigh, execution, solved)?;
            solved.i_did_a_thing()
        }

        //if the left side is WriteAddr without a size, the only option is to
        //use the left side size
        if let Some(write_size) = self.mem.size.final_value() {
            //if left side size is known, right side will need to produce a
            //a value with size equal or smaller then that
            let right_size = self.right.size(sleigh, execution);
            let modified = self
                .right
                .size_mut(sleigh, execution)
                .update_action(|size| {
                    size.set_max_bits(write_size)?.set_possible_bits(write_size)
                })
                .ok_or_else(|| VarSizeError::AssignmentSides {
                    left: FieldSize::Value(write_size),
                    right: right_size,
                    location: self.src.clone(),
                })?;
            if modified {
                solved.i_did_a_thing();
            }
        } else {
            //if the left side is WriteAddr without a size, the only option is
            //to use the right side size
            let modified = self
                .mem
                .size
                .update_action(|size| {
                    size.intersection(self.right.size(sleigh, execution))
                })
                .ok_or_else(|| VarSizeError::AssignmentSides {
                    left: self.mem.size,
                    right: self.right.size(sleigh, execution),
                    location: self.src.clone(),
                })?;
            if modified {
                solved.i_did_a_thing();
            }
        }

        if self.addr.size(sleigh, execution).is_undefined()
            || self.right.size(sleigh, execution).is_undefined()
        {
            solved.iam_not_finished(self.right.src(), file!(), line!())
        }

        Ok(())
    }
    pub fn convert(self) -> FinalMemWrite {
        FinalMemWrite {
            addr: self.addr.convert(),
            mem: self.mem.convert(),
            right: self.right.convert(),
        }
    }
}
