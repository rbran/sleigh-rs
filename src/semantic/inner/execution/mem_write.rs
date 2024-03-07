use crate::semantic::execution::{MemWrite as FinalMemWrite, Unary};
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::{ExecutionError, Number, Span};

use super::{
    Execution, Expr, ExprElement, ExprNumber, ExprValue, FieldSize,
    MemoryLocation, Truncate, Variable,
};

#[derive(Clone, Debug)]
pub struct MemWrite {
    pub addr: Expr,
    mem: MemoryLocation,
    truncate: Option<Truncate>,
    src: Span,
    pub right: Expr,
}

impl MemWrite {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut addr: Expr,
        truncate: Option<Truncate>,
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
                .size(sleigh, &execution.vars)
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
        addr.size_mut(sleigh, &execution.vars)
            .set(FieldSize::new_bytes(space.addr_bytes));
        Self {
            addr,
            mem,
            truncate,
            src,
            right,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        self.right.solve(sleigh, variables, solved)?;
        // HACK: exception in case the right size is smaller then the left size,
        // truncate the right size with a msb(0)
        let left_size = self.mem.size.final_value();
        let right_size = self.right.size(sleigh, variables).final_value();
        if left_size
            .zip(right_size)
            .map(|(left, right)| left.get() < right.get())
            .unwrap_or(false)
        {
            //dummy value
            let dummy_src = Span::File(crate::FileSpan {
                start: crate::FileLocation {
                    file: std::rc::Rc::from(std::path::Path::new("")),
                    line: 0,
                    column: 0,
                },
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
            self.right = Expr::Value(ExprElement::Truncate(
                taken.src().clone(),
                Truncate::new(0, left_size.unwrap()),
                Box::new(taken),
            ));
            self.right.solve(sleigh, variables, solved)?;
            solved.i_did_a_thing()
        }

        //if the left side is WriteAddr without a size, the only option is to
        //use the left side size
        if let Some(write_size) = self.mem.size.final_value() {
            //if left side size is known, right side will need to produce a
            //a value with size equal or smaller then that
            let modified = self
                .right
                .size_mut(sleigh, variables)
                .update_action(|size| {
                    size.set_max_bits(write_size)?
                        .set_possible_value(write_size)
                });
            if modified.ok_or_else(|| {
                Box::new(ExecutionError::VarSize(self.mem.src.clone()))
            })? {
                solved.i_did_a_thing();
            }
        } else {
            //if the left side is WriteAddr without a size, the only option is
            //to use the right side size
            let modified = self.mem.size.update_action(|size| {
                size.intersection(self.right.size(sleigh, variables))
            });
            if modified.ok_or_else(|| {
                Box::new(ExecutionError::VarSize(self.src.clone()))
            })? {
                solved.i_did_a_thing();
            }
        }

        if self.addr.size(sleigh, variables).is_undefined()
            || self.right.size(sleigh, variables).is_undefined()
        {
            solved.iam_not_finished(self.right.src(), file!(), line!())
        }

        Ok(())
    }
    pub fn convert(self) -> FinalMemWrite {
        FinalMemWrite {
            addr: self.addr.convert(),
            mem: self.mem.convert(),
            truncate: self.truncate.map(Truncate::convert),
            right: self.right.convert(),
        }
    }
}
