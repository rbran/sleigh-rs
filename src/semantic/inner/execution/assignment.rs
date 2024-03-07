use crate::semantic::execution::{
    Assignment as FinalAssignment, Unary, WriteValue,
};
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::{ExecutionError, Number, Span};

use super::{
    restrict_field_same_size, Expr, ExprElement, ExprNumber, ExprUnaryOp,
    ExprValue, FieldSize, Truncate, Variable,
};

#[derive(Clone, Debug)]
pub struct Assignment {
    pub var: WriteValue,
    op: Option<Truncate>,
    src: Span,
    pub right: Expr,
}

impl Assignment {
    pub fn new(
        var: WriteValue,
        op: Option<Truncate>,
        src: Span,
        right: Expr,
    ) -> Self {
        Self {
            var,
            op,
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
        let error_src = self.right.src().clone();
        let error = || Box::new(ExecutionError::VarSize(error_src.clone()));
        self.right.solve(sleigh, variables, solved)?;

        //exception in case the right result is 1 bit and the left is
        //less then 1 byte, auto add zext the right side
        let left_size = self
            .op
            .as_ref()
            .map(|op| op.output_size())
            .unwrap_or_else(|| self.var.size(sleigh, variables))
            .final_value();
        let right_size = self.right.size(sleigh, variables).final_value();
        if left_size
            .zip(right_size)
            .map(|(left, right)| left.get() == 8 && right.get() < 8)
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
            self.right = Expr::Value(ExprElement::Op(ExprUnaryOp {
                location: taken.src().clone(),
                output_size: FieldSize::new_bits(left_size.unwrap()),
                op: Unary::Zext,
                input: Box::new(taken),
            }));
            self.right.solve(sleigh, variables, solved)?;
            solved.i_did_a_thing()
        }

        //left and right sizes are the same
        if let Some(trunc) = &mut self.op {
            let modified = restrict_field_same_size(&mut [
                self.right.size_mut(sleigh, variables).as_dyn(),
                trunc.output_size_mut(),
            ]);
            if modified.ok_or_else(error)? {
                solved.i_did_a_thing()
            }
        } else {
            let modified = restrict_field_same_size(&mut [
                &mut *self.right.size_mut(sleigh, variables),
                &mut *self.var.size_mut(sleigh, variables),
            ]);

            //if right size is possible min, so does left if size is not defined
            if self.var.size(sleigh, variables).is_undefined()
                && self.right.size(sleigh, variables).possible_min()
                && self
                    .var
                    .size_mut(sleigh, variables)
                    .update_action(|size| Some(size.set_possible_min()))
                    .unwrap()
            {
                solved.i_did_a_thing();
            }
            if modified.ok_or_else(error)? {
                solved.i_did_a_thing()
            }
        }

        if self.var.size(sleigh, variables).is_undefined()
            || self.right.size(sleigh, variables).is_undefined()
        {
            solved.iam_not_finished(self.right.src(), file!(), line!())
        }

        Ok(())
    }
    pub fn convert(self) -> FinalAssignment {
        FinalAssignment {
            location: self.src,
            var: self.var,
            op: self.op.map(|op| op.convert()),
            right: self.right.convert(),
        }
    }
}
