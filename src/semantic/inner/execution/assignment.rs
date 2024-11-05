use std::ops::Range;

use crate::execution::Binary;
use crate::semantic::execution::{
    Assignment as FinalAssignment, AssignmentOp as FinalAssignmentOp,
    WriteValue,
};
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::{
    ExecutionError, NumberNonZeroUnsigned, NumberUnsigned, Span, VarSizeError,
};

use super::{
    len, Execution, Expr, ExprBinaryOp, ExprElement, ExprUnaryOp, ExprValue,
    FieldSize, FieldSizeMut, MemoryLocation, Unary,
};

#[derive(Clone, Debug)]
pub struct Assignment {
    pub location: Span,
    pub var_location: Span,
    pub var: WriteValue,
    pub op: Option<AssignmentOp>,
    pub right: Expr,
}

impl Assignment {
    pub fn new(
        var_location: Span,
        var: WriteValue,
        op: Option<AssignmentOp>,
        location: Span,
        right: Expr,
    ) -> Self {
        Self {
            var_location,
            var,
            op,
            location,
            right,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        self.right.solve(sleigh, execution, solved)?;

        // solve simple expr that don't follow many rules
        if hack_solve_simple_bin_ands(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        // add extra information for the variable creation
        if hack_extra_var_info_creation(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        // identify the implicity truncation for varnodes
        if hack_assignemnt_left_hand_truncation_implied(self, sleigh, execution)
        {
            solved.i_did_a_thing();
        }

        // identify the implicity truncation for left size if one byte varnode
        if hack_1_byte_varnode_assign_to_bit(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        // TODO check left size can be truncated correctly

        // left and right sizes are the same

        let modified = len::a_receive_b(
            &mut *self
                .op
                .as_mut()
                .map(|trunc| trunc.output_size_mut())
                .unwrap_or_else(|| self.var.size_mut(sleigh, execution)),
            &mut *self.right.size_mut(sleigh, execution),
        );
        if modified.ok_or_else(|| VarSizeError::AssignmentSides {
            left: self.left_size(sleigh, execution),
            right: self.right.size(sleigh, execution),
            location: self.location.clone(),
            backtrace: format!("{}:{}", file!(), line!()),
        })? {
            solved.i_did_a_thing()
        }

        if self.var.size(sleigh, execution).is_undefined()
            || self.right.size(sleigh, execution).is_undefined()
        {
            solved.iam_not_finished(self.right.src(), file!(), line!())
        }

        Ok(())
    }

    pub fn left_size<'a>(
        &'a self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> FieldSize {
        self.op
            .as_ref()
            .map(|trunc| trunc.output_size())
            .unwrap_or_else(|| self.var.size(sleigh, execution))
    }

    pub fn left_size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        self.op
            .as_mut()
            .map(|trunc| trunc.output_size_mut())
            .unwrap_or_else(|| self.var.size_mut(sleigh, execution))
    }

    pub fn convert(self) -> FinalAssignment {
        FinalAssignment {
            location: self.location,
            var: self.var,
            op: self.op.map(|op| op.convert()),
            right: self.right.convert(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AssignmentOp {
    TakeLsb(NumberNonZeroUnsigned),
    TrunkLsb {
        bytes: NumberUnsigned,
        output_size: FieldSize,
    },
    BitRange(Range<NumberUnsigned>),
}

impl AssignmentOp {
    pub fn output_size_mut(&mut self) -> Box<dyn FieldSizeMut + '_> {
        match self {
            AssignmentOp::TakeLsb(bytes) => {
                Box::new(len::FieldSizeUnmutable(FieldSize::new_bytes(*bytes)))
            }
            AssignmentOp::TrunkLsb {
                bytes: _,
                output_size,
            } => Box::new(output_size),
            AssignmentOp::BitRange(bits) => {
                Box::new(len::FieldSizeUnmutable(FieldSize::new_bits(
                    (bits.end - bits.start).try_into().unwrap(),
                )))
            }
        }
    }
    pub fn output_size(&self) -> FieldSize {
        match self {
            AssignmentOp::TakeLsb(bytes) => FieldSize::new_bytes(*bytes),
            AssignmentOp::TrunkLsb {
                bytes: _,
                output_size,
            } => *output_size,
            AssignmentOp::BitRange(bits) => {
                FieldSize::new_bits((bits.end - bits.start).try_into().unwrap())
            }
        }
    }
    pub fn convert(self) -> FinalAssignmentOp {
        match self {
            AssignmentOp::TakeLsb(x) => FinalAssignmentOp::TakeLsb(x),
            AssignmentOp::TrunkLsb {
                bytes,
                output_size: _,
            } => FinalAssignmentOp::TrunkLsb(bytes),
            AssignmentOp::BitRange(x) => FinalAssignmentOp::BitRange(x),
        }
    }
}

// HACK auto solve simple assignments.
// eg: local tmp:2 = disassembly_var & xFFFF;
fn hack_solve_simple_bin_ands(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // left need to have a known size
    let Some(ass_size) = ass.left_size(sleigh, execution).final_value() else {
        return false;
    };

    fn get_simple_value_size(value: &mut ExprValue) -> Option<&mut FieldSize> {
        Some(match value {
            ExprValue::Int(var) => &mut var.size,
            ExprValue::TokenField(tf) => &mut tf.size,
            ExprValue::Context(ctx) => &mut ctx.size,
            ExprValue::Bitrange(bt) => &mut bt.size,
            ExprValue::DisVar(dis) => &mut dis.size,
            _ => return None,
        })
    }

    match &mut ass.right {
        // if a simple value, just assign the size to the value
        Expr::Value(ExprElement::Value { location: _, value }) => {
            let Some(var_size) = get_simple_value_size(value) else {
                return false;
            };

            var_size
                .update_action(|var| var.set_final_value(ass_size))
                .unwrap()
        }

        // if a simple binary expr with two simple values, just set all of then to the same size
        Expr::Op(ExprBinaryOp {
            location: _,
            output_size,
            op: Binary::BitOr | Binary::BitAnd | Binary::BitXor,
            left,
            right,
        }) => {
            let (
                Expr::Value(ExprElement::Value {
                    location: _,
                    value: expr_left,
                }),
                Expr::Value(ExprElement::Value {
                    location: _,
                    value: expr_right,
                }),
            ) = (left.as_mut(), right.as_mut())
            else {
                return false;
            };

            let Some(left_size) = get_simple_value_size(expr_left) else {
                return false;
            };

            let Some(right_size) = get_simple_value_size(expr_right) else {
                return false;
            };

            let mut result = false;
            result |= left_size
                .update_action(|x| x.set_final_value(ass_size))
                .unwrap();
            result |= right_size
                .update_action(|x| x.set_final_value(ass_size))
                .unwrap();
            result |= output_size
                .update_action(|x| x.set_final_value(ass_size))
                .unwrap();
            result
        }

        _ => false,
    }
}

// HACK get extra info for the variable during it's creation
// eg: local tmp = *:1 value; # this var is always 1 byte
fn hack_extra_var_info_creation(
    ass: &mut Assignment,
    _sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    let WriteValue::Local { id, creation: true } = ass.var else {
        return false;
    };

    let var = execution.variable(id);
    if !var.size.get().is_fully_undefined() {
        return false;
    }

    match &ass.right {
        // in a deref from address, just use the size that is being deref as size
        Expr::Value(ExprElement::Op(ExprUnaryOp {
            location: _,
            op:
                Unary::Dereference(MemoryLocation {
                    space: _,
                    size,
                    location: _,
                }),
            input: _,
        })) => {
            var.size.set(*size);
            true
        }

        _ => false,
    }
}

// HACK: sometimes when assigning a value smaller then the size of the left side,
// eg one bit registers are declare as one
// byte instead of bitranges (eg flags on X86 like ZF), and assigned
// binary values to it, when that happen, it's unclear if zext or a left
// hand truncation happen, I'll stick with left hand operation for now
fn hack_assignemnt_left_hand_truncation_implied(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // left hand need to be an varnode or local variable
    let left_size = match &ass.var {
        WriteValue::Varnode(id) => {
            let var = sleigh.varnode(*id);
            var.len_bytes.get() * 8
        }
        WriteValue::Local { id, creation: _ } => {
            let var = execution.variable(*id);
            // TODO maybe allow non explicit declared variables
            if !var.explicit {
                return false;
            }
            let Some(bits) = var.size.get().final_value() else {
                return false;
            };
            bits.get()
        }
        _ => return false,
    };

    // can't have operators on the left side
    if ass.op.is_some() {
        return false;
    }

    // left side need to be smaller then the right side
    let Some(right_size) = ass.right.size(sleigh, execution).max_bits() else {
        return false;
    };
    if left_size < right_size.get() {
        return false;
    }

    // NOTE this could also be solved by adding a zext to the right side, but
    // it seems that a left hand truncation is implied because this only happens
    // if the rest of the varnode is not important.
    ass.op = Some(AssignmentOp::BitRange(0..right_size.get()));
    true
}

// HACK some times, single bit register will be declared as single byte register.
// but in the execution body the value will be assign to a single bit variable
fn hack_1_byte_varnode_assign_to_bit(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // right hand need to be one byte sized
    if ass.right.size(sleigh, execution).final_value()
        != Some(8.try_into().unwrap())
    {
        return false;
    }

    // left hand need to be one bit len
    if ass.left_size(sleigh, execution).final_value()
        != Some(1.try_into().unwrap())
    {
        return false;
    }

    // truncate the right size to one bit

    // dummy value for temporary use
    let location = Span::File(crate::FileSpan {
        start: crate::FileLocation {
            file: std::rc::Rc::from(std::path::Path::new("")),
            line: 0,
            column: 0,
        },
        end_line: 0,
        end_column: 0,
    });
    let mut swap_right = Expr::Value(ExprElement::Value {
        location,
        value: ExprValue::Int(super::ExprNumber {
            size: FieldSize::default(),
            number: crate::Number::Positive(0),
        }),
    });
    core::mem::swap(&mut ass.right, &mut swap_right);
    ass.right = Expr::Value(ExprElement::new_op(
        ass.location.clone(),
        Unary::BitRange {
            range: 0..1,
            size: FieldSize::Value(1.try_into().unwrap()),
        },
        swap_right,
    ));

    true
}
