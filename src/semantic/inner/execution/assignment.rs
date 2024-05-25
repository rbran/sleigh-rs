use std::ops::Range;

use crate::semantic::execution::{
    Assignment as FinalAssignment, AssignmentOp as FinalAssignmentOp,
    WriteValue, WriteVarnode,
};
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::{
    ExecutionError, NumberNonZeroUnsigned, NumberUnsigned, Span, VarSizeError,
};

use super::{len, Execution, Expr, FieldSize, FieldSizeMut};

#[derive(Clone, Debug)]
pub struct Assignment {
    pub var: WriteValue,
    pub(crate) op: Option<AssignmentOp>,
    pub(crate) src: Span,
    pub right: Expr,
}

impl Assignment {
    pub fn new(
        var: WriteValue,
        op: Option<AssignmentOp>,
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
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        self.right.solve(sleigh, execution, solved)?;

        // identify the implicity truncation for varnodes
        if hack_varnode_assignemnt_left_hand_truncation_implied(
            self, sleigh, execution,
        ) {
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
            left: self.var.size(sleigh, execution),
            right: self.right.size(sleigh, execution),
            location: self.src.clone(),
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
    pub fn convert(self) -> FinalAssignment {
        FinalAssignment {
            location: self.src,
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
                Box::new(len::FieldSizeUnmutable(FieldSize::from(
                    FieldSize::Value((bytes.get() * 8).try_into().unwrap()),
                )))
            }
            AssignmentOp::TrunkLsb {
                bytes: _,
                output_size,
            } => Box::new(output_size),
            AssignmentOp::BitRange(bits) => Box::new(len::FieldSizeUnmutable(
                FieldSize::Value((bits.end - bits.start).try_into().unwrap()),
            )),
        }
    }
    pub fn output_size(&self) -> FieldSize {
        match self {
            AssignmentOp::TakeLsb(bytes) => {
                FieldSize::Value((bytes.get() * 8).try_into().unwrap())
            }
            AssignmentOp::TrunkLsb {
                bytes: _,
                output_size,
            } => *output_size,
            AssignmentOp::BitRange(bits) => {
                FieldSize::Value((bits.end - bits.start).try_into().unwrap())
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

// HACK: sometimes when assigning to varnodes, a value smaller then the
// varnode size is provided, eg one bit registers are declare as one
// byte instead of bitranges (eg flags on X86 like ZF), and assigned
// binary values to it, when that happen, it's unclear if zext or a left
// hand truncation happen, I'll stick with left hand operation for now
fn hack_varnode_assignemnt_left_hand_truncation_implied(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // left hand need to be an varnode
    let WriteValue::Varnode(WriteVarnode { id, location: _ }) = &ass.var else {
        return false;
    };

    // can't have operators on the left side
    if ass.op.is_some() {
        return false;
    }

    // left side need to be smaller then the right side
    let Some(right_size) = ass.right.size(sleigh, execution).max_bits() else {
        return false;
    };
    let varnode = sleigh.varnode(*id);
    let left_size = varnode.len_bytes.get() * 8;
    if left_size < right_size.get() {
        return false;
    }

    // NOTE this could also be solved by adding a zext to the right side, but
    // it seems that a left hand truncation is implied because this only happens
    // if the rest of the varnode is not important.
    ass.op = Some(AssignmentOp::BitRange(0..right_size.get()));
    true
}
