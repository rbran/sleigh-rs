use std::ops::Range;

use crate::execution::{Binary, VariableId};
use crate::semantic::execution::{
    Assignment as FinalAssignment, AssignmentOp as FinalAssignmentOp,
    AssignmentWrite as FinalAssignmentType,
    AssignmentWriteVariable as FinalAssignmentValueWrite,
};
use crate::semantic::inner::execution::TableExportType;
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::{
    AttachVarnodeId, BitrangeId, ExecutionError, NumberNonZeroUnsigned,
    NumberUnsigned, Span, TableId, TokenFieldId, VarSizeError, VarnodeId,
};

use super::{
    len, Execution, Expr, ExprBinaryOp, ExprElement, ExprNumber, ExprUnaryOp,
    ExprValue, FieldSize, FieldSizeMut, FieldSizeTableExport,
    FieldSizeUnmutable, MemoryLocation, Unary,
};

#[derive(Clone, Debug)]
pub struct Assignment {
    pub location: Span,
    pub var_location: Span,
    pub var: AssignmentWrite,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub enum AssignmentWrite {
    Variable {
        value: AssignmentWriteVariable,
        op: Option<AssignmentOp>,
    },
    Memory {
        mem: MemoryLocation,
        addr: Expr,
    },
    // write to memory based on the table export
    TableExport {
        table_id: TableId,
        op: Option<AssignmentOp>,
    },
    // table export, if a reference, the size is just a sugestion,
    // each write could have a diferent size.
    // should only be created if the write have diferent size from the export
    TableReferenceExport {
        table_id: TableId,
        size: FieldSize,
    },
}
impl AssignmentWrite {
    fn convert(self) -> FinalAssignmentType {
        match self {
            AssignmentWrite::Variable { value, op } => {
                FinalAssignmentType::Variable {
                    value: value.convert(),
                    op: op.map(AssignmentOp::convert),
                }
            }
            AssignmentWrite::Memory { mem, addr } => {
                FinalAssignmentType::Memory {
                    mem: mem.convert(),
                    addr: addr.convert(),
                }
            }
            AssignmentWrite::TableExport { table_id, op } => {
                FinalAssignmentType::TableExport {
                    table_id,
                    op: op.map(AssignmentOp::convert),
                    size: None,
                }
            }
            AssignmentWrite::TableReferenceExport { table_id, size } => {
                FinalAssignmentType::TableExport {
                    table_id,
                    op: None,
                    size: size.possible_value(),
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum AssignmentWriteVariable {
    Varnode(VarnodeId),
    Bitrange(BitrangeId),
    TokenField {
        token_field_id: TokenFieldId,
        attach_id: AttachVarnodeId,
    },
    Local {
        id: VariableId,
        creation: bool,
    },
}
impl AssignmentWriteVariable {
    fn convert(self) -> FinalAssignmentValueWrite {
        match self {
            AssignmentWriteVariable::Varnode(var) => {
                FinalAssignmentValueWrite::Varnode(var)
            }
            AssignmentWriteVariable::Bitrange(bit) => {
                FinalAssignmentValueWrite::Bitrange(bit)
            }
            AssignmentWriteVariable::TokenField {
                token_field_id,
                attach_id,
            } => FinalAssignmentValueWrite::TokenField {
                token_field_id,
                attach_id,
            },
            AssignmentWriteVariable::Local { id, creation: _ } => {
                FinalAssignmentValueWrite::Variable(id)
            }
        }
    }
}

impl Assignment {
    pub fn new(
        var_location: Span,
        var: AssignmentWrite,
        location: Span,
        right: Expr,
    ) -> Self {
        // TODO table export can't be writen if is value or const
        Self {
            var_location,
            var,
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

        // change the table mem write size
        if hack_solve_table_reference_export_size(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        // solve simple expr that don't follow many rules
        if hack_solve_simple_bin_ands(self, sleigh, execution)? {
            solved.i_did_a_thing();
        }

        // add auto truncate in simple bitand expr
        if hack_auto_fix_bitrange_with_bitand(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        // add extra information for the variable creation
        if hack_extra_var_info_creation(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        // identify the implicity truncation for varnodes
        if hack_auto_zext_right_side(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        // identify the implicity truncation for left size if one byte varnode
        if hack_1_byte_varnode_assign_to_bit(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        // add auto truncate in mem deref
        if hack_auto_trunkate_right_side(self, sleigh, execution) {
            solved.i_did_a_thing();
        }

        if hack_force_solve_usercall(self, sleigh, execution)? {
            solved.i_did_a_thing();
        }

        // TODO check left size can be truncated correctly

        // left and right sizes are the same
        let modified = {
            let (mut left, mut right) =
                self.left_and_right_size_mut(sleigh, execution);
            len::a_receive_b(&mut *left, &mut *right)
        };
        if modified.ok_or_else(|| VarSizeError::AssignmentSides {
            left: self.left_size(sleigh, execution),
            right: self.right.size(sleigh, execution),
            location: self.location.clone(),
            backtrace: format!("{}:{}", file!(), line!()),
        })? {
            solved.i_did_a_thing()
        }

        match &mut self.var {
            AssignmentWrite::Variable { value, op } => {
                if let Some(op) = op {
                    if op.output_size().is_undefined() {
                        solved.iam_not_finished(
                            &self.var_location,
                            file!(),
                            line!(),
                        )
                    }
                }
                match value {
                    AssignmentWriteVariable::Local { id, creation: _ } => {
                        let var = execution.variable(*id);
                        if var.size.get().is_undefined() {
                            solved.iam_not_finished(
                                &self.var_location,
                                file!(),
                                line!(),
                            )
                        }
                    }
                    AssignmentWriteVariable::Varnode(_)
                    | AssignmentWriteVariable::Bitrange(_)
                    | AssignmentWriteVariable::TokenField { .. } => {}
                }
            }
            AssignmentWrite::Memory { mem, addr } => {
                if mem.size.is_undefined() {
                    solved.iam_not_finished(
                        &self.var_location,
                        file!(),
                        line!(),
                    )
                }
                addr.solve(sleigh, execution, solved)?;
            }
            AssignmentWrite::TableExport { table_id: _, op } => {
                if let Some(op) = op {
                    if op.output_size().is_undefined() {
                        solved.iam_not_finished(
                            &self.var_location,
                            file!(),
                            line!(),
                        )
                    }
                }
            }
            AssignmentWrite::TableReferenceExport { table_id: _, size } => {
                if size.is_undefined() {
                    solved.iam_not_finished(
                        &self.var_location,
                        file!(),
                        line!(),
                    )
                }
            }
        }

        Ok(())
    }

    pub fn left_size(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> FieldSize {
        match &self.var {
            AssignmentWrite::TableExport { table_id, op: None } => {
                let table = sleigh.table(*table_id);
                let table_export = *table.export.borrow();
                *table_export.unwrap().size().unwrap()
            }
            AssignmentWrite::Memory { mem, .. } => mem.size,
            AssignmentWrite::Variable { op: Some(op), .. }
            | AssignmentWrite::TableExport { op: Some(op), .. } => {
                op.output_size()
            }
            AssignmentWrite::Variable {
                op: None,
                value: AssignmentWriteVariable::Varnode(var),
            } => FieldSize::new_bytes(sleigh.varnode(*var).len_bytes),
            AssignmentWrite::Variable {
                op: None,
                value: AssignmentWriteVariable::Bitrange(bit),
            } => FieldSize::new_bits(sleigh.bitrange(*bit).bits.len()),
            AssignmentWrite::Variable {
                op: None,
                value: AssignmentWriteVariable::TokenField { attach_id, .. },
            } => FieldSize::new_bytes(
                sleigh.attach_varnodes_len_bytes(*attach_id),
            ),
            AssignmentWrite::Variable {
                op: None,
                value: AssignmentWriteVariable::Local { id, creation: _ },
            } => execution.variable(*id).size.get(),
            AssignmentWrite::TableReferenceExport { size, .. } => *size,
        }
    }

    pub fn left_and_right_size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> (Box<dyn FieldSizeMut + 'a>, Box<dyn FieldSizeMut + 'a>) {
        let left = match &mut self.var {
            AssignmentWrite::TableReferenceExport { size, .. } => {
                Box::new(size)
            }
            AssignmentWrite::Variable { op: Some(op), .. }
            | AssignmentWrite::TableExport { op: Some(op), .. } => {
                op.output_size_mut()
            }
            AssignmentWrite::Memory { mem, .. } => Box::new(&mut mem.size),
            AssignmentWrite::Variable {
                op: None,
                value: AssignmentWriteVariable::Varnode(var),
            } => Box::new(FieldSizeUnmutable(FieldSize::new_bytes(
                sleigh.varnode(*var).len_bytes,
            ))),
            AssignmentWrite::Variable {
                op: None,
                value: AssignmentWriteVariable::Bitrange(bit),
            } => Box::new(FieldSizeUnmutable(FieldSize::new_bits(
                sleigh.bitrange(*bit).bits.len(),
            ))),
            AssignmentWrite::Variable {
                op: None,
                value: AssignmentWriteVariable::TokenField { attach_id, .. },
            } => Box::new(FieldSizeUnmutable(FieldSize::new_bytes(
                sleigh.attach_varnodes_len_bytes(*attach_id),
            ))),
            AssignmentWrite::Variable {
                op: None,
                value: AssignmentWriteVariable::Local { id, creation: _ },
            } => Box::new(&execution.variable(*id).size),
            AssignmentWrite::TableExport { table_id, op: None } => {
                let table = sleigh.table(*table_id);
                Box::new(FieldSizeTableExport(&table.export))
            }
        };
        let right = self.right.size_mut(sleigh, execution);
        (left, right)
    }

    pub fn convert(self) -> FinalAssignment {
        FinalAssignment {
            location: self.location,
            var: self.var.convert(),
            right: self.right.convert(),
        }
    }

    fn swap_right(&mut self, mut new_right: impl FnMut(Expr) -> Expr) {
        // dummy value for temporary use
        let mut swap_right = Expr::Value(ExprElement::Value {
            location: Span::File(crate::FileSpan {
                start: crate::FileLocation {
                    file: std::rc::Rc::from(std::path::Path::new("")),
                    line: 0,
                    column: 0,
                },
                end_line: 0,
                end_column: 0,
            }),
            value: ExprValue::Int(ExprNumber {
                size: FieldSize::default(),
                number: crate::Number::Positive(0),
            }),
        });
        core::mem::swap(&mut self.right, &mut swap_right);
        self.right = new_right(swap_right);
    }
}

#[derive(Clone, Debug)]
pub struct MacroParamAssignment {
    pub var: VariableId,
    pub right: Expr,
}

impl MacroParamAssignment {
    pub fn new(var: VariableId, right: Expr) -> Self {
        Self { var, right }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        self.right.solve(sleigh, execution, solved)?;

        let var = execution.variable(self.var);
        let result = len::a_equivalent_b(
            &mut &var.size,
            &mut *self.right.size_mut(sleigh, execution),
        );
        let did_something =
            result.ok_or_else(|| VarSizeError::AssignmentSides {
                left: var.size.get(),
                right: self.right.size(sleigh, execution),
                location: self.right.src().clone(),
                backtrace: format!("{}:{}", file!(), line!()),
            })?;

        if did_something {
            solved.i_did_a_thing();
        }

        Ok(())
    }

    pub fn convert(self) -> FinalAssignment {
        FinalAssignment {
            location: self.right.src().clone(),
            var: FinalAssignmentType::Variable {
                value: FinalAssignmentValueWrite::Variable(self.var),
                op: None,
            },
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

// HACK solve table export reference size
// eg: Mem = temp:1; # Mem is a ref to 4 bytes in Ram
fn hack_solve_table_reference_export_size(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // left need to be a table
    let AssignmentWrite::TableExport { table_id, op: None } = &ass.var else {
        return false;
    };
    // table need to export a reference
    let table = sleigh.table(*table_id);
    let table_export = *table.export.borrow();
    let Some(TableExportType::Reference {
        len: left_size,
        space: _,
        also_values: _,
    }) = table_export
    else {
        return false;
    };
    // table with a defined export size
    let Some(left_size) = left_size.final_value() else {
        return false;
    };

    // right need to have a defined size
    let Some(right_size) = ass.right.size(sleigh, execution).final_value()
    else {
        return false;
    };

    // only if right and left size differ
    if left_size == right_size {
        return false;
    }

    // write the number of bytes of the right side
    ass.var = AssignmentWrite::TableReferenceExport {
        table_id: *table_id,
        size: FieldSize::new_bits(right_size),
    };
    true
}

// HACK auto solve simple assignments.
// eg: local tmp:2 = disassembly_var & xFFFF;
fn hack_solve_simple_bin_ands(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> Result<bool, Box<VarSizeError>> {
    // left need to have a known size
    let Some(ass_size) = ass.left_size(sleigh, execution).final_value() else {
        return Ok(false);
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
                return Ok(false);
            };

            var_size
                .update_action(|var| var.set_final_value(ass_size))
                .ok_or_else(|| {
                    Box::new(VarSizeError::AssignmentSides {
                        left: FieldSize::new_bits(ass_size),
                        right: ass.right.size(sleigh, execution),
                        location: ass.location.clone(),
                        backtrace: format!("{}:{}", file!(), line!()),
                    })
                })
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
                return Ok(false);
            };

            let Some(left_size) = get_simple_value_size(expr_left) else {
                return Ok(false);
            };

            let Some(right_size) = get_simple_value_size(expr_right) else {
                return Ok(false);
            };

            let result_right =
                right_size.update_action(|x| x.set_final_value(ass_size));
            let result_right = result_right.ok_or_else(|| {
                Box::new(VarSizeError::AssignmentSides {
                    left: FieldSize::new_bits(ass_size),
                    right: right.size(sleigh, execution),
                    location: ass.location.clone(),
                    backtrace: format!("{}:{}", file!(), line!()),
                })
            })?;
            let result_left =
                left_size.update_action(|x| x.set_final_value(ass_size));
            let result_left = result_left.ok_or_else(|| {
                Box::new(VarSizeError::AssignmentSides {
                    left: FieldSize::new_bits(ass_size),
                    right: right.size(sleigh, execution),
                    location: ass.location.clone(),
                    backtrace: format!("{}:{}", file!(), line!()),
                })
            })?;
            let result_output =
                output_size.update_action(|x| x.set_final_value(ass_size));
            let result_output = result_output.ok_or_else(|| {
                Box::new(VarSizeError::AssignmentSides {
                    left: FieldSize::new_bits(ass_size),
                    right: ass.right.size(sleigh, execution),
                    location: ass.location.clone(),
                    backtrace: format!("{}:{}", file!(), line!()),
                })
            })?;
            Ok(result_left | result_right | result_output)
        }

        _ => Ok(false),
    }
}

fn hack_auto_fix_bitrange_with_bitand(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // left side need to be a bitrange op
    let AssignmentWrite::Variable {
        op: Some(AssignmentOp::BitRange(bitrange)),
        value: _,
    } = &ass.var
    else {
        return false;
    };
    let range = bitrange.end - bitrange.start;

    // right side need to be bigger then the left side
    let Some(right_size) = ass.right.size(sleigh, execution).final_value()
    else {
        return false;
    };
    if right_size.get() <= range {
        return false;
    }

    // add the bitrange around the bitand
    ass.swap_right(|swap_right| {
        Expr::Value(ExprElement::new_op(
            swap_right.src().clone(),
            Unary::BitRange {
                range: 0..range,
                size: FieldSize::Value(range.try_into().unwrap()),
            },
            swap_right,
        ))
    });
    true
}

// HACK get extra info for the variable during it's creation
// eg: local tmp = *:1 value; # this var is always 1 byte
fn hack_extra_var_info_creation(
    ass: &mut Assignment,
    _sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    let AssignmentWrite::Variable {
        op: _,
        value: AssignmentWriteVariable::Local { id, creation: true },
    } = &ass.var
    else {
        return false;
    };

    let var = execution.variable(*id);
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

// HACK truncate the right side if left side is smaller
// eg mem: R0 = *:3 ptr; # R0 is 1 byte
fn hack_auto_trunkate_right_side(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // left need to have a known size
    let Some(left_size) = ass.left_size(sleigh, execution).final_value() else {
        return false;
    };

    // right need to have a known size
    let right_size = ass.right.size(sleigh, execution);
    let Some(right_size) = right_size.final_value() else {
        return false;
    };

    // only if left side is smaller then the right side
    if left_size >= right_size {
        return false;
    }

    match &mut ass.right {
        // if right side is deref and left side is byte sized, just adjust the mem read-size
        Expr::Value(ExprElement::Op(ExprUnaryOp {
            location: _,
            op: Unary::Dereference(MemoryLocation { size, .. }),
            input: _,
        })) if left_size.get() % 8 == 0 => {
            *size = FieldSize::new_bits(left_size);
        }

        // otherwise just add a implici truncation to the right
        _ => ass.swap_right(|swap_right| {
            Expr::Value(ExprElement::new_op(
                swap_right.src().clone(),
                Unary::BitRange {
                    range: 0..left_size.get(),
                    size: FieldSize::Value(left_size),
                },
                swap_right,
            ))
        }),
    }
    true
}

// HACK: sometimes when assigning a value smaller then the size of the left side,
// eg one bit registers are declare as one
// byte instead of bitranges (eg flags on X86 like ZF), and assigned
// binary values to it, when that happen, it's unclear if zext or a left
// hand truncation happen, I'll stick with left hand operation for now
fn hack_auto_zext_right_side(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // left hand need to have a known size
    let left_size = match &mut ass.var {
        // can be a varnode
        AssignmentWrite::Variable {
            value: AssignmentWriteVariable::Varnode(id),
            op: None,
        } => {
            let var = sleigh.varnode(*id);
            let len = var.len_bytes.get() * 8;
            len.try_into().unwrap()
        }
        // local variable
        AssignmentWrite::Variable {
            value: AssignmentWriteVariable::Local { id, creation: _ },
            op: None,
        } => {
            let var = execution.variable(*id);
            // TODO maybe allow non explicit declared variables
            if !var.explicit {
                return false;
            }
            let Some(bits) = var.size.get().final_value() else {
                return false;
            };
            bits
        }
        // or mem write
        AssignmentWrite::Memory { mem, addr: _ } => {
            let Some(size) = mem.size.final_value() else {
                return false;
            };
            size
        }
        _ => return false,
    };

    // left side need to be smaller then the right side
    let Some(right_size) = ass.right.size(sleigh, execution).max_bits() else {
        return false;
    };
    if left_size <= right_size {
        return false;
    }

    // add a zext to the right side
    ass.swap_right(|swap_right| {
        Expr::new_value(ExprElement::new_op(
            swap_right.src().clone(),
            Unary::Zext(FieldSize::new_bits(left_size)),
            swap_right,
        ))
    });
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
    ass.swap_right(|swap_right| {
        Expr::Value(ExprElement::new_op(
            swap_right.src().clone(),
            Unary::BitRange {
                range: 0..1,
                size: FieldSize::Value(1.try_into().unwrap()),
            },
            swap_right,
        ))
    });

    true
}

// HACK force solving simple usercall
// eg YmmReg1 = vpblendd_avx2( vexVVVV_YmmReg, YmmReg2_m256, imm8:1 ); # vpblendd_avx2 is pcodeop
fn hack_force_solve_usercall(
    ass: &mut Assignment,
    sleigh: &Sleigh,
    execution: &Execution,
) -> Result<bool, Box<VarSizeError>> {
    // if right is a simple user_call
    let Expr::Value(ExprElement::UserCall(usercall)) = &ass.right else {
        return Ok(false);
    };
    let location = usercall.location.clone();
    // user the same size on both sides
    let (mut left_size, mut right_size) =
        ass.left_and_right_size_mut(sleigh, execution);
    len::a_equivalent_b(&mut *left_size, &mut *right_size).ok_or_else(|| {
        Box::new(VarSizeError::AssignmentSides {
            left: left_size.get(),
            right: right_size.get(),
            location: location.clone(),
            backtrace: format!("{}:{}", file!(), line!()),
        })
    })
}
