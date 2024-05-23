use crate::semantic::execution::{
    Binary, Expr as FinalExpr, ExprBinaryOp as FinalExprBinaryOp,
    ExprBitrange as FinalExprBitrange, ExprCPool as FinalExprCPool,
    ExprContext as FinalExprContext, ExprDisVar as FinalExprDisVar,
    ExprElement as FinalExprElement, ExprExeVar, ExprInstNext, ExprInstStart,
    ExprNew as FinalExprNew, ExprNumber as FinalExprNumber, ExprTable,
    ExprTokenField as FinalExprTokenField, ExprUnaryOp as FinalExprUnaryOp,
    ExprValue as FinalReadValue, ExprVarnode, Reference as FinalReference,
    ReferencedValue, Unary,
};
use crate::semantic::inner::execution::len;
use crate::semantic::inner::{FieldSize, Sleigh, Solved, SolverStatus};
use crate::semantic::{disassembly, InstNext, InstStart};
use crate::{
    BitrangeId, ContextId, ExecutionError, Number, NumberNonZeroUnsigned,
    NumberUnsigned, Span, TokenFieldId, VarSizeError,
};

use super::{
    Execution, ExportLen, FieldSizeMut, FieldSizeUnmutable, MemoryLocation, ReadScope, UserCall
};

macro_rules! mark_unfinished_size {
    ($size:expr, $solved:expr, $location:expr $(,)?) => {
        mark_unfinished_size_at($size, $solved, $location, file!(), line!())
    };
}

fn mark_unfinished_size_at(
    size: &FieldSize,
    solved: &mut impl SolverStatus,
    location: &Span,
    file: &'static str,
    line: u32,
) {
    if size.is_undefined() {
        solved.iam_not_finished(location, file, line)
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(ExprBinaryOp),
}

#[derive(Clone, Debug)]
pub struct ExprBinaryOp {
    pub location: Span,
    pub output_size: FieldSize,
    pub op: Binary,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ExprValue),
    UserCall(UserCall),
    Reference(Reference),
    DeReference(Span, MemoryLocation, Box<Expr>),
    Op(ExprUnaryOp),
    New(ExprNew),
    CPool(ExprCPool),
}

#[derive(Clone, Debug)]
pub enum ExprValue {
    Int(ExprNumber),
    TokenField(ExprTokenField),
    InstStart(ExprInstStart),
    InstNext(ExprInstNext),
    Varnode(ExprVarnode),
    Context(ExprContext),
    Bitrange(ExprBitrange),
    Table(ExprTable),
    DisVar(ExprDisVar),
    ExeVar(ExprExeVar),
}

#[derive(Clone, Debug)]
pub struct ExprNumber {
    pub location: Span,
    pub size: FieldSize,
    pub number: Number,
}

#[derive(Clone, Debug)]
pub struct ExprTokenField {
    pub location: Span,
    pub size: FieldSize,
    pub id: TokenFieldId,
}

#[derive(Clone, Debug)]
pub struct ExprContext {
    pub location: Span,
    pub size: FieldSize,
    pub id: ContextId,
}

#[derive(Clone, Debug)]
pub struct ExprBitrange {
    pub location: Span,
    pub size: FieldSize,
    pub id: BitrangeId,
}

#[derive(Clone, Debug)]
pub struct ExprDisVar {
    pub location: Span,
    pub size: FieldSize,
    pub id: disassembly::VariableId,
}

#[derive(Clone, Debug)]
pub struct Reference {
    pub location: Span,
    pub len: FieldSize,
    pub value: ReferencedValue,
}

#[derive(Clone, Debug)]
pub struct ExprUnaryOp {
    pub location: Span,
    pub output_size: FieldSize,
    pub op: Unary,
    pub input: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprNew {
    pub location: Span,
    pub first: Box<Expr>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct ExprCPool {
    pub location: Span,
    pub params: Vec<Expr>,
}

impl Expr {
    pub fn new_value(value: ExprElement) -> Self {
        Self::Value(value)
    }
    pub fn new_op(
        sleigh: &Sleigh,
        execution: &Execution,
        location: Span,
        op: Binary,
        left: Expr,
        mut right: Expr,
    ) -> Self {
        use Binary::*;
        let output_size = match op {
            Lsl | Lsr | Asr => {
                // rotation is unlikelly to rotate more then 128 bits,
                // so right element size only require 32bits max
                // HACK we are enforcing a 32 bits field requirement
                let _ = right
                    .size_mut(sleigh, execution)
                    .update_action(|size| {
                        size.set_final_value(32.try_into().unwrap())
                    })
                    .unwrap();
                FieldSize::new_unsized()
            }
            SigLess | SigGreater | SigRem | SigLessEq | SigGreaterEq | Less
            | Greater | LessEq | GreaterEq | FloatLess | FloatGreater
            | FloatLessEq | FloatGreaterEq | And | Xor | Or | Eq | Ne
            | FloatEq | FloatNe | Carry | SCarry | SBorrow => {
                FieldSize::new_bool()
            }
            Mult | Div | SigDiv | Rem | FloatDiv | FloatMult | Sub
            | FloatAdd | FloatSub | BitAnd | BitXor | BitOr | Add => {
                FieldSize::new_unsized()
            }
        };
        Self::Op(ExprBinaryOp {
            location,
            output_size,
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }
    pub fn src(&self) -> &Span {
        match self {
            Expr::Value(value) => value.src(),
            Expr::Op(op) => &op.location,
        }
    }
    pub fn size(&self, sleigh: &Sleigh, execution: &Execution) -> FieldSize {
        match self {
            Expr::Value(value) => value.size(sleigh, execution),
            Expr::Op(op) => op.output_size,
        }
    }
    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Expr::Value(value) => value.size_mut(sleigh, execution),
            Expr::Op(op) => Box::new(&mut op.output_size),
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        // if a value, just resolve the value and return
        if let Expr::Value(value) = self {
            return value.solve(sleigh, execution, solved);
        }

        // if not a value, take self, and replace it by a dummy value
        //TODO make this less akward and remove the unecessary deref
        //and recreation of Box's
        //akwardly move values from self, replacing with a dummy value
        let mut slf_moved = self.clone();
        std::mem::swap(self, &mut slf_moved);

        //TODO make the moves and mut ref more elegant
        let Expr::Op(op_moved) = slf_moved else {
            unreachable!();
        };
        *self = inner_expr_solve(op_moved, sleigh, execution, solved)?;
        Ok(())
    }
    pub fn convert(self) -> FinalExpr {
        match self {
            Expr::Value(value) => FinalExpr::Value(value.convert()),
            Expr::Op(op) => FinalExpr::Op(op.convert()),
        }
    }
}

impl ExprElement {
    /// varnode:size
    pub fn new_take_lsb(
        location: Span,
        bytes: NumberNonZeroUnsigned,
        expr: Expr,
    ) -> Self {
        Self::Op(ExprUnaryOp {
            location,
            output_size: FieldSize::Value(bytes),
            op: Unary::TakeLsb(bytes),
            input: Box::new(expr),
        })
    }
    /// varnode(size)
    pub fn new_trunk_lsb(
        location: Span,
        bytes: NumberUnsigned,
        expr: Expr,
    ) -> Self {
        Self::Op(ExprUnaryOp {
            location,
            output_size: FieldSize::default(),
            op: Unary::TrunkLsb(bytes),
            input: Box::new(expr),
        })
    }
    // varnode[lsb, size]
    pub fn new_bitrange(
        location: Span,
        lsb: NumberUnsigned,
        size: NumberNonZeroUnsigned,
        expr: Expr,
    ) -> Self {
        Self::Op(ExprUnaryOp {
            location,
            output_size: FieldSize::default()
                .set_min_bits(size)
                .unwrap()
                .set_possible_min(),
            op: Unary::BitRange(lsb..lsb + size.get()),
            input: Box::new(expr),
        })
    }
    pub fn new_deref(
        _sleigh: &Sleigh,
        _execution: &Execution,
        src: Span,
        deref: MemoryLocation,
        addr: Expr,
    ) -> Self {
        Self::DeReference(src, deref, Box::new(addr))
    }
    pub fn new_op(src: Span, mut op: Unary, expr: Expr) -> Self {
        let size = match &mut op {
            //the output can be one bit (true/false)
            Unary::FloatNan => FieldSize::new_bool(),
            _ => FieldSize::new_unsized(),
        };
        Self::Op(ExprUnaryOp {
            location: src,
            output_size: size,
            op,
            input: Box::new(expr),
        })
    }
    pub fn src(&self) -> &Span {
        match self {
            Self::Value(value) => value.src(),
            Self::UserCall(call) => &call.location,
            Self::Op(op) => &op.location,
            Self::Reference(reference) => &reference.location,
            Self::New(new) => &new.location,
            Self::CPool(cpool) => &cpool.location,
            Self::DeReference(src, _, _) => src,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        let mut modified = false;
        match self {
            Self::Value(value) => value.solve(sleigh, execution, solved)?,
            Self::Reference(Reference {
                location: _,
                len: _,
                value: ReferencedValue::Table(table),
            }) => {
                //if the table reference is return space references
                //(like varnode) update the output size with the addr size
                let table = sleigh.table(table.id);
                match table.export.borrow().as_ref().unwrap_or_else(|| todo!())
                {
                    ExportLen::Reference(_) => (/*TODO*/),
                    ExportLen::None
                    | ExportLen::Const(_)
                    | ExportLen::Value(_)
                    | ExportLen::Multiple(_) => (/*TODO*/),
                }
            }
            Self::Reference(Reference {
                location: _,
                len: _,
                value: ReferencedValue::TokenField(_table),
            }) => (/*TODO*/),
            Self::Reference(Reference {
                location: _,
                len: _,
                value:
                    ReferencedValue::InstStart(_) | ReferencedValue::InstNext(_),
            }) => (/*TODO*/),
            Self::Op(ExprUnaryOp {
                location,
                output_size,
                op: Unary::TakeLsb(bytes),
                input,
            }) => {
                // the output_size is set at creation
                debug_assert_eq!(
                    output_size.final_value().map(|x| x.get()),
                    Some(bytes.get() * 8)
                );
                // input need to be lsb bytes or bigger
                let modified_result = input
                    .size_mut(sleigh, execution)
                    .update_action(|x| x.set_min_bytes(*bytes));
                modified |= modified_result.ok_or_else(|| {
                    VarSizeError::TakeLsbTooSmall {
                        location: input.src().clone(),
                        lsb: *bytes,
                        input: input.size(sleigh, execution),
                    }
                })?;

                // if the input or output len are not possible, we are not done
                if input.size(sleigh, execution).is_undefined()
                    || output_size.is_undefined()
                {
                    solved.iam_not_finished(&location, file!(), line!());
                }

                input.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
                output_size,
                op: Unary::TrunkLsb(bytes),
                input,
            }) => {
                // input need to be (op_bytes + 1bit) or bigger
                // +1 because the operation can't return 0 bits
                let modified_result =
                    input.size_mut(sleigh, execution).update_action(|x| {
                        x.set_min_bits((*bytes * 8 + 1).try_into().unwrap())
                    });
                modified |= modified_result.ok_or_else(|| {
                    VarSizeError::TrunkLsbTooSmall {
                        lsb: *bytes,
                        output: *output_size,
                        input: input.size(sleigh, execution),
                        location: input.src().clone(),
                    }
                })?;

                // output size need to be equal to (input_size - op_bytes)
                let input_size = input.size(sleigh, execution);
                if let Some(max_bits) = input_size.max_bits() {
                    let max_bits = max_bits.get() - *bytes * 8;
                    modified |= output_size
                        .update_action(|x| {
                            x.set_max_bits(max_bits.try_into().unwrap())
                        })
                        .ok_or_else(|| VarSizeError::TrunkLsbTooSmall {
                            lsb: *bytes,
                            output: *output_size,
                            input: input.size(sleigh, execution),
                            location: input.src().clone(),
                        })?;
                }
                if let Some(min_bits) = input_size.min_bits() {
                    let min_bits = min_bits.get() - *bytes * 8;
                    modified |= output_size
                        .update_action(|x| {
                            x.set_min_bits(min_bits.try_into().unwrap())
                        })
                        .ok_or_else(|| VarSizeError::TrunkLsbTooSmall {
                            lsb: *bytes,
                            output: *output_size,
                            input: input.size(sleigh, execution),
                            location: input.src().clone(),
                        })?;
                }

                // if the input or output len are not possible, we are not done
                if !input.size(sleigh, execution).is_possible()
                    || !output_size.is_possible()
                {
                    solved.iam_not_finished(&location, file!(), line!());
                }

                input.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
                output_size,
                op: Unary::BitRange(bitrange),
                input,
            }) => {
                // output size need to be equal or bigger then the binary range
                let bits = (bitrange.end - bitrange.start).try_into().unwrap();
                modified |= output_size
                    .update_action(|x| x.set_min_bits(bits))
                    .ok_or_else(|| VarSizeError::BitRangeTooBig {
                        location: input.src().clone(),
                        output: *output_size,
                        bits,
                    })?;

                // input len need to be equal to bitrange.end or bigger
                let modified_result =
                    input.size_mut(sleigh, execution).update_action(|x| {
                        x.set_min_bits(bitrange.end.try_into().unwrap())
                    });
                modified |= modified_result.ok_or_else(|| {
                    VarSizeError::BitRangeInputSmall {
                        location: input.src().clone(),
                        input: input.size(sleigh, execution),
                        bits,
                    }
                })?;

                // if the input or output len are not possible, we are not done
                if !input.size(sleigh, execution).is_possible()
                    || !output_size.is_possible()
                {
                    solved.iam_not_finished(&location, file!(), line!());
                }

                input.solve(sleigh, execution, solved)?;
            }
            Self::DeReference(location, deref, addr) => {
                let space = sleigh.space(deref.space);
                //addr expr, need to be the space_addr size or greater
                let modified = addr
                    .size_mut(sleigh, execution)
                    .update_action(|size| size.set_min_bytes(space.addr_bytes));
                if modified.ok_or_else(|| VarSizeError::AddressTooBig {
                    address_size: addr.size(sleigh, execution),
                    space_bytes: space.addr_bytes,
                    location: location.clone(),
                })? {
                    solved.i_did_a_thing();
                }
                deref.solve(solved);
                addr.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
                output_size: size,
                op:
                    Unary::Negation
                    | Unary::BitNegation
                    | Unary::Negative
                    | Unary::FloatNegative
                    | Unary::FloatAbs
                    | Unary::FloatSqrt
                    | Unary::FloatCeil
                    | Unary::FloatFloor
                    | Unary::FloatRound,
                input,
            }) => {
                //the input and output have the same number of bits
                let modified_result = len::a_generate_b(
                    &mut *input.size_mut(sleigh, execution),
                    size,
                );
                modified |= modified_result.ok_or_else(|| {
                    VarSizeError::UnaryOpDiffSize {
                        input: input.size(sleigh, execution),
                        output: *size,
                        location: location.clone(),
                    }
                })?;
                mark_unfinished_size!(&size, solved, location);
                input.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
                output_size,
                op: Unary::Popcount | Unary::Lzcount,
                input,
            }) => {
                //the output min size is: log2(bit_len(input) + 1)
                if let Some(input_num_bits) =
                    input.size(sleigh, execution).final_value()
                {
                    //equivalent to log2(bit_len(input) + 1)
                    let output_min = NumberUnsigned::BITS
                        - input_num_bits.get().leading_zeros();
                    if let Some(output_min) =
                        NumberNonZeroUnsigned::new(output_min.into())
                    {
                        if output_size
                            .update_action(|size| {
                                size.set_possible_min().set_min_bits(output_min)
                            })
                            .ok_or_else(|| {
                                VarSizeError::BitCountInvalidOutput {
                                    location: location.clone(),
                                    output: *output_size,
                                    bits: output_min,
                                }
                            })?
                        {
                            solved.i_did_a_thing();
                        }
                    }
                }
                mark_unfinished_size!(output_size, solved, location);
                input.solve(sleigh, execution, solved)?;
            }

            Self::Op(ExprUnaryOp {
                location,
                mut output_size,
                op: Unary::Zext | Unary::Sext,
                input,
            }) => {
                //output size need to be bigger or eq to the value size and vise-versa
                let modified_result = len::a_extend_b(
                    &mut *input.size_mut(sleigh, execution),
                    &mut output_size,
                );
                modified |=
                    modified_result.ok_or_else(|| VarSizeError::ExtShrink {
                        input: input.size(sleigh, execution),
                        output: output_size,
                        location: location.clone(),
                    })?;

                mark_unfinished_size!(&output_size, solved, location);
                input.solve(sleigh, execution, solved)?;
            }
            // both can have any size
            // NOTE don't confuse signed truncation with regular truncation
            // sleigh `trunc` function converts float into interger, both
            // can have any size.
            Self::Op(ExprUnaryOp {
                location,
                output_size,
                op:
                    Unary::SignTrunc
                    | Unary::Float2Float
                    | Unary::Int2Float
                    | Unary::FloatNan,
                input,
            }) => {
                mark_unfinished_size!(output_size, solved, location);
                input.solve(sleigh, execution, solved)?;
            }
            Self::UserCall(UserCall {
                output_size,
                params,
                location,
                ..
            }) => {
                mark_unfinished_size!(output_size, solved, location,);
                params.iter_mut().try_for_each(|param| {
                    param.solve(sleigh, execution, solved)
                })?;
            }
            Self::New(_) => todo!(),
            Self::CPool(_) => todo!(),
            ExprElement::Op(ExprUnaryOp {
                op: Unary::Dereference(_),
                ..
            }) => unreachable!(),
        }
        if modified {
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(self) -> FinalExprElement {
        match self {
            Self::Value(value) => FinalExprElement::Value(value.convert()),
            Self::Reference(reference) => {
                FinalExprElement::Reference(reference.convert())
            }
            Self::DeReference(src, deref, value) => {
                let size = deref.size.final_value().unwrap();
                FinalExprElement::Op(FinalExprUnaryOp {
                    location: src,
                    output_bits: size,
                    op: Unary::Dereference(deref.convert()),
                    input: Box::new(value.convert()),
                })
            }
            Self::Op(op) => FinalExprElement::Op(op.convert()),
            Self::UserCall(call) => FinalExprElement::UserCall(call.convert()),
            Self::New(new) => FinalExprElement::New(new.convert()),
            Self::CPool(cpool) => FinalExprElement::CPool(cpool.convert()),
        }
    }
    //return the size of the value with unary ops apply to it
    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Value(value) => value.size_mut(sleigh, execution),
            Self::DeReference(_, deref, _) => Box::new(&mut deref.size),
            Self::Reference(x) => Box::new(&mut x.len),
            Self::Op(x) => Box::new(&mut x.output_size),
            Self::UserCall(x) => Box::new(&mut x.output_size),
            Self::New(_x) => todo!(),
            Self::CPool(_x) => todo!(),
        }
    }
    pub fn size(&self, sleigh: &Sleigh, execution: &Execution) -> FieldSize {
        match self {
            Self::Value(value) => value.size(sleigh, execution),
            Self::DeReference(_, deref, _) => deref.size,
            Self::Reference(x) => x.len,
            Self::Op(x) => x.output_size,
            Self::UserCall(x) => x.output_size,
            Self::New(_) => todo!(),
            Self::CPool(_) => todo!(),
        }
    }
}

impl ExprValue {
    pub fn from_read_scope(
        sleigh: &Sleigh,
        location: Span,
        value: ReadScope,
    ) -> Self {
        match value {
            ReadScope::TokenField(id) => {
                let size = sleigh.token_field(id).exec_value_len(sleigh);
                Self::TokenField(ExprTokenField { location, size, id })
            }
            ReadScope::InstStart => Self::InstStart(ExprInstStart {
                location,
                data: InstStart,
            }),
            ReadScope::InstNext => Self::InstNext(ExprInstNext {
                location,
                data: InstNext,
            }),
            ReadScope::Varnode(id) => {
                Self::Varnode(ExprVarnode { location, id })
            }
            ReadScope::Context(id) => {
                let size = sleigh.context(id).exec_value_size(sleigh);
                Self::Context(ExprContext { location, size, id })
            }
            ReadScope::Bitrange(id) => {
                let bitrange = sleigh.bitrange(id);
                let size = FieldSize::new_unsized()
                    .set_min_bits(bitrange.bits.len())
                    .unwrap()
                    .set_possible_min();
                Self::Bitrange(ExprBitrange { location, size, id })
            }
            ReadScope::Table(id) => Self::Table(ExprTable { location, id }),
            ReadScope::DisVar(id) => {
                let size = FieldSize::new_unsized();
                Self::DisVar(ExprDisVar { location, size, id })
            }
            ReadScope::ExeVar(id) => Self::ExeVar(ExprExeVar { location, id }),
        }
    }

    pub fn src(&self) -> &Span {
        match self {
            Self::Int(int) => &int.location,
            Self::DisVar(x) => &x.location,
            Self::ExeVar(x) => &x.location,
            Self::TokenField(x) => &x.location,
            Self::Varnode(x) => &x.location,
            Self::Context(x) => &x.location,
            Self::Bitrange(x) => &x.location,
            Self::Table(x) => &x.location,
            Self::InstStart(x) => &x.location,
            Self::InstNext(x) => &x.location,
        }
    }
    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Int(x) => Box::new(&mut x.size),
            Self::DisVar(x) => Box::new(&mut x.size),
            // TODO exec_value_len stuff here
            Self::TokenField(x) => Box::new(&mut x.size),
            Self::Context(x) => Box::new(&mut x.size),
            Self::Bitrange(x) => Box::new(&mut x.size),
            Self::InstStart(_) | Self::InstNext(_) => {
                Box::new(FieldSizeUnmutable::from(self.size(sleigh, execution)))
            }
            Self::Varnode(var) => Box::new(FieldSizeUnmutable::from(
                FieldSize::new_bytes(sleigh.varnode(var.id).len_bytes),
            )),
            Self::Table(x) => Box::new(sleigh.table(x.id)),
            Self::ExeVar(x) => Box::new(&execution.variable(x.id).size),
        }
    }
    pub fn size(&self, sleigh: &Sleigh, execution: &Execution) -> FieldSize {
        match self {
            Self::Int(x) => x.size,
            Self::DisVar(x) => x.size,
            Self::Context(x) => x.size,
            Self::Bitrange(x) => {
                FieldSize::new_bits(sleigh.bitrange(x.id).bits.len())
            }
            Self::TokenField(x) => x.size,
            Self::InstStart(_) | Self::InstNext(_) => sleigh
                .addr_bytes()
                .map(FieldSize::new_bytes)
                .unwrap_or_default(),
            Self::Varnode(x) => {
                FieldSize::new_bytes(sleigh.varnode(x.id).len_bytes)
            }
            Self::Table(x) => *sleigh
                .table(x.id)
                .export
                .borrow()
                .as_ref()
                .unwrap()
                .size()
                .unwrap(),
            Self::ExeVar(x) => execution.variable(x.id).size.get(),
        }
    }
    pub fn solve(
        &mut self,
        _sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        match self {
            //don't call table solve directly, let the main loop do it
            Self::Table(_) => {}
            Self::Context(_) | Self::Bitrange(_) | Self::TokenField(_) => {}
            // the len don't need solving
            Self::Varnode(_) | Self::InstStart(_) | Self::InstNext(_) => {}
            Self::ExeVar(var_expr) => {
                let var = &execution.variable(var_expr.id);
                mark_unfinished_size!(
                    &var.size.get(),
                    solved,
                    &var_expr.location
                )
            }
            Self::Int(num) => {
                mark_unfinished_size!(&num.size, solved, &num.location)
            }
            Self::DisVar(var) => {
                mark_unfinished_size!(&var.size, solved, &var.location)
            }
        }
        Ok(())
    }
    pub fn convert(self) -> FinalReadValue {
        match self {
            Self::Int(x) => FinalReadValue::Int(x.convert()),
            Self::TokenField(x) => FinalReadValue::TokenField(x.convert()),
            Self::InstStart(x) => FinalReadValue::InstStart(x),
            Self::InstNext(x) => FinalReadValue::InstNext(x),
            Self::Varnode(x) => FinalReadValue::Varnode(x),
            Self::Context(x) => FinalReadValue::Context(x.convert()),
            Self::Bitrange(x) => FinalReadValue::Bitrange(x.convert()),
            Self::Table(x) => FinalReadValue::Table(x),
            Self::DisVar(x) => FinalReadValue::DisVar(x.convert()),
            Self::ExeVar(x) => FinalReadValue::ExeVar(x),
        }
    }
}

impl Reference {
    pub fn convert(self) -> FinalReference {
        FinalReference {
            location: self.location,
            len_bits: self.len.possible_value().unwrap(),
            value: self.value,
        }
    }
}

impl ExprUnaryOp {
    pub fn convert(self) -> FinalExprUnaryOp {
        FinalExprUnaryOp {
            location: self.location,
            output_bits: self.output_size.possible_value().unwrap(),
            op: self.op,
            input: Box::new(self.input.convert()),
        }
    }
}

impl ExprNew {
    pub fn convert(self) -> FinalExprNew {
        FinalExprNew {
            location: self.location,
            first: Box::new(self.first.convert()),
            second: self.second.map(|expr| expr.convert()).map(Box::new),
        }
    }
}

impl ExprCPool {
    pub fn convert(self) -> FinalExprCPool {
        FinalExprCPool {
            location: self.location,
            params: self.params.into_iter().map(Expr::convert).collect(),
        }
    }
}
impl ExprBinaryOp {
    pub fn convert(self) -> FinalExprBinaryOp {
        FinalExprBinaryOp {
            location: self.location,
            len_bits: self.output_size.possible_value().unwrap(),
            op: self.op,
            left: Box::new(self.left.convert()),
            right: Box::new(self.right.convert()),
        }
    }
}
impl ExprNumber {
    pub fn convert(self) -> FinalExprNumber {
        FinalExprNumber {
            location: self.location,
            size: self.size.possible_value().unwrap(),
            number: self.number,
        }
    }

    pub fn new(location: Span, number: Number) -> ExprNumber {
        let size = FieldSize::default()
            .set_min_bits(u64::from(number.bits_required()).try_into().unwrap())
            .unwrap()
            .set_possible_min();
        Self {
            location,
            number,
            size,
        }
    }
}

impl ExprTokenField {
    pub fn convert(self) -> FinalExprTokenField {
        FinalExprTokenField {
            location: self.location,
            size: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }

    pub fn new(
        location: Span,
        sleigh: &Sleigh,
        id: TokenFieldId,
    ) -> ExprTokenField {
        let token_field = sleigh.token_field(id);
        let min_bits = token_field.bits.len();
        let size = FieldSize::default().set_min_bits(min_bits).unwrap();
        Self { location, id, size }
    }
}

impl ExprContext {
    pub fn convert(self) -> FinalExprContext {
        FinalExprContext {
            location: self.location,
            size: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }

    pub fn new(location: Span, sleigh: &Sleigh, id: ContextId) -> ExprContext {
        let context = sleigh.context(id);
        let min_bits = context.bitrange.bits.len();
        let size = FieldSize::default().set_min_bits(min_bits).unwrap();
        Self { location, id, size }
    }
}

impl ExprBitrange {
    pub fn convert(self) -> FinalExprBitrange {
        FinalExprBitrange {
            location: self.location,
            size: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }

    pub fn new(
        location: Span,
        sleigh: &Sleigh,
        id: BitrangeId,
    ) -> ExprBitrange {
        let bitrange = sleigh.bitrange(id);
        let min_bits = bitrange.bits.len();
        let size = FieldSize::default()
            .set_min_bits(min_bits)
            .unwrap()
            .set_possible_min();
        Self { location, id, size }
    }
}

impl ExprDisVar {
    pub fn convert(self) -> FinalExprDisVar {
        FinalExprDisVar {
            location: self.location,
            size: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }
}

fn inner_expr_solve(
    mut op: ExprBinaryOp,
    sleigh: &Sleigh,
    variables: &Execution,
    solved: &mut impl SolverStatus,
) -> Result<Expr, Box<ExecutionError>> {
    use Binary::*;
    use ExprElement as Ele;
    use ExprValue as Value;

    //TODO make the moves and mut ref more elegant
    match (*op.left, op.op, *op.right) {
        //if two Integer, calculate it and replace self with the result.
        (
            Expr::Value(Ele::Value(Value::Int(left))),
            op_binary,
            Expr::Value(Ele::Value(Value::Int(right))),
        ) => {
            solved.i_did_a_thing();
            solved.iam_not_finished(&op.location, file!(), line!());
            //TODO create an error if the value is too big, for now let the
            //unwrap so we can detect if ghidra uses value > u64
            let value = op_binary
                .execute(
                    left.number.as_unsigned().unwrap(),
                    right.number.as_unsigned().unwrap(),
                )
                //TODO better error
                .ok_or(ExecutionError::InvalidExport)?;
            //replace self with our new value
            Ok(Expr::Value(Ele::Value(Value::Int(ExprNumber::new(
                op.location,
                value.into(),
            )))))
        }

        // HACK: convert some kinds of bit_and into bitrange if the value is a
        // an bitrange starting from 0 (eg: 0b1 0b11 0b111 0b1111 0b11111 etc)
        // and the output size is expected to reduce the size of the input
        // using the bitwise op.
        // eg: `reg0[0,1] = value & 1; reg1[0,2] = value & 3`
        (
            value,
            BitAnd,
            Expr::Value(Ele::Value(Value::Int(ExprNumber {
                location,
                number: integer,
                size: _,
            }))),
        )
        | (
            Expr::Value(Ele::Value(Value::Int(ExprNumber {
                location,
                number: integer,
                size: _,
            }))),
            BitAnd,
            value,
        ) if op
            .output_size
            .final_value()
            .map(|bits| {
                bits.get() == integer.as_unsigned().unwrap().count_ones().into()
            })
            .unwrap_or(false)
            && value
                .size(sleigh, variables)
                .final_value()
                .map(|bits| {
                    bits.get()
                        >= integer.as_unsigned().unwrap().count_ones().into()
                })
                .unwrap_or(true) =>
        {
            solved.i_did_a_thing();
            solved.iam_not_finished(&op.location, file!(), line!());
            let size = op.output_size.final_value().unwrap();
            Ok(Expr::Value(ExprElement::new_bitrange(
                location, 0, size, value,
            )))
        }

        // TODO create an error if the value is too big, and as_unsigned
        // fails for now let the unwrap so we can detect if ghidra uses
        // value > u64

        // HACK: convert if the output bit size, if the left hand is a value
        // with an defined bit size and the right side an integer, the output
        // can be a bitrange truncate from the left.
        // eg `value` is 8bits: `tmp = value >> 7; => tmp = value[7,1]`
        (
            value,
            Lsr,
            Expr::Value(Ele::Value(Value::Int(ExprNumber {
                location: src,
                number: lsb,
                size: _,
            }))),
        ) if op
            .output_size
            .final_value()
            .zip(value.size(sleigh, variables).final_value())
            .map(|(out_bits, val_bits)| (out_bits.get(), val_bits.get()))
            .map(|(out_bits, val_bits)| {
                val_bits >= lsb.as_unsigned().unwrap()
                    && out_bits == (val_bits - lsb.as_unsigned().unwrap())
            })
            .unwrap_or(false) =>
        {
            solved.i_did_a_thing();
            solved.iam_not_finished(&src, file!(), line!());
            let size = op.output_size.final_value().unwrap();
            //take the value from self, and put on the new self
            //safe because the self is overwriten after
            Ok(Expr::Value(Ele::new_bitrange(
                src.clone(),
                lsb.as_unsigned().unwrap(),
                size,
                value,
            )))
        }

        //output and left have the same size, right can have any size
        (mut left, Lsl | Lsr | Asr, mut right) => {
            left.solve(sleigh, variables, solved)?;
            //NOTE right defaults to 32bits
            right.solve(sleigh, variables, solved)?;

            let restricted = len::a_generate_b(
                left.size_mut(sleigh, variables).as_dyn(),
                &mut op.output_size,
            );
            let restricted = restricted.ok_or_else(|| {
                VarSizeError::ShiftLeftOutputDiff {
                    left: left.size(sleigh, variables),
                    output: op.output_size,
                    location: left.src().clone(),
                }
            })?;
            if restricted {
                solved.i_did_a_thing();
            }
            mark_unfinished_size!(&op.output_size, solved, &op.location,);
            Ok(Expr::Op(ExprBinaryOp {
                location: op.location,
                output_size: op.output_size,
                op: op.op,
                left: Box::new(left),
                right: Box::new(right),
            }))
        }

        //left/right/output can have any size, they are all just `0` or `!=0`
        (mut left, And | Xor | Or, mut right) => {
            //left/right can have any lenght, so just try to solve the len
            //if possible, otherwise just ignore it
            left.solve(sleigh, variables, &mut Solved::default())?;
            right.solve(sleigh, variables, &mut Solved::default())?;
            mark_unfinished_size!(&op.output_size, solved, &op.location,);
            Ok(Expr::Op(ExprBinaryOp {
                location: op.location,
                output_size: op.output_size,
                op: op.op,
                left: Box::new(left),
                right: Box::new(right),
            }))
        }

        // All sides need to have the same number of bits.
        (
            mut left,
            Mult | FloatMult | Div | SigDiv | FloatDiv | Rem | SigRem | Add
            | FloatAdd | Sub | FloatSub | BitAnd | BitXor | BitOr,
            mut right,
        ) => {
            left.solve(sleigh, variables, solved)?;
            right.solve(sleigh, variables, solved)?;
            let restricted = len::a_b_generate_c(
                left.size_mut(sleigh, variables).as_dyn(),
                right.size_mut(sleigh, variables).as_dyn(),
                &mut op.output_size,
            );
            let restricted =
                restricted.ok_or_else(|| VarSizeError::TriBinaryOp {
                    left: left.size(sleigh, variables),
                    right: right.size(sleigh, variables),
                    output: op.output_size,
                    location: op.location.clone(),
                })?;
            if restricted {
                solved.i_did_a_thing();
            }

            mark_unfinished_size!(&op.output_size, solved, &op.location);
            Ok(Expr::Op(ExprBinaryOp {
                location: op.location,
                output_size: op.output_size,
                op: op.op,
                left: Box::new(left),
                right: Box::new(right),
            }))
        }

        //both need to have the same number of bits, the output is value 0/1
        (
            mut left,
            Less | SigLess | FloatLess | LessEq | SigLessEq | FloatLessEq
            | Greater | SigGreater | FloatGreater | GreaterEq | SigGreaterEq
            | FloatGreaterEq | Eq | FloatEq | Ne | FloatNe | Carry | SCarry
            | SBorrow,
            mut right,
        ) => {
            left.solve(sleigh, variables, solved)?;
            right.solve(sleigh, variables, solved)?;
            //Both sides need to have the same number of bits.
            //output can have any size because is always 0/1
            let restricted = len::a_cmp_b(
                left.size_mut(sleigh, variables).as_dyn(),
                right.size_mut(sleigh, variables).as_dyn(),
            );
            let restricted =
                restricted.ok_or_else(|| VarSizeError::BoolBinaryOp {
                    left: left.size(sleigh, variables),
                    right: right.size(sleigh, variables),
                    location: op.location.clone(),
                })?;
            if restricted {
                solved.i_did_a_thing();
            }
            mark_unfinished_size!(&op.output_size, solved, &op.location);
            Ok(Expr::Op(ExprBinaryOp {
                location: op.location,
                output_size: op.output_size,
                op: op.op,
                left: Box::new(left),
                right: Box::new(right),
            }))
        }
    }
}
