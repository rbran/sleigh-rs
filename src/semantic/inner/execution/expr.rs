use std::ops::Range;

use crate::execution::{DynamicValueType, ExprVarnodeDynamic, VariableId};
use crate::semantic::execution::{
    Binary, Expr as FinalExpr, ExprBinaryOp as FinalExprBinaryOp,
    ExprBitrange as FinalExprBitrange, ExprCPool as FinalExprCPool,
    ExprContext as FinalExprContext, ExprDisVar as FinalExprDisVar,
    ExprDynamicInt as FinalExprDynamicInt, ExprElement as FinalExprElement,
    ExprNew as FinalExprNew, ExprNumber as FinalExprNumber,
    ExprTokenField as FinalExprTokenField, ExprUnaryOp as FinalExprUnaryOp,
    ExprValue as FinalReadValue, Reference as FinalReference, ReferencedValue,
    Unary as FinalUnary,
};
use crate::semantic::inner::execution::len;
use crate::semantic::inner::{FieldSize, Sleigh, SolverStatus};
use crate::semantic::{disassembly, InstNext, InstStart};
use crate::{
    AttachNumberId, BitrangeId, ContextId, ExecutionError, Number,
    NumberNonZeroUnsigned, NumberUnsigned, Span, TableId, TokenFieldId,
    VarSizeError, VarnodeId,
};

use super::{
    Execution, FieldSizeMut, FieldSizeUnmutable, MemoryLocation, ReadScope,
    TableExportType, UserCall,
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
    Value { location: Span, value: ExprValue },
    UserCall(UserCall),
    Reference(Reference),
    Op(ExprUnaryOp),
    New(ExprNew),
    CPool(ExprCPool),
}

#[derive(Clone, Debug)]
pub enum ExprValue {
    /// Simple Int value
    Int(ExprNumber),
    /// Context/TokenField value translated into a Int
    /// Dynamic Int from Context or TokenField
    IntDynamic(ExprIntDynamic),
    InstStart(InstStart),
    InstNext(InstNext),
    /// simple TokenField, no attachment
    TokenField(ExprTokenField),
    /// simple Context, no attachment
    Context(ExprContext),
    /// A Varnode Value
    Varnode(VarnodeId),
    /// A Context/TokenField translated into a varnode
    VarnodeDynamic(ExprVarnodeDynamic),
    Bitrange(ExprBitrange),
    Table(TableId),
    DisVar(ExprDisVar),
    ExeVar(VariableId),
}

#[derive(Clone, Debug)]
pub struct ExprNumber {
    pub size: FieldSize,
    pub number: Number,
}

#[derive(Clone, Debug)]
pub struct ExprIntDynamic {
    pub size: FieldSize,
    pub attach_id: AttachNumberId,
    pub attach_value: DynamicValueType,
}

#[derive(Clone, Debug)]
pub struct ExprTokenField {
    pub size: FieldSize,
    pub id: TokenFieldId,
}

#[derive(Clone, Debug)]
pub struct ExprContext {
    pub size: FieldSize,
    pub id: ContextId,
}

#[derive(Clone, Debug)]
pub struct ExprBitrange {
    pub size: FieldSize,
    pub id: BitrangeId,
}

#[derive(Clone, Debug)]
pub struct ExprDisVar {
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

#[derive(Clone, Debug)]
pub enum Unary {
    TakeLsb(NumberNonZeroUnsigned),
    TrunkLsb {
        trunk: NumberUnsigned,
        size: FieldSize,
    },
    // BitRange have an auto Sext to it
    BitRange {
        range: Range<NumberUnsigned>,
        size: FieldSize,
    },
    Dereference(MemoryLocation),
    //Reference(AddrReference),
    /// output size is deferent from the input size
    Zext(FieldSize),
    Sext(FieldSize),
    Popcount(FieldSize),
    Lzcount(FieldSize),
    FloatNan(FieldSize),
    // NOTE don't confuse signed truncation with regular truncation
    // sleigh `trunc` function converts float into interger
    SignTrunc(FieldSize),
    Float2Float(FieldSize),
    Int2Float(FieldSize),

    /// output size is just the input size
    Negation,
    BitNegation,
    Negative,
    FloatNegative,
    FloatAbs,
    FloatSqrt,
    FloatCeil,
    FloatFloor,
    FloatRound,
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
                // set the size only if it's not set already
                if right.size(sleigh, execution).is_unrestricted() {
                    let _ = right
                        .size_mut(sleigh, execution)
                        .update_action(|size| {
                            size.set_possible_bits(32.try_into().unwrap())
                        })
                        .unwrap();
                }
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
            op: Unary::TrunkLsb {
                trunk: bytes,
                size: FieldSize::new_unsized(),
            },
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
            op: Unary::BitRange {
                range: lsb..lsb + size.get(),
                size: FieldSize::default()
                    .set_min_bits(size)
                    .unwrap()
                    .set_possible_min(),
            },
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
        Self::new_op(src, Unary::Dereference(deref), addr)
    }
    pub fn new_op(src: Span, op: Unary, expr: Expr) -> Self {
        Self::Op(ExprUnaryOp {
            location: src,
            op,
            input: Box::new(expr),
        })
    }
    pub fn src(&self) -> &Span {
        match self {
            Self::Value { location, value: _ } => location,
            Self::UserCall(call) => &call.location,
            Self::Op(op) => &op.location,
            Self::Reference(reference) => &reference.location,
            Self::New(new) => &new.location,
            Self::CPool(cpool) => &cpool.location,
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
            Self::Value { location, value } => {
                value.solve(location, sleigh, execution, solved)?
            }
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
                    TableExportType::Reference { .. } => (/*TODO*/),
                    TableExportType::None
                    | TableExportType::Const(_)
                    | TableExportType::Value(_) => (/*TODO*/),
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
                op: Unary::TakeLsb(bytes),
                input,
            }) => {
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

                // if the input is not possible, we are not done
                if input.size(sleigh, execution).is_undefined() {
                    solved.iam_not_finished(location, file!(), line!());
                }

                input.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
                op: Unary::TrunkLsb { trunk, size },
                input,
            }) => {
                // input need to be (op_bytes + 1bit) or bigger
                // +1 because the operation can't return 0 bits
                let modified_result =
                    input.size_mut(sleigh, execution).update_action(|x| {
                        x.set_min_bits((*trunk * 8 + 1).try_into().unwrap())
                    });
                modified |= modified_result.ok_or_else(|| {
                    VarSizeError::TrunkLsbTooSmall {
                        lsb: *trunk,
                        output: *size,
                        input: input.size(sleigh, execution),
                        location: input.src().clone(),
                    }
                })?;

                let input_size = input.size(sleigh, execution);
                modified |= size
                    .update_action(|size| {
                        // output size can't take less then 1 byte
                        size.set_min_bits(8.try_into().unwrap()).and_then(
                            |size| {
                                if let Some(max_bits) = input_size.max_bits() {
                                    let max_bits = max_bits.get() - *trunk * 8;
                                    // output size need to be at most `input_size - op_bytes`
                                    size.set_max_bits(
                                        max_bits.try_into().unwrap(),
                                    )
                                } else {
                                    Some(size)
                                }
                            },
                        )
                    })
                    .ok_or_else(|| VarSizeError::TrunkLsbTooSmall {
                        lsb: *trunk,
                        output: *size,
                        input: input.size(sleigh, execution),
                        location: input.src().clone(),
                    })?;

                // if the input or output len are not possible, we are not done
                if !input.size(sleigh, execution).is_possible()
                    || !size.is_possible()
                {
                    solved.iam_not_finished(location, file!(), line!());
                }

                input.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
                op: Unary::BitRange { range, size },
                input,
            }) => {
                // output size need to be equal or bigger then the binary range
                let bits = (range.end - range.start).try_into().unwrap();
                modified |= size
                    .update_action(|x| x.set_min_bits(bits))
                    .ok_or_else(|| VarSizeError::BitRangeTooBig {
                        location: input.src().clone(),
                        output: *size,
                        bits,
                    })?;

                // input len need to be equal to bitrange.end or bigger
                let modified_result =
                    input.size_mut(sleigh, execution).update_action(|x| {
                        x.set_min_bits(range.end.try_into().unwrap())
                    });
                modified |= modified_result.ok_or_else(|| {
                    VarSizeError::BitRangeInputSmall {
                        location: input.src().clone(),
                        input: input.size(sleigh, execution),
                        bits,
                    }
                })?;

                // if the input or output len are not possible, we are not done
                if !(input.size(sleigh, execution).is_possible()
                    && size.is_possible())
                {
                    solved.iam_not_finished(location, file!(), line!());
                }

                input.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
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
                input.solve(sleigh, execution, solved)?;
                mark_unfinished_size!(
                    &input.size(sleigh, execution),
                    solved,
                    location
                );
            }
            Self::Op(ExprUnaryOp {
                location,
                op: Unary::Popcount(output_size) | Unary::Lzcount(output_size),
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
                op: Unary::Zext(mut output_size) | Unary::Sext(mut output_size),
                input,
            }) => {
                //output size need to be bigger or eq to the value size and vise-versa
                let modified_result = len::a_extend_b(
                    &mut *input.size_mut(sleigh, execution),
                    &mut &mut output_size,
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
                op:
                    Unary::SignTrunc(output_size)
                    | Unary::Float2Float(output_size)
                    | Unary::Int2Float(output_size)
                    | Unary::FloatNan(output_size),
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
                location,
                op: Unary::Dereference(deref),
                input,
            }) => {
                let space = sleigh.space(deref.space);
                //addr expr, need to be the space_addr size or greater
                let modified = input
                    .size_mut(sleigh, execution)
                    .update_action(|size| size.set_min_bytes(space.addr_bytes));
                if modified.ok_or_else(|| VarSizeError::AddressTooBig {
                    address_size: input.size(sleigh, execution),
                    space_bytes: space.addr_bytes,
                    location: location.clone(),
                })? {
                    solved.i_did_a_thing();
                }
                deref.solve(solved);
                input.solve(sleigh, execution, solved)?;
            }
        }
        if modified {
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(self) -> FinalExprElement {
        match self {
            Self::Value { value, location } => FinalExprElement::Value {
                location,
                value: value.convert(),
            },
            Self::Reference(reference) => {
                FinalExprElement::Reference(reference.convert())
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
            Self::Value { value, location: _ } => {
                value.size_mut(sleigh, execution)
            }
            Self::Reference(x) => Box::new(&mut x.len),
            Self::Op(x) => x.size_mut(sleigh, execution),
            Self::UserCall(x) => Box::new(&mut x.output_size),
            Self::New(_x) => todo!(),
            Self::CPool(_x) => todo!(),
        }
    }
    pub fn size(&self, sleigh: &Sleigh, execution: &Execution) -> FieldSize {
        match self {
            Self::Value { value, location: _ } => value.size(sleigh, execution),
            Self::Reference(x) => x.len,
            Self::Op(x) => x.size(sleigh, execution),
            Self::UserCall(x) => x.output_size,
            Self::New(_) => todo!(),
            Self::CPool(_) => todo!(),
        }
    }
}

impl ExprValue {
    pub fn from_read_scope(sleigh: &Sleigh, value: ReadScope) -> Self {
        match value {
            ReadScope::TokenField(tf_id) => {
                use crate::execution::DynamicValueType::*;
                use crate::token::TokenFieldAttach::*;
                let tf = sleigh.token_field(tf_id);
                match tf.attach {
                    Some(Varnode(attach_id)) => {
                        Self::VarnodeDynamic(ExprVarnodeDynamic {
                            attach_id,
                            attach_value: TokenField(tf_id),
                        })
                    }
                    Some(Number(_, attach_id)) => {
                        let attach = sleigh.attach_number(attach_id);
                        let size = FieldSize::new_unsized()
                            .set_min_bits(
                                u64::from(attach.bits_required())
                                    .try_into()
                                    .unwrap_or(1.try_into().unwrap()),
                            )
                            .unwrap();
                        Self::IntDynamic(ExprIntDynamic {
                            size,
                            attach_id,
                            attach_value: TokenField(tf_id),
                        })
                    }
                    // Raw value for the TokenField
                    _ => {
                        // TokenField size auto adjust it's size
                        let size = FieldSize::new_unsized()
                            .set_possible_bits(tf.bits.len())
                            .unwrap();
                        Self::TokenField(ExprTokenField { size, id: tf_id })
                    }
                }
            }
            ReadScope::Context(ctx_id) => {
                use crate::varnode::ContextAttach::*;
                let ctx = sleigh.context(ctx_id);
                match ctx.attach {
                    Some(Varnode(attach_id)) => {
                        Self::VarnodeDynamic(ExprVarnodeDynamic {
                            attach_id,
                            attach_value: DynamicValueType::Context(ctx_id),
                        })
                    }
                    _ => {
                        let size = FieldSize::default()
                            .set_min_bits(ctx.bitrange.bits.len())
                            .unwrap();
                        Self::Context(ExprContext { size, id: ctx_id })
                    }
                }
            }
            ReadScope::InstStart => Self::InstStart(InstStart),
            ReadScope::InstNext => Self::InstNext(InstNext),
            ReadScope::Varnode(id) => Self::Varnode(id),
            ReadScope::Bitrange(id) => {
                let bitrange = sleigh.bitrange(id);
                let size = FieldSize::new_unsized()
                    .set_min_bits(bitrange.bits.len())
                    .unwrap()
                    .set_possible_min();
                Self::Bitrange(ExprBitrange { size, id })
            }
            ReadScope::Table(id) => Self::Table(id),
            ReadScope::DisVar(id) => {
                let size = FieldSize::new_unsized();
                Self::DisVar(ExprDisVar { size, id })
            }
            ReadScope::ExeVar(id) => Self::ExeVar(id),
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
                FieldSize::new_bytes(sleigh.varnode(*var).len_bytes),
            )),
            Self::Table(table_id) => Box::new(sleigh.table(*table_id)),
            Self::ExeVar(var_id) => Box::new(&execution.variable(*var_id).size),
            Self::IntDynamic(ExprIntDynamic { size, .. }) => Box::new(size),
            Self::VarnodeDynamic(ExprVarnodeDynamic { attach_id, .. }) => {
                Box::new(FieldSizeUnmutable::from(FieldSize::new_bytes(
                    sleigh.attach_varnodes_len_bytes(*attach_id),
                )))
            }
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
            Self::Varnode(id) => {
                FieldSize::new_bytes(sleigh.varnode(*id).len_bytes)
            }
            Self::Table(id) => *sleigh
                .table(*id)
                .export
                .borrow()
                .as_ref()
                .unwrap()
                .size()
                .unwrap(),
            Self::ExeVar(id) => execution.variable(*id).size.get(),
            Self::IntDynamic(ExprIntDynamic { size, .. }) => *size,
            Self::VarnodeDynamic(ExprVarnodeDynamic { attach_id, .. }) => {
                FieldSize::new_bytes(
                    sleigh.attach_varnodes_len_bytes(*attach_id),
                )
            }
        }
    }
    pub fn solve(
        &mut self,
        location: &Span,
        _sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        match self {
            //don't call table solve directly, let the main loop do it
            Self::Table(_) => {}
            Self::Context(ctx) => {
                mark_unfinished_size!(&ctx.size, solved, location)
            }
            Self::Bitrange(bit) => {
                mark_unfinished_size!(&bit.size, solved, location)
            }
            Self::TokenField(tf) => {
                mark_unfinished_size!(&tf.size, solved, location)
            }
            // the len don't need solving
            Self::Varnode(_) | Self::InstStart(_) | Self::InstNext(_) => {}
            Self::ExeVar(id) => {
                let var = execution.variable(*id);
                mark_unfinished_size!(&var.size.get(), solved, location)
            }
            Self::Int(num) => {
                if num.size.is_unrestricted() {
                    num.size = num
                        .size
                        .set_min_bits(
                            u64::from(num.number.bits_required())
                                .try_into()
                                .unwrap(),
                        )
                        .unwrap();
                    solved.i_did_a_thing();
                }
                mark_unfinished_size!(&num.size, solved, location)
            }
            Self::DisVar(var) => {
                mark_unfinished_size!(&var.size, solved, location)
            }
            Self::IntDynamic(var) => {
                mark_unfinished_size!(&var.size, solved, location)
            }
            Self::VarnodeDynamic(_) => {}
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
            Self::IntDynamic(ExprIntDynamic {
                size,
                attach_id,
                attach_value,
            }) => FinalReadValue::IntDynamic(FinalExprDynamicInt {
                bits: size.possible_value().unwrap(),
                attach_id,
                attach_value,
            }),
            Self::VarnodeDynamic(var) => FinalReadValue::VarnodeDynamic(var),
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
    pub fn size(&self, sleigh: &Sleigh, execution: &Execution) -> FieldSize {
        match &self.op {
            Unary::TakeLsb(lsb) => FieldSize::new_bytes(*lsb),
            Unary::TrunkLsb { trunk: _, size }
            | Unary::BitRange { range: _, size } => *size,
            Unary::Dereference(mem) => mem.size,
            Unary::Zext(size)
            | Unary::Sext(size)
            | Unary::Popcount(size)
            | Unary::Lzcount(size)
            | Unary::FloatNan(size)
            | Unary::SignTrunc(size)
            | Unary::Float2Float(size)
            | Unary::Int2Float(size) => *size,
            Unary::Negation
            | Unary::BitNegation
            | Unary::Negative
            | Unary::FloatNegative
            | Unary::FloatAbs
            | Unary::FloatSqrt
            | Unary::FloatCeil
            | Unary::FloatFloor
            | Unary::FloatRound => self.input.size(sleigh, execution),
        }
    }

    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match &mut self.op {
            Unary::TakeLsb(lsb) => {
                Box::new(FieldSizeUnmutable::from(FieldSize::new_bytes(*lsb)))
            }
            Unary::TrunkLsb { trunk: _, size }
            | Unary::BitRange { range: _, size } => Box::new(size),
            Unary::Dereference(mem) => Box::new(&mut mem.size),
            Unary::Zext(size)
            | Unary::Sext(size)
            | Unary::Popcount(size)
            | Unary::Lzcount(size)
            | Unary::FloatNan(size)
            | Unary::SignTrunc(size)
            | Unary::Float2Float(size)
            | Unary::Int2Float(size) => Box::new(size),
            Unary::Negation
            | Unary::BitNegation
            | Unary::Negative
            | Unary::FloatNegative
            | Unary::FloatAbs
            | Unary::FloatSqrt
            | Unary::FloatCeil
            | Unary::FloatFloor
            | Unary::FloatRound => self.input.size_mut(sleigh, execution),
        }
    }

    pub fn convert(self) -> FinalExprUnaryOp {
        FinalExprUnaryOp {
            location: self.location,
            op: self.op.convert(),
            input: Box::new(self.input.convert()),
        }
    }
}

impl Unary {
    pub fn convert(self) -> FinalUnary {
        match self {
            Unary::TakeLsb(len) => FinalUnary::TakeLsb(len),
            Unary::TrunkLsb { trunk, size } => FinalUnary::TrunkLsb {
                trunk,
                bits: size.possible_value().unwrap(),
            },
            Unary::BitRange { range, size } => FinalUnary::BitRange {
                range,
                bits: size.possible_value().unwrap(),
            },
            Unary::Dereference(mem) => FinalUnary::Dereference(mem.convert()),
            Unary::Zext(size) => {
                FinalUnary::Zext(size.possible_value().unwrap())
            }
            Unary::Sext(size) => {
                FinalUnary::Sext(size.possible_value().unwrap())
            }
            Unary::Popcount(size) => {
                FinalUnary::Popcount(size.possible_value().unwrap())
            }
            Unary::Lzcount(size) => {
                FinalUnary::Lzcount(size.possible_value().unwrap())
            }
            Unary::FloatNan(size) => {
                FinalUnary::FloatNan(size.possible_value().unwrap())
            }
            Unary::SignTrunc(size) => {
                FinalUnary::SignTrunc(size.possible_value().unwrap())
            }
            Unary::Float2Float(size) => {
                FinalUnary::Float2Float(size.possible_value().unwrap())
            }
            Unary::Int2Float(size) => {
                FinalUnary::Int2Float(size.possible_value().unwrap())
            }
            Unary::Negation => FinalUnary::Negation,
            Unary::BitNegation => FinalUnary::BitNegation,
            Unary::Negative => FinalUnary::Negative,
            Unary::FloatNegative => FinalUnary::FloatNegative,
            Unary::FloatAbs => FinalUnary::FloatAbs,
            Unary::FloatSqrt => FinalUnary::FloatSqrt,
            Unary::FloatCeil => FinalUnary::FloatCeil,
            Unary::FloatFloor => FinalUnary::FloatFloor,
            Unary::FloatRound => FinalUnary::FloatRound,
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
            size: self.size.possible_value().unwrap(),
            number: self.number,
        }
    }

    pub fn new(number: Number) -> ExprNumber {
        Self {
            number,
            size: FieldSize::new_unsized().set_possible_min(),
        }
    }
}

impl ExprTokenField {
    pub fn convert(self) -> FinalExprTokenField {
        FinalExprTokenField {
            size: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }

    pub fn new(sleigh: &Sleigh, id: TokenFieldId) -> ExprTokenField {
        let token_field = sleigh.token_field(id);
        let min_bits = token_field.bits.len();
        let size = FieldSize::default().set_min_bits(min_bits).unwrap();
        Self { id, size }
    }
}

impl ExprContext {
    pub fn convert(self) -> FinalExprContext {
        FinalExprContext {
            size: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }

    pub fn new(sleigh: &Sleigh, id: ContextId) -> ExprContext {
        let context = sleigh.context(id);
        let min_bits = context.bitrange.bits.len();
        let size = FieldSize::default().set_min_bits(min_bits).unwrap();
        Self { id, size }
    }
}

impl ExprBitrange {
    pub fn convert(self) -> FinalExprBitrange {
        FinalExprBitrange {
            size: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }

    pub fn new(sleigh: &Sleigh, id: BitrangeId) -> ExprBitrange {
        let bitrange = sleigh.bitrange(id);
        let min_bits = bitrange.bits.len();
        let size = FieldSize::default()
            .set_min_bits(min_bits)
            .unwrap()
            .set_possible_min();
        Self { id, size }
    }
}

impl ExprDisVar {
    pub fn convert(self) -> FinalExprDisVar {
        FinalExprDisVar {
            size: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }
}

fn inner_expr_solve(
    mut op: ExprBinaryOp,
    sleigh: &Sleigh,
    execution: &Execution,
    solved: &mut impl SolverStatus,
) -> Result<Expr, Box<ExecutionError>> {
    use Binary::*;
    use ExprElement as Ele;
    use ExprValue as Value;

    //TODO make the moves and mut ref more elegant
    match (*op.left, op.op, *op.right) {
        //if two Integer, calculate it and replace self with the result.
        (
            Expr::Value(Ele::Value {
                location: _,
                value: Value::Int(left),
            }),
            op_binary,
            Expr::Value(Ele::Value {
                location,
                value: Value::Int(right),
            }),
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
                .ok_or_else(|| {
                    ExecutionError::OperationOverflow(op.location.clone())
                })?;
            //replace self with our new value
            Ok(Expr::Value(Ele::Value {
                location,
                value: Value::Int(ExprNumber::new(value.into())),
            }))
        }

        // HACK: convert some kinds of bit_and into bitrange if the value is a
        // an bitrange starting from 0 (eg: 0b1 0b11 0b111 0b1111 0b11111 etc)
        // and the output size is expected to reduce the size of the input
        // using the bitwise op.
        // eg: `reg0[0,1] = value & 1; reg1[0,2] = value & 3`
        (
            value,
            BitAnd,
            Expr::Value(Ele::Value {
                location,
                value:
                    Value::Int(ExprNumber {
                        number: integer,
                        size: _,
                    }),
            }),
        )
        | (
            Expr::Value(Ele::Value {
                location,
                value:
                    Value::Int(ExprNumber {
                        number: integer,
                        size: _,
                    }),
            }),
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
                .size(sleigh, execution)
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
            let mut value = value;
            // input value can have any len, has long it's greater then bitrange
            value
                .size_mut(sleigh, execution)
                .update_action(|s| s.set_possible_min().set_min_bits(size))
                .unwrap();
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
            Expr::Value(Ele::Value {
                location,
                value:
                    Value::Int(ExprNumber {
                        number: lsb,
                        size: _,
                    }),
            }),
        ) if op
            .output_size
            .final_value()
            .zip(value.size(sleigh, execution).final_value())
            .map(|(out_bits, val_bits)| (out_bits.get(), val_bits.get()))
            .map(|(out_bits, val_bits)| {
                val_bits >= lsb.as_unsigned().unwrap()
                    && out_bits == (val_bits - lsb.as_unsigned().unwrap())
            })
            .unwrap_or(false) =>
        {
            solved.i_did_a_thing();
            solved.iam_not_finished(&location, file!(), line!());
            let size = op.output_size.final_value().unwrap();
            //take the value from self, and put on the new self
            //safe because the self is overwriten after
            Ok(Expr::Value(Ele::new_bitrange(
                location,
                lsb.as_unsigned().unwrap(),
                size,
                value,
            )))
        }

        //output and left have the same size, right can have any size
        (mut left, Lsl | Lsr | Asr, mut right) => {
            left.solve(sleigh, execution, solved)?;
            //NOTE right defaults to 32bits
            right.solve(sleigh, execution, solved)?;

            // right can have any size, 32bits if possible
            right
                .size_mut(sleigh, execution)
                .update_action(|s| {
                    let s = s.set_possible_min();
                    Some(
                        s.set_possible_bits(32.try_into().unwrap())
                            .unwrap_or(s),
                    )
                })
                .unwrap();

            let restricted = len::a_generate_b(
                &mut *left.size_mut(sleigh, execution),
                &mut &mut op.output_size,
            );
            let restricted = restricted.ok_or_else(|| {
                VarSizeError::ShiftLeftOutputDiff {
                    left: left.size(sleigh, execution),
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
            // TODO those are binary and implicitly with a != 0 verification,
            // we may need to add a set_possible_max for this
            //left/right can have any lenght
            left.size_mut(sleigh, execution)
                .update_action(|x| Some(x.set_possible_min()))
                .unwrap();
            right
                .size_mut(sleigh, execution)
                .update_action(|x| Some(x.set_possible_min()))
                .unwrap();

            left.solve(sleigh, execution, solved)?;
            right.solve(sleigh, execution, solved)?;
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
            left.solve(sleigh, execution, solved)?;
            right.solve(sleigh, execution, solved)?;
            let restricted = len::a_b_generate_c(
                &mut *left.size_mut(sleigh, execution),
                &mut *right.size_mut(sleigh, execution),
                &mut &mut op.output_size,
            );
            let restricted =
                restricted.ok_or_else(|| VarSizeError::TriBinaryOp {
                    left: left.size(sleigh, execution),
                    right: right.size(sleigh, execution),
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
            left.solve(sleigh, execution, solved)?;
            right.solve(sleigh, execution, solved)?;
            //Both sides need to have the same number of bits.
            //output can have any size because is always 0/1
            let restricted = len::a_cmp_b(
                &mut *left.size_mut(sleigh, execution),
                &mut *right.size_mut(sleigh, execution),
            );
            let restricted =
                restricted.ok_or_else(|| VarSizeError::BoolBinaryOp {
                    left: left.size(sleigh, execution),
                    right: right.size(sleigh, execution),
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
