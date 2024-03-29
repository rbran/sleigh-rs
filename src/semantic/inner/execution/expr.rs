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
use crate::semantic::inner::execution::restrict_field_same_size;
use crate::semantic::inner::{FieldSize, Sleigh, Solved, SolverStatus};
use crate::semantic::{disassembly, InstNext, InstStart};
use crate::{
    BitrangeId, ContextId, ExecutionError, Number, NumberNonZeroUnsigned,
    NumberUnsigned, Span, TokenFieldId,
};

use super::{
    ExportLen, FieldSizeMut, FieldSizeUnmutable, MemoryLocation, ReadScope,
    Truncate, UserCall, Variable, FIELD_SIZE_BOOL,
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
    Truncate(Span, Truncate, Box<Expr>),
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
        variables: &[Variable],
        src: Span,
        op: Binary,
        left: Expr,
        mut right: Expr,
    ) -> Self {
        use Binary::*;
        let size = match op {
            Lsl | Lsr | Asr => {
                // rotation is unlikelly to rotate more then 128 bits,
                // so right element size only require 32bits max
                // NOTE error is ignored
                let _ =
                    right.size_mut(sleigh, variables).update_action(|size| {
                        size.set_possible_bits(32.try_into().unwrap())
                    });
                FieldSize::new_unsized()
            }
            SigLess | SigGreater | SigRem | SigLessEq | SigGreaterEq | Less
            | Greater | LessEq | GreaterEq | FloatLess | FloatGreater
            | FloatLessEq | FloatGreaterEq | And | Xor | Or | Eq | Ne
            | FloatEq | FloatNe | Carry | SCarry | SBorrow => FIELD_SIZE_BOOL,
            Mult | Div | SigDiv | Rem | FloatDiv | FloatMult | Sub
            | FloatAdd | FloatSub | BitAnd | BitXor | BitOr | Add => {
                FieldSize::new_unsized()
            }
        };
        Self::Op(ExprBinaryOp {
            location: src,
            output_size: size,
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
    pub fn size(&self, sleigh: &Sleigh, variables: &[Variable]) -> FieldSize {
        match self {
            Expr::Value(value) => value.size(sleigh, variables),
            Expr::Op(op) => op.output_size,
        }
    }
    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        variables: &'a [Variable],
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Expr::Value(value) => value.size_mut(sleigh, variables),
            Expr::Op(op) => Box::new(&mut op.output_size),
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        // if a value, just resolve the value and return
        if let Expr::Value(value) = self {
            return value.solve(sleigh, variables, solved);
        }

        // if not a value, take self, and replace it by a dummy value
        //TODO make this less akward and remove the unecessary deref
        //and recreation of Box's
        //akwardly move values from self, replacing with a dummy value
        let dummy_src = Span::File(crate::FileSpan {
            start: crate::FileLocation::new_start(""),
            end_line: 0,
            end_column: 0,
        });
        let mut slf_moved =
            Expr::Value(ExprElement::Value(ExprValue::Int(ExprNumber {
                location: dummy_src,
                size: FieldSize::default(),
                number: Number::Positive(0),
            })));
        std::mem::swap(self, &mut slf_moved);

        //TODO make the moves and mut ref more elegant
        let Expr::Op(op_moved) = slf_moved else {
            unreachable!();
        };
        *self = inner_expr_solve(op_moved, sleigh, variables, solved)?;
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
    pub fn new_truncate(src: Span, truncate: Truncate, expr: Expr) -> Self {
        Self::Truncate(src, truncate, Box::new(expr))
    }
    pub fn new_deref(
        sleigh: &Sleigh,
        variables: &[Variable],
        src: Span,
        deref: MemoryLocation,
        mut addr: Expr,
    ) -> Self {
        let space = sleigh.space(deref.space);
        //addr expr, need to be the space_addr size
        addr.size_mut(sleigh, variables)
            .set(FieldSize::new_bytes(space.addr_bytes));
        Self::DeReference(src, deref, Box::new(addr))
    }
    pub fn new_op(src: Span, mut op: Unary, expr: Expr) -> Self {
        let size = match &mut op {
            Unary::FloatNan => {
                //the output can be one bit (true/false)
                FieldSize::new_unsized().set_possible_min()
            }
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
            Self::DeReference(src, _, _) | Self::Truncate(src, _, _) => src,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        let mut modified = false;
        match self {
            Self::Value(value) => value.solve(solved)?,
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
            Self::Truncate(src, truncate, input) => {
                let error = || Box::new(ExecutionError::VarSize(src.clone()));
                let mut input_len = input.size_mut(sleigh, variables);
                let output_len = &mut truncate.size;

                // input len need to be (output_len + truncate.lsb) or bigger
                if let Some(min_bits) = output_len.min_bits() {
                    modified |= input_len
                        .update_action(|input_len| {
                            input_len.set_min_bits(
                                (truncate.lsb + min_bits.get())
                                    .try_into()
                                    .unwrap(),
                            )
                        })
                        .ok_or_else(error)?;
                }
                // truncation can't return 0 bits, so input len need to be
                // bigger then truncate.lsb, NOTE not equal
                if matches!(input_len.get().max_bits(), Some(max_bits) if max_bits.get()
                    <= truncate.lsb)
                {
                    return Err(error());
                }
                // output_len need to be (input_len - truncate.lsb) or smaller
                if let Some(max_bits) = input_len.get().max_bits() {
                    modified |= output_len
                        .update_action(|output_len| {
                            output_len.set_max_bits(
                                (max_bits.get() - truncate.lsb)
                                    .try_into()
                                    .unwrap(),
                            )
                        })
                        .ok_or_else(error)?;
                }

                // if the input or output len are not possible, we are not done
                if !input_len.get().is_possible() || !output_len.is_possible() {
                    solved.iam_not_finished(src, file!(), line!());
                }
                // try to solve the input with the new information
                drop(input_len);
                input.solve(sleigh, variables, solved)?;
            }
            Self::DeReference(_, deref, value) => {
                deref.solve(solved);
                value.solve(sleigh, variables, solved)?;
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
                //if one can be min size, both can be
                if size.possible_min()
                    || input.size(sleigh, variables).possible_min()
                {
                    let set_min =
                        |size: FieldSize| Some(size.set_possible_min());
                    modified |= size.update_action(set_min).unwrap();
                    modified |= input
                        .size_mut(sleigh, variables)
                        .update_action(set_min)
                        .unwrap();
                }
                //the input and output have the same number of bits
                {
                    let mut input_size = input.size_mut(sleigh, variables);
                    modified |=
                        restrict_field_same_size(&mut [&mut *input_size, size])
                            .ok_or_else(|| {
                                ExecutionError::VarSize(location.clone())
                            })?;
                }
                mark_unfinished_size!(
                    &input.size(sleigh, variables),
                    solved,
                    location
                );
                input.solve(sleigh, variables, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location: src,
                output_size: size,
                op: Unary::Popcount | Unary::Lzcount,
                input,
            }) => {
                //the output min size is: log2(bit_len(input) + 1)
                if let Some(input_num_bits) =
                    input.size(sleigh, variables).final_value()
                {
                    //equivalent to log2(bit_len(input) + 1)
                    let output_min = NumberUnsigned::BITS
                        - input_num_bits.get().leading_zeros();
                    if let Some(output_min) =
                        NumberNonZeroUnsigned::new(output_min.into())
                    {
                        if size
                            .update_action(|size| {
                                size.set_possible_min().set_min_bits(output_min)
                            })
                            .ok_or_else(|| {
                                ExecutionError::VarSize(src.clone())
                            })?
                        {
                            solved.i_did_a_thing();
                        }
                    }
                }
                mark_unfinished_size!(size, solved, src);
                input.solve(sleigh, variables, solved)?;
            }

            Self::Op(ExprUnaryOp {
                location: src,
                output_size,
                op: Unary::Zext | Unary::Sext,
                input: value,
            }) => {
                let error = || ExecutionError::VarSize(src.clone());
                //output size need to be bigger or eq to the value size
                if let Some(min_bits) = value.size(sleigh, variables).min_bits()
                {
                    modified |= output_size
                        .update_action(|size| size.set_min_bits(min_bits))
                        .ok_or_else(error)?;
                }
                //and vise-versa
                if let Some(max_bits) = output_size.max_bits() {
                    modified |= value
                        .size_mut(sleigh, variables)
                        .update_action(|size| size.set_max_bits(max_bits))
                        .ok_or_else(error)?;
                }

                mark_unfinished_size!(output_size, solved, src);
                mark_unfinished_size!(
                    &value.size(sleigh, variables),
                    solved,
                    src
                );
                value.solve(sleigh, variables, solved)?;
            }
            // NOTE don't confuse signed truncation with regular truncation
            // sleigh `trunc` function converts float into interger, both
            // can have any size.
            Self::Op(ExprUnaryOp {
                location,
                output_size,
                op: Unary::SignTrunc,
                input,
            }) => {
                mark_unfinished_size!(output_size, solved, location);
                mark_unfinished_size!(
                    &input.size(sleigh, variables),
                    solved,
                    location
                );
                input.solve(sleigh, variables, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
                output_size,
                op: Unary::Float2Float | Unary::Int2Float,
                input,
            }) => {
                //input and output can have any size
                mark_unfinished_size!(output_size, solved, location);
                mark_unfinished_size!(
                    &input.size(sleigh, variables),
                    solved,
                    location,
                );
                input.solve(sleigh, variables, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location,
                output_size,
                op: Unary::FloatNan,
                input,
            }) => {
                mark_unfinished_size!(output_size, solved, location);
                input.solve(sleigh, variables, solved)?;
            }
            Self::UserCall(UserCall {
                output_size,
                params,
                location,
                ..
            }) => {
                mark_unfinished_size!(output_size, solved, location,);
                params.iter_mut().try_for_each(|param| {
                    param.solve(sleigh, variables, solved)
                })?;
            }
            Self::New(_) => todo!(),
            Self::CPool(_) => todo!(),
            ExprElement::Op(ExprUnaryOp {
                op: Unary::Truncate(_) | Unary::Dereference(_),
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
            Self::Truncate(src, truncate, value) => {
                FinalExprElement::Op(FinalExprUnaryOp {
                    location: src,
                    output_bits: truncate.size.final_value().unwrap(),
                    op: Unary::Truncate(truncate.convert()),
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
        variables: &'a [Variable],
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Value(value) => value.size_mut(sleigh, variables),
            Self::DeReference(_, deref, _) => Box::new(&mut deref.size),
            Self::Truncate(_, trunc, _) => Box::new(&mut trunc.size),
            Self::Reference(x) => Box::new(&mut x.len),
            Self::Op(x) => Box::new(&mut x.output_size),
            Self::UserCall(x) => Box::new(&mut x.output_size),
            Self::New(_x) => todo!(),
            Self::CPool(_x) => todo!(),
        }
    }
    pub fn size(&self, sleigh: &Sleigh, variables: &[Variable]) -> FieldSize {
        match self {
            Self::Value(value) => value.size(sleigh, variables),
            Self::DeReference(_, deref, _) => deref.size,
            Self::Truncate(_, trunc, _) => trunc.size,
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
                let token_field = sleigh.token_field(id);
                let size = FieldSize::new_unsized()
                    .set_min_bits(token_field.bits.len())
                    .unwrap();
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
                let context = sleigh.context(id);
                let size = FieldSize::new_unsized()
                    .set_min_bits(context.bitrange.bits.len())
                    .unwrap();
                Self::Context(ExprContext { location, size, id })
            }
            ReadScope::Bitrange(id) => {
                let bitrange = sleigh.bitrange(id);
                let size = FieldSize::new_unsized()
                    .set_min_bits(bitrange.bits.len())
                    .unwrap();
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
        variables: &'a [Variable],
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Int(x) => Box::new(&mut x.size),
            Self::DisVar(x) => Box::new(&mut x.size),
            // TODO exec_value_len stuff here
            Self::TokenField(x) => Box::new(&mut x.size),
            Self::Context(x) => Box::new(&mut x.size),
            Self::Bitrange(x) => Box::new(&mut x.size),
            Self::InstStart(_) | Self::InstNext(_) => {
                Box::new(FieldSizeUnmutable::from(self.size(sleigh, variables)))
            }
            Self::Varnode(var) => Box::new(FieldSizeUnmutable::from(
                FieldSize::new_bytes(sleigh.varnode(var.id).len_bytes),
            )),
            Self::Table(x) => Box::new(sleigh.table(x.id)),
            Self::ExeVar(x) => Box::new(&variables[x.id.0].size),
        }
    }
    pub fn size(&self, sleigh: &Sleigh, variables: &[Variable]) -> FieldSize {
        match self {
            Self::Int(x) => x.size,
            Self::DisVar(x) => x.size,
            Self::Context(x) => {
                sleigh.context(x.id).exec_out_value_bits(sleigh)
            }
            Self::Bitrange(x) => {
                FieldSize::new_bits(sleigh.bitrange(x.id).bits.len())
            }
            Self::TokenField(x) => {
                sleigh.token_field(x.id).exec_value_len(sleigh)
            }
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
            Self::ExeVar(x) => variables[x.id.0].size.get(),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        match self {
            //don't call table solve directly, let the main loop do it
            Self::Table(_) | Self::ExeVar(_) => (),
            Self::Context(_) | Self::Bitrange(_) | Self::TokenField(_) => (),
            // the len don't need solving
            Self::Varnode(_) | Self::InstStart(_) | Self::InstNext(_) => (),
            Self::Int(num) => {
                mark_unfinished_size!(&num.size, solved, &num.location,)
            }
            Self::DisVar(var) => {
                mark_unfinished_size!(&var.size, solved, &var.location,)
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
            .set_min_bits(
                NumberNonZeroUnsigned::new(number.bits_required().into())
                    .unwrap(),
            )
            .unwrap();
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
        let size = FieldSize::default().set_min_bits(min_bits).unwrap();
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
    variables: &[Variable],
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

        //convert some kinds of bit_and into bitrange if the value is a an
        //bitrange starting from 0 (eg: 0b1 0b11 0b111 0b1111 0b11111 etc)
        //and the output size is expected to reduce the size of the input
        //using the bitwise op.
        //eg: `reg0[0,1] = value & 1; reg1[0,2] = value & 3`
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
            Ok(Expr::Value(ExprElement::Truncate(
                location,
                Truncate::new(0, size),
                Box::new(value),
            )))
        }

        //TODO create an error if the value is too big, and as_unsigned
        //fails for now let the unwrap so we can detect if ghidra uses
        //value > u64

        //convert if the output bit size, if the left hand is a value with
        //an defined bit size and the right side an integer, the output
        //can be a bitrange truncate from the left.
        //eg `value` is 8bits: `tmp = value >> 7; => tmp = value[7,1]`
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
            Ok(Expr::Value(Ele::Truncate(
                src.clone(),
                Truncate::new(lsb.as_unsigned().unwrap(), size),
                Box::new(value),
            )))
        }

        //output and left have the same size, right can have any size
        (mut left, Lsl | Lsr | Asr, mut right) => {
            left.solve(sleigh, variables, solved)?;
            //NOTE right defaults to 32bits
            right.solve(sleigh, variables, solved)?;

            let output_size = &mut op.output_size;
            let output_size: &mut dyn FieldSizeMut = output_size;
            let restricted = restrict_field_same_size(&mut [
                left.size_mut(sleigh, variables).as_dyn(),
                output_size,
            ])
            .ok_or_else(|| ExecutionError::VarSize(op.location.clone()))?;
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

        //All sides need to have the same number of bits.
        (
            mut left,
            Mult | FloatMult | Div | SigDiv | FloatDiv | Rem | SigRem | Add
            | FloatAdd | Sub | FloatSub | BitAnd | BitXor | BitOr,
            mut right,
        ) => {
            left.solve(sleigh, variables, solved)?;
            right.solve(sleigh, variables, solved)?;
            let restricted = restrict_field_same_size(&mut [
                left.size_mut(sleigh, variables).as_dyn(),
                right.size_mut(sleigh, variables).as_dyn(),
                &mut op.output_size,
            ])
            .ok_or_else(|| ExecutionError::VarSize(op.location.clone()))?;
            if restricted {
                solved.i_did_a_thing();
            }
            //TODO is that really right?
            //if the output have a possible size, make left/right also have
            if let Some(possible) = op.output_size.possible_value() {
                let new_left =
                    left.size(sleigh, variables).set_possible_bits(possible);
                let new_right =
                    right.size(sleigh, variables).set_possible_bits(possible);
                if let Some((new_left, new_right)) = new_left.zip(new_right) {
                    left.size_mut(sleigh, variables).set(new_left);
                    right.size_mut(sleigh, variables).set(new_right);
                }
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
            let restricted = restrict_field_same_size(&mut [
                left.size_mut(sleigh, variables).as_dyn(),
                right.size_mut(sleigh, variables).as_dyn(),
            ])
            .ok_or_else(|| ExecutionError::VarSize(op.location.clone()))?;
            if restricted {
                solved.i_did_a_thing();
            }
            mark_unfinished_size!(
                &left.size(sleigh, variables),
                solved,
                &op.location
            );
            mark_unfinished_size!(
                &right.size(sleigh, variables),
                solved,
                &op.location
            );
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
