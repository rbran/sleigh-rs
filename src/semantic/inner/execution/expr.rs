use crate::semantic::disassembly;
use crate::semantic::execution::{
    Binary, Expr as FinalExpr, ExprBinaryOp as FinalExprBinaryOp, ExprBitrange,
    ExprCPool as FinalExprCPool, ExprContext, ExprDisVar as FinalExprDisVar,
    ExprElement as FinalExprElement, ExprExeVar, ExprInstNext, ExprInstStart,
    ExprNew as FinalExprNew, ExprNumber as FinalExprNumber, ExprTable,
    ExprTokenField, ExprUnaryOp as FinalExprUnaryOp, ExprVarnode,
    ReadValue as FinalReadValue, Reference as FinalReference, ReferencedValue,
    Unary,
};
use crate::semantic::inner::{FieldSize, Sleigh, Solved, SolverStatus};
use crate::{
    ExecutionError, Number, NumberNonZeroUnsigned, NumberUnsigned, Span,
};

use super::{
    Execution, ExportLen, FieldSizeIntersectIter, FieldSizeMut,
    FieldSizeMutOwned, FieldSizeMutRef, MemoryLocation, Truncate, UserCall,
    FIELD_SIZE_BOOL,
};

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
    Value(ReadValue),
    UserCall(UserCall),
    Reference(Reference),
    DeReference(Span, MemoryLocation, Box<Expr>),
    Truncate(Span, Truncate, Box<Expr>),
    Op(ExprUnaryOp),
    New(ExprNew),
    CPool(ExprCPool),
}

#[derive(Clone, Debug)]
pub enum ReadValue {
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
        src: Span,
        op: Binary,
        left: Expr,
        mut right: Expr,
    ) -> Self {
        let size = match op {
            Binary::Lsl | Binary::Lsr | Binary::Asr => {
                //rotation is unlikelly to rotate more then 128 bits,
                //so right element size only require 32bits max
                right.size_mut(sleigh, execution).update_action(|size| {
                    size.set_possible_value(32.try_into().unwrap())
                });
                FieldSize::new_unsized()
            }
            Binary::SigLess
            | Binary::SigGreater
            | Binary::SigRem
            | Binary::SigLessEq
            | Binary::SigGreaterEq
            | Binary::Less
            | Binary::Greater
            | Binary::LessEq
            | Binary::GreaterEq
            | Binary::FloatLess
            | Binary::FloatGreater
            | Binary::FloatLessEq
            | Binary::FloatGreaterEq
            | Binary::And
            | Binary::Xor
            | Binary::Or
            | Binary::Eq
            | Binary::Ne
            | Binary::FloatEq
            | Binary::FloatNe
            | Binary::Carry
            | Binary::SCarry
            | Binary::SBorrow => FIELD_SIZE_BOOL,
            Binary::Mult
            | Binary::Div
            | Binary::SigDiv
            | Binary::Rem
            | Binary::FloatDiv
            | Binary::FloatMult
            | Binary::Sub
            | Binary::FloatAdd
            | Binary::FloatSub
            | Binary::BitAnd
            | Binary::BitXor
            | Binary::BitOr
            | Binary::Add => FieldSize::new_unsized(),
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
    pub fn size(&self, sleigh: &Sleigh, constructor: &Execution) -> FieldSize {
        match self {
            Expr::Value(value) => value.size(sleigh, constructor),
            Expr::Op(op) => op.output_size,
        }
    }
    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        constructor: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Expr::Value(value) => value.size_mut(sleigh, constructor),
            Expr::Op(op) => {
                Box::new(FieldSizeMutRef::from(&mut op.output_size))
            }
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        constructor: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let (self_moved, mut op) = match self {
            Expr::Value(value) => {
                return value.solve(sleigh, constructor, solved)
            }
            self_moved @ Expr::Op(_) => {
                //TODO make this less akward and remove the unecessary deref
                //and recreation of Box's
                //akwardly move values from self, replacing with a dummy value
                let dummy_src = Span::File(crate::FileSpan {
                    start: crate::FileLocation {
                        file: std::rc::Rc::from(std::path::Path::new("")),
                        line: 0,
                        column: 0,
                    },
                    end_line: 0,
                    end_column: 0,
                });
                let mut self_tmp = Expr::Value(ExprElement::Value(
                    ReadValue::Int(ExprNumber {
                        location: dummy_src,
                        size: FieldSize::default(),
                        number: Number::Positive(0),
                    }),
                ));
                std::mem::swap(self_moved, &mut self_tmp);
                match self_tmp {
                    Expr::Op(op) => (self_moved, op),
                    _ => unreachable!(),
                }
            }
        };

        use ExprElement as Ele;
        use ReadValue as Value;

        let mut modified = false;
        //TODO make the moves and mut ref more elegant
        *self_moved = match (*op.left, op.op, *op.right) {
            //if two Integer, calculate it and replace self with the result.
            (
                Expr::Value(Ele::Value(Value::Int(left))),
                op_binary,
                Expr::Value(Ele::Value(Value::Int(right))),
            ) => {
                solved.i_did_a_thing();
                solved.iam_not_finished_location(
                    &op.location,
                    file!(),
                    line!(),
                );
                //TODO create an error if the value is too big, for now let the
                //unwrap so we can detect if ghidra uses value > u64
                let value = op_binary
                    .execute(
                        left.number.as_unsigned().unwrap(),
                        right.number.as_unsigned().unwrap(),
                    )
                    .ok_or_else(|| {
                        //TODO better error
                        ExecutionError::InvalidExport
                    })?;
                //replace self with our new value
                Expr::Value(Ele::Value(Value::Int(ExprNumber::new(
                    op.location,
                    value.into(),
                ))))
            }

            //convert some kinds of bit_and into bitrange if the value is a an
            //bitrange starting from 0 (eg: 0b1 0b11 0b111 0b1111 0b11111 etc)
            //and the output size is expected to reduce the size of the input
            //using the bitwise op.
            //eg: `reg0[0,1] = value & 1; reg1[0,2] = value & 3`
            (
                value,
                Binary::BitAnd,
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
                Binary::BitAnd,
                value,
            ) if op
                .output_size
                .final_value()
                .map(|bits| {
                    bits.get()
                        == integer.as_unsigned().unwrap().count_ones().into()
                })
                .unwrap_or(false)
                && value
                    .size(sleigh, constructor)
                    .final_value()
                    .map(|bits| {
                        bits.get()
                            >= integer
                                .as_unsigned()
                                .unwrap()
                                .count_ones()
                                .into()
                    })
                    .unwrap_or(true) =>
            {
                solved.i_did_a_thing();
                solved.iam_not_finished_location(
                    &op.location,
                    file!(),
                    line!(),
                );
                let size = op.output_size.final_value().unwrap();
                Expr::Value(ExprElement::Truncate(
                    location,
                    Truncate::new(0, size),
                    Box::new(value),
                ))
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
                Binary::Lsr,
                Expr::Value(Ele::Value(Value::Int(ExprNumber {
                    location: src,
                    number: lsb,
                    size: _,
                }))),
            ) if op
                .output_size
                .final_value()
                .zip(value.size(sleigh, constructor).final_value())
                .map(|(out_bits, val_bits)| (out_bits.get(), val_bits.get()))
                .map(|(out_bits, val_bits)| {
                    val_bits >= lsb.as_unsigned().unwrap()
                        && out_bits == (val_bits - lsb.as_unsigned().unwrap())
                })
                .unwrap_or(false) =>
            {
                solved.i_did_a_thing();
                solved.iam_not_finished_location(&src, file!(), line!());
                let size = op.output_size.final_value().unwrap();
                //take the value from self, and put on the new self
                //safe because the self is overwriten after
                Expr::Value(Ele::Truncate(
                    src.clone(),
                    Truncate::new(lsb.as_unsigned().unwrap(), size),
                    Box::new(value),
                ))
            }

            //output and left have the same size, right can have any size
            (mut left, Binary::Lsl | Binary::Lsr | Binary::Asr, mut right) => {
                left.solve(sleigh, constructor, solved)?;
                //NOTE right defaults to 32bits
                right.solve(sleigh, constructor, solved)?;

                modified |= [
                    &mut left.size_mut(sleigh, constructor)
                        as &mut dyn FieldSizeMut,
                    &mut FieldSizeMutRef::from(&mut op.output_size),
                ]
                .all_same_lenght()
                .ok_or_else(|| ExecutionError::VarSize(op.location.clone()))?;
                if op.output_size.is_undefined() {
                    solved.iam_not_finished_location(
                        &op.location,
                        file!(),
                        line!(),
                    );
                }
                Expr::Op(ExprBinaryOp {
                    location: op.location,
                    output_size: op.output_size,
                    op: op.op,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            //left/right/output can have any size, they are all just `0` or `!=0`
            (mut left, Binary::And | Binary::Xor | Binary::Or, mut right) => {
                //left/right can have any lenght, so just try to solve the len
                //if possible, otherwise just ignore it
                left.solve(sleigh, constructor, &mut Solved::default())?;
                right.solve(sleigh, constructor, &mut Solved::default())?;
                if op.output_size.is_undefined() {
                    solved.iam_not_finished_location(
                        &op.location,
                        file!(),
                        line!(),
                    );
                }
                Expr::Op(ExprBinaryOp {
                    location: op.location,
                    output_size: op.output_size,
                    op: op.op,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            //All sides need to have the same number of bits.
            (
                mut left,
                Binary::Mult
                | Binary::FloatMult
                | Binary::Div
                | Binary::SigDiv
                | Binary::FloatDiv
                | Binary::Rem
                | Binary::SigRem
                | Binary::Add
                | Binary::FloatAdd
                | Binary::Sub
                | Binary::FloatSub
                | Binary::BitAnd
                | Binary::BitXor
                | Binary::BitOr,
                mut right,
            ) => {
                left.solve(sleigh, constructor, solved)?;
                right.solve(sleigh, constructor, solved)?;
                modified |= [
                    &mut left.size_mut(sleigh, constructor)
                        as &mut dyn FieldSizeMut,
                    &mut right.size_mut(sleigh, constructor),
                    &mut FieldSizeMutRef::from(&mut op.output_size),
                ]
                .all_same_lenght()
                .ok_or_else(|| ExecutionError::VarSize(op.location.clone()))?;
                //TODO is that really right?
                //if the output have a possible size, make left/right also have
                if let Some(possible) = op.output_size.possible_value() {
                    let new_left = left
                        .size(sleigh, constructor)
                        .set_possible_value(possible);
                    let new_right = right
                        .size(sleigh, constructor)
                        .set_possible_value(possible);
                    if let Some((new_left, new_right)) = new_left.zip(new_right)
                    {
                        left.size_mut(sleigh, constructor).set(new_left);
                        right.size_mut(sleigh, constructor).set(new_right);
                    }
                }
                if op.output_size.is_undefined() {
                    solved.iam_not_finished_location(
                        &op.location,
                        file!(),
                        line!(),
                    );
                }
                Expr::Op(ExprBinaryOp {
                    location: op.location,
                    output_size: op.output_size,
                    op: op.op,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            //both need to have the same number of bits, the output is value 0/1
            (
                mut left,
                Binary::Less
                | Binary::SigLess
                | Binary::FloatLess
                | Binary::LessEq
                | Binary::SigLessEq
                | Binary::FloatLessEq
                | Binary::Greater
                | Binary::SigGreater
                | Binary::FloatGreater
                | Binary::GreaterEq
                | Binary::SigGreaterEq
                | Binary::FloatGreaterEq
                | Binary::Eq
                | Binary::FloatEq
                | Binary::Ne
                | Binary::FloatNe
                | Binary::Carry
                | Binary::SCarry
                | Binary::SBorrow,
                mut right,
            ) => {
                left.solve(sleigh, constructor, solved)?;
                right.solve(sleigh, constructor, solved)?;
                //Both sides need to have the same number of bits.
                //output can have any size because is always 0/1
                modified |= [
                    &mut left.size_mut(sleigh, constructor)
                        as &mut dyn FieldSizeMut,
                    &mut right.size_mut(sleigh, constructor),
                ]
                .all_same_lenght()
                .ok_or_else(|| ExecutionError::VarSize(op.location.clone()))?;
                if left.size(sleigh, constructor).is_undefined()
                    || right.size(sleigh, constructor).is_undefined()
                {
                    solved.iam_not_finished_location(
                        &op.location,
                        file!(),
                        line!(),
                    );
                }
                Expr::Op(ExprBinaryOp {
                    location: op.location,
                    output_size: op.output_size,
                    op: op.op,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
        };
        if modified {
            solved.i_did_a_thing();
        }
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
        constructor: &Execution,
        src: Span,
        deref: MemoryLocation,
        mut addr: Expr,
    ) -> Self {
        let space = sleigh.space(deref.space);
        //addr expr, need to be the space_addr size
        addr.size_mut(sleigh, constructor)
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
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
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
                let error = || ExecutionError::VarSize(src.clone());
                let mut input_len = input.size_mut(sleigh, execution);
                let output_len = &mut truncate.size;

                // input len need to be (output_len truncate.lsb) or bigger
                modified |= input_len
                    .update_action(|input_len| {
                        input_len.set_min(
                            (truncate.lsb + output_len.min().get())
                                .try_into()
                                .unwrap(),
                        )
                    })
                    .ok_or_else(error)?;
                // input len can't be smaller then (truncate.lsb + 1)
                if input_len.get().max().get() <= truncate.lsb {
                    return Err(error());
                }
                // output_len need to me (input_len - truncate.lsb) or smaller
                modified |= output_len
                    .update_action(|output_len| {
                        output_len.set_max(
                            (input_len.get().max().get() - truncate.lsb)
                                .try_into()
                                .unwrap(),
                        )
                    })
                    .ok_or_else(error)?;

                // if the output len is known it auto ajusts to the remaining
                // of the input
                if output_len.final_value().is_none() {
                    if let Some(input_len) = input_len.get().final_value() {
                        modified |= output_len
                            .update_action(|len| {
                                len.set_possible_value(
                                    (input_len.get() - truncate.lsb)
                                        .try_into()
                                        .unwrap(),
                                )
                            })
                            .ok_or_else(error)?;
                    }
                }
                // if the input or output len are not possible, we are not done
                if !input_len.get().is_possible() || !output_len.is_possible() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                // try to solve the input with the new information
                drop(input_len);
                input.solve(sleigh, execution, solved)?;
            }
            Self::DeReference(_, deref, value) => {
                deref.solve(solved);
                value.solve(sleigh, execution, solved)?;
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
                    || input.size(sleigh, execution).possible_min()
                {
                    let set_min =
                        |size: FieldSize| Some(size.set_possible_min());
                    modified |= size.update_action(set_min).unwrap();
                    modified |= input
                        .size_mut(sleigh, execution)
                        .update_action(set_min)
                        .unwrap();
                }
                //the input and output have the same number of bits
                {
                    let mut input_size = input.size_mut(sleigh, execution);
                    let mut size = FieldSizeMutRef::from(size);
                    let mut lens: [&mut dyn FieldSizeMut; 2] =
                        [&mut *input_size, &mut size];
                    modified |= lens.all_same_lenght().ok_or_else(|| {
                        ExecutionError::VarSize(location.clone())
                    })?;
                }
                if input.size(sleigh, execution).is_undefined() {
                    solved.iam_not_finished_location(
                        &location,
                        file!(),
                        line!(),
                    );
                }
                input.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location: src,
                output_size: size,
                op: Unary::Popcount,
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
                        if size
                            .update_action(|size| {
                                size.set_possible_min().set_min(output_min)
                            })
                            .ok_or_else(|| {
                                ExecutionError::VarSize(src.clone())
                            })?
                        {
                            solved.i_did_a_thing();
                        }
                    }
                }
                if size.is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                input.solve(sleigh, execution, solved)?;
            }

            Self::Op(ExprUnaryOp {
                location: src,
                output_size: size,
                op: Unary::Zext | Unary::Sext,
                input: value,
            }) => {
                let error = || ExecutionError::VarSize(src.clone());
                //output size need to be bigger or eq to the value size
                modified |= size
                    .update_action(|size| {
                        size.set_min(value.size(sleigh, execution).min())
                    })
                    .ok_or_else(error)?;
                //and vise-versa
                modified |= value
                    .size_mut(sleigh, execution)
                    .update_action(|size| size.set_max(size.max()))
                    .ok_or_else(error)?;

                if size.is_undefined()
                    || value.size(sleigh, execution).is_undefined()
                {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                value.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location: src,
                output_size: size,
                op: Unary::SignTrunc,
                input: value,
            }) => {
                let error = || ExecutionError::VarSize(src.clone());
                //output size need to be smaller or equal then the value size
                modified |= size
                    .update_action(|size| {
                        size.set_max(value.size(sleigh, execution).min())
                    })
                    .ok_or_else(error)?;
                //and vise-versa
                modified |= value
                    .size_mut(sleigh, execution)
                    .update_action(|size| size.set_min(size.max()))
                    .ok_or_else(error)?;

                if size.is_undefined()
                    || value.size(sleigh, execution).is_undefined()
                {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                value.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location: src,
                output_size: size,
                op: Unary::Float2Float | Unary::Int2Float,
                input,
            }) => {
                //input and output can have any size
                if size.is_undefined()
                    || input.size(sleigh, execution).is_undefined()
                {
                    solved.iam_not_finished_location(src, file!(), line!());
                }
                input.solve(sleigh, execution, solved)?;
            }
            Self::Op(ExprUnaryOp {
                location: src,
                output_size: size,
                op: Unary::FloatNan,
                input,
            }) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(src, file!(), line!());
                }
                input.solve(sleigh, execution, solved)?;
            }
            Self::UserCall(UserCall {
                output_size: size,
                params,
                location,
                ..
            }) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(
                        &location,
                        file!(),
                        line!(),
                    );
                }
                params
                    .iter_mut()
                    .map(|param| param.solve(sleigh, execution, solved))
                    .collect::<Result<_, _>>()?;
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
        constructor: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Value(value) => value.size_mut(sleigh, constructor),
            Self::DeReference(_, deref, _) => {
                Box::new(FieldSizeMutRef::from(&mut deref.size))
            }
            Self::Truncate(_, trunc, _) => {
                Box::new(FieldSizeMutRef::from(&mut trunc.size))
            }
            Self::Reference(x) => Box::new(FieldSizeMutRef::from(&mut x.len)),
            Self::Op(x) => Box::new(FieldSizeMutRef::from(&mut x.output_size)),
            Self::UserCall(x) => {
                Box::new(FieldSizeMutRef::from(&mut x.output_size))
            }
            Self::New(_x) => todo!(),
            Self::CPool(_x) => todo!(),
        }
    }
    pub fn size(&self, sleigh: &Sleigh, constructor: &Execution) -> FieldSize {
        match self {
            Self::Value(value) => value.size(sleigh, constructor),
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

impl ReadValue {
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
        constructor: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Int(x) => Box::new(FieldSizeMutRef::from(&mut x.size)),
            Self::DisVar(x) => Box::new(FieldSizeMutRef::from(&mut x.size)),
            Self::Context(_)
            | Self::Bitrange(_)
            | Self::InstStart(_)
            | Self::InstNext(_)
            | Self::TokenField(_) => Box::new(FieldSizeMutOwned::from(
                self.size(sleigh, constructor),
            )),
            Self::Varnode(var) => Box::new(FieldSizeMutOwned::from(
                FieldSize::new_bytes(sleigh.varnode(var.id).len_bytes),
            )),
            Self::Table(x) => Box::new(sleigh.table(x.id)),
            Self::ExeVar(x) => Box::new(&constructor.variable(x.id).size),
        }
    }
    pub fn size(&self, sleigh: &Sleigh, constructor: &Execution) -> FieldSize {
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
            Self::ExeVar(x) => constructor.variable(x.id).size.get(),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        match self {
            //don't call table solve directly, let the main loop do it
            Self::Table(_) | Self::ExeVar(_) => (),
            // the len don't need solving
            Self::Context(_)
            | Self::Bitrange(_)
            | Self::TokenField(_)
            | Self::Varnode(_)
            | Self::InstStart(_)
            | Self::InstNext(_) => (),
            Self::Int(num) => {
                if num.size.is_undefined() {
                    solved.iam_not_finished_location(
                        &num.location,
                        file!(),
                        line!(),
                    )
                }
            }
            Self::DisVar(var) => {
                if var.size.is_undefined() {
                    solved.iam_not_finished_location(
                        &var.location,
                        file!(),
                        line!(),
                    )
                }
            }
        }
        Ok(())
    }
    pub fn convert(self) -> FinalReadValue {
        match self {
            Self::Int(x) => FinalReadValue::Int(x.convert()),
            Self::TokenField(x) => FinalReadValue::TokenField(x),
            Self::InstStart(x) => FinalReadValue::InstStart(x),
            Self::InstNext(x) => FinalReadValue::InstNext(x),
            Self::Varnode(x) => FinalReadValue::Varnode(x),
            Self::Context(x) => FinalReadValue::Context(x),
            Self::Bitrange(x) => FinalReadValue::Bitrange(x),
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
            len_bits: self.size.possible_value().unwrap(),
            number: self.number,
        }
    }

    pub(crate) fn new(location: Span, number: Number) -> ExprNumber {
        let size = FieldSize::default()
            .set_min(
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

impl ExprDisVar {
    pub fn convert(self) -> FinalExprDisVar {
        FinalExprDisVar {
            location: self.location,
            len_bits: self.size.possible_value().unwrap(),
            id: self.id,
        }
    }
}
