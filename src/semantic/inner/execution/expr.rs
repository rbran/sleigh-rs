use crate::semantic::inner::execution::FieldSizeIntersectIter;
use crate::semantic::inner::execution::FieldSizeMutRef;
use crate::semantic::inner::execution::FinalTruncate;
use crate::semantic::inner::token::TokenField;
use crate::semantic::inner::varnode::Context;
use crate::semantic::inner::Solved;
use crate::semantic::varnode::Bitrange;
use crate::semantic::varnode::Varnode;
use crate::semantic::GlobalElement;
use crate::semantic::GlobalReference;
use crate::semantic::InstNext;
use crate::semantic::InstStart;
use crate::Number;
use crate::NumberNonZeroUnsigned;
use crate::NumberUnsigned;
use crate::Span;
use std::rc::Rc;

use crate::semantic;
use crate::semantic::execution::{Binary, ExecutionError};
use crate::semantic::inner::pcode_macro::Parameter;
use crate::semantic::inner::{disassembly, FieldSize, SolverStatus, Table};

use super::FieldSizeMutOwned;
use super::{
    AddrDereference, ExecutionExport, FieldSizeMut, Truncate, Unary, UserCall,
    Variable, FIELD_SIZE_BOOL,
};

pub type FinalExpr = semantic::execution::Expr;
#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(Span, FieldSize, Binary, Box<Expr>, Box<Expr>),
}
impl Expr {
    pub fn new_value(value: ExprElement) -> Self {
        Self::Value(value)
    }
    pub fn new_op(src: Span, op: Binary, left: Expr, mut right: Expr) -> Self {
        let size = match op {
            Binary::Lsl | Binary::Lsr | Binary::Asr => {
                //rotation is unlikelly to rotate more then 128 bits,
                //so right element size only require 32bits max
                right.size_mut().update_action(|size| {
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
            _ => FieldSize::new_unsized(),
        };
        Self::Op(src, size, op, Box::new(left), Box::new(right))
    }
    pub fn src(&self) -> &Span {
        match self {
            Expr::Value(value) => value.src(),
            Expr::Op(src, _, _, _, _) => src,
        }
    }
    pub fn size(&self) -> FieldSize {
        match self {
            Expr::Value(value) => value.size(),
            Expr::Op(_, out_size, _, _, _) => *out_size,
        }
    }
    pub fn size_mut<'a>(&'a mut self) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Expr::Value(value) => value.size_mut(),
            Expr::Op(_, out_size, _, _, _) => {
                Box::new(FieldSizeMutRef::from(out_size))
            }
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let (self_moved, src, mut out_size, op, left, right) = match self {
            Expr::Value(value) => return value.solve(solved),
            self_moved @ Expr::Op(_, _, _, _, _) => {
                //TODO make this less akward and remove the unecessary deref
                //and recreation of Box's
                //akwardly move values from self, replacing with a dummy value
                let dummy_src = Span::File(crate::FileSpan {
                    start: crate::FileLocation {
                        file: Rc::from(std::path::Path::new("")),
                        line: 0,
                        column: 0,
                    },
                    end_line: 0,
                    end_column: 0,
                });
                let mut self_tmp =
                    Expr::Value(ExprElement::Value(ExprValue::Int(
                        dummy_src,
                        FieldSize::default(),
                        Number::Positive(0),
                    )));
                std::mem::swap(self_moved, &mut self_tmp);
                match self_tmp {
                    Expr::Op(src, out_size, op, left, right) => {
                        (self_moved, src, out_size, op, left, right)
                    }
                    _ => unreachable!(),
                }
            }
        };

        use ExprElement as Ele;
        use ExprValue as Value;

        let mut modified = false;
        //TODO make the moves and mut ref more elegant
        *self_moved = match (*left, op, *right) {
            //if two Integer, calculate it and replace self with the result.
            (
                Expr::Value(Ele::Value(Value::Int(src, _, left))),
                op,
                Expr::Value(Ele::Value(Value::Int(_, _, right))),
            ) => {
                solved.i_did_a_thing();
                solved.iam_not_finished_location(&src, file!(), line!());
                //TODO create an error if the value is too big, for now let the
                //unwrap so we can detect if ghidra uses value > u64
                let value = op
                    .execute(
                        left.as_unsigned().unwrap(),
                        right.as_unsigned().unwrap(),
                    )
                    .ok_or_else(|| {
                        //TODO better error
                        ExecutionError::InvalidExport
                    })?;
                //replace self with our new value
                Expr::Value(Ele::Value(Value::new_int(src, value.into())))
            }

            //convert some kinds of bit_and into bitrange if the value is a an
            //bitrange starting from 0 (eg: 0b1 0b11 0b111 0b1111 0b11111 etc)
            //and the output size is expected to reduce the size of the input
            //using the bitwise op.
            //eg: `reg0[0,1] = value & 1; reg1[0,2] = value & 3`
            (
                value,
                Binary::BitAnd,
                Expr::Value(Ele::Value(Value::Int(src, _, integer))),
            )
            | (
                Expr::Value(Ele::Value(Value::Int(src, _, integer))),
                Binary::BitAnd,
                value,
            ) if out_size
                .final_value()
                .map(|bits| {
                    bits.get()
                        == integer.as_unsigned().unwrap().count_ones().into()
                })
                .unwrap_or(false)
                && value
                    .size()
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
                solved.iam_not_finished_location(&src, file!(), line!());
                let size = out_size.final_value().unwrap();
                Expr::Value(ExprElement::Truncate(
                    src.clone(),
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
                Expr::Value(Ele::Value(Value::Int(_, _, lsb))),
            ) if out_size
                .final_value()
                .zip(value.size().final_value())
                .map(|(out_bits, val_bits)| (out_bits.get(), val_bits.get()))
                .map(|(out_bits, val_bits)| {
                    val_bits >= lsb.as_unsigned().unwrap()
                        && out_bits == (val_bits - lsb.as_unsigned().unwrap())
                })
                .unwrap_or(false) =>
            {
                solved.i_did_a_thing();
                solved.iam_not_finished_location(&src, file!(), line!());
                let size = out_size.final_value().unwrap();
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
                left.solve(solved)?;
                //NOTE right defaults to 32bits
                right.solve(solved)?;

                let error = || ExecutionError::VarSize(src.clone());
                modified |= [
                    &mut left.size_mut() as &mut dyn FieldSizeMut,
                    &mut FieldSizeMutRef::from(&mut out_size),
                ]
                .all_same_lenght()
                .ok_or_else(error)?;
                if out_size.is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                Expr::Op(src, out_size, op, Box::new(left), Box::new(right))
            }

            //left/right/output can have any size, they are all just `0` or `!=0`
            (mut left, Binary::And | Binary::Xor | Binary::Or, mut right) => {
                //left/right can have any lenght, so just try to solve the len
                //if possible, otherwise just ignore it
                left.solve(&mut Solved::default())?;
                right.solve(&mut Solved::default())?;
                if out_size.is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                Expr::Op(src, out_size, op, Box::new(left), Box::new(right))
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
                left.solve(solved)?;
                right.solve(solved)?;
                let error = || ExecutionError::VarSize(src.clone());
                modified |= [
                    &mut left.size_mut() as &mut dyn FieldSizeMut,
                    &mut right.size_mut(),
                    &mut FieldSizeMutRef::from(&mut out_size),
                ]
                .all_same_lenght()
                .ok_or_else(error)?;
                //TODO is that really right?
                //if the output have a possible size, make left/right also have
                if let Some(possible) = out_size.possible_value() {
                    let new_left = left.size().set_possible_value(possible);
                    let new_right = right.size().set_possible_value(possible);
                    if let Some((new_left, new_right)) = new_left.zip(new_right)
                    {
                        left.size_mut().set(new_left);
                        right.size_mut().set(new_right);
                    }
                }
                if out_size.is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                Expr::Op(src, out_size, op, Box::new(left), Box::new(right))
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
                left.solve(solved)?;
                right.solve(solved)?;
                //Both sides need to have the same number of bits.
                //output can have any size because is always 0/1
                let error = || ExecutionError::VarSize(src.clone());
                modified |= [
                    &mut left.size_mut() as &mut dyn FieldSizeMut,
                    &mut right.size_mut(),
                ]
                .all_same_lenght()
                .ok_or_else(error)?;
                if left.size().is_undefined() || right.size().is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                Expr::Op(src, out_size, op, Box::new(left), Box::new(right))
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
            Expr::Op(_, size, op, left, right) => FinalExpr::Op(
                size.possible_value().unwrap(),
                op,
                Box::new(left.convert()),
                Box::new(right.convert()),
            ),
        }
    }
}

pub type FinalReferencedValue = semantic::execution::ReferencedValue;
#[derive(Clone, Debug)]
pub enum ReferencedValue {
    //only if translate into varnode
    TokenField(GlobalReference<TokenField>),
    //InstStart(GlobalReference<InstStart>),
    //InstNext(GlobalReference<InstNext>),
    //Context(InputSource, Rc<Context>),
    Table(GlobalReference<Table>),
    //Param(InputSource, Rc<Parameter>),
}

impl ReferencedValue {
    pub fn src(&self) -> &Span {
        match self {
            Self::TokenField(x) => x.location(),
            Self::Table(x) => x.location(),
            //Self::InstStart(x) => x.location(),
            //Self::InstNext(x) => x.location(),
        }
    }
    pub fn convert(self) -> FinalReferencedValue {
        match self {
            Self::TokenField(value) => {
                FinalReferencedValue::TokenField(value.convert_reference())
            }
            Self::Table(value) => {
                FinalReferencedValue::Table(value.convert_reference())
            } //Self::Param(_, value) => FinalExprValue::Param(value.convert()),
            //Self::InstStart(x) => FinalReferencedValue::InstStart(x),
            //Self::InstNext(x) => FinalReferencedValue::InstNext(x),
        }
    }
}

pub type FinalExprElement = semantic::execution::ExprElement;
#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ExprValue),
    UserCall(FieldSize, UserCall),
    Reference(Span, FieldSize, ReferencedValue),
    DeReference(Span, AddrDereference, Box<Expr>),
    Truncate(Span, Truncate, Box<Expr>),
    Op(Span, FieldSize, Unary, Box<Expr>),
    //TODO allow full expr??
    New(Span, Box<Expr>, Option<Box<Expr>>),
    //TODO allow full expr??
    CPool(Span, Vec<Expr>),
}
impl ExprElement {
    pub fn new_truncate(src: Span, truncate: Truncate, expr: Expr) -> Self {
        Self::Truncate(src, truncate, Box::new(expr))
    }
    pub fn new_deref(
        src: Span,
        deref: AddrDereference,
        mut addr: Expr,
    ) -> Self {
        //addr expr, need to be the space_addr size
        addr.size_mut()
            .set(FieldSize::new_bytes(deref.space.element().addr_bytes()));
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
        Self::Op(src, size, op, Box::new(expr))
    }
    pub fn src(&self) -> &Span {
        match self {
            Self::Value(value) => value.src(),
            Self::UserCall(_, call) => call.src(),
            Self::Op(src, _, _, _)
            | Self::DeReference(src, _, _)
            | Self::Truncate(src, _, _)
            | Self::Reference(src, _, _)
            | Self::New(src, _, _)
            | Self::CPool(src, _) => src,
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let mut modified = false;
        match self {
            Self::Value(value) => value.solve(solved)?,
            Self::Reference(src, _size, ReferencedValue::Table(table)) => {
                let _error = || ExecutionError::VarSize(src.clone());
                //if the table reference is return space references
                //(like varnode) update the output size with the addr size
                match table.element().export().borrow().as_ref().unwrap(/*TODO*/)
                {
                    ExecutionExport::Reference(_) => (/*TODO*/),
                    ExecutionExport::None
                    | ExecutionExport::Const(_)
                    | ExecutionExport::Value(_)
                    | ExecutionExport::Multiple(_) => (/*TODO*/),
                }
            }
            Self::Reference(_, _, ReferencedValue::TokenField(_)) => {
                (/*TODO*/)
            }
            //Self::Reference(
            //    _,
            //    _,
            //    ReferencedValue::InstStart(_) | ReferencedValue::InstNext(_),
            //) => (),
            Self::Truncate(src, Truncate { size, lsb }, value) => {
                let error = || ExecutionError::VarSize(src.clone());
                //value min size need to be lsb + size, if size is unknown
                //use 1 because size will never be zero
                let lsb_size =
                    size.final_value().unwrap_or(1.try_into().unwrap());
                modified |= value
                    .size_mut()
                    .update_action(|size| {
                        size.set_min(
                            (*lsb + lsb_size.get()).try_into().unwrap(),
                        )
                    })
                    .ok_or_else(error)?;
                //truncate output size can't be bigger than the value size
                modified |= size
                    .update_action(|size| size.set_max(value.size().max()))
                    .ok_or_else(error)?;
                //if the value size is known, the size of truncate output size
                //is optional, because MsbTruncate auto adjust to the value size
                //so we are are not finished only, if the truncate size is
                //unknown and value size is also unknown
                if !value.size().is_final() && !size.is_final() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                value.solve(solved)?;
            }
            Self::DeReference(_, deref, value) => {
                deref.solve(solved);
                value.solve(solved)?;
            }
            Self::Op(
                src,
                size,
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
            ) => {
                let error = || ExecutionError::VarSize(src.clone());
                //if one can be min size, both can be
                if size.possible_min() || input.size().possible_min() {
                    let set_min =
                        |size: FieldSize| Some(size.set_possible_min());
                    modified |= size.update_action(set_min).unwrap();
                    modified |=
                        input.size_mut().update_action(set_min).unwrap();
                }
                //the input and output have the same number of bits
                {
                    let mut input_size = input.size_mut();
                    let mut size = FieldSizeMutRef::from(size);
                    let mut lens: [&mut dyn FieldSizeMut; 2] =
                        [&mut *input_size, &mut size];
                    modified |= lens.all_same_lenght().ok_or_else(error)?;
                }
                if input.size().is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                input.solve(solved)?;
            }
            Self::Op(src, size, Unary::Popcount, input) => {
                let error = || ExecutionError::VarSize(src.clone());
                //the output min size is: log2(bit_len(input) + 1)
                if let Some(input_num_bits) = input.size().final_value() {
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
                            .ok_or_else(error)?
                        {
                            solved.i_did_a_thing();
                        }
                    }
                }
                if size.is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                input.solve(solved)?;
            }

            Self::Op(src, size, Unary::Zext | Unary::Sext, value) => {
                let error = || ExecutionError::VarSize(src.clone());
                //output size need to be bigger or eq to the value size
                modified |= size
                    .update_action(|size| size.set_min(value.size().min()))
                    .ok_or_else(error)?;
                //and vise-versa
                modified |= value
                    .size_mut()
                    .update_action(|size| size.set_max(size.max()))
                    .ok_or_else(error)?;

                if size.is_undefined() || value.size().is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                value.solve(solved)?;
            }
            Self::Op(src, size, Unary::SignTrunc, value) => {
                let error = || ExecutionError::VarSize(src.clone());
                //output size need to be smaller or equal then the value size
                modified |= size
                    .update_action(|size| size.set_max(value.size().min()))
                    .ok_or_else(error)?;
                //and vise-versa
                modified |= value
                    .size_mut()
                    .update_action(|size| size.set_min(size.max()))
                    .ok_or_else(error)?;

                if size.is_undefined() || value.size().is_undefined() {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
                value.solve(solved)?;
            }
            Self::Op(
                src,
                size,
                Unary::Float2Float | Unary::Int2Float,
                input,
            ) => {
                //input and output can have any size
                if size.is_undefined() || input.size().is_undefined() {
                    solved.iam_not_finished_location(src, file!(), line!());
                }
                input.solve(solved)?;
            }
            Self::Op(src, size, Unary::FloatNan, input) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(src, file!(), line!());
                }
                input.solve(solved)?;
            }
            Self::UserCall(size, call) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(
                        call.src(),
                        file!(),
                        line!(),
                    );
                }
                call.params
                    .iter_mut()
                    .map(|param| param.solve(solved))
                    .collect::<Result<_, _>>()?;
            }
            Self::New(_, _, _) => todo!(),
            Self::CPool(_, _) => todo!(),
        }
        if modified {
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(self) -> FinalExprElement {
        match self {
            Self::Value(value) => FinalExprElement::Value(value.convert()),
            Self::Reference(_, size, value) => FinalExprElement::Reference(
                size.possible_value().unwrap(),
                value.convert(),
            ),
            Self::DeReference(_, deref, value) => {
                let size = deref.size.final_value().unwrap();
                FinalExprElement::Op(
                    size,
                    super::FinalUnary::Dereference(deref.convert()),
                    Box::new(value.convert()),
                )
            }
            Self::Truncate(_, truncate, value) => {
                //if the truncate size is unknown, the value size is used,
                //because Msb truncate auto adjust the output size
                if let Some(out_size) = truncate.size.final_value() {
                    FinalExprElement::Op(
                        out_size,
                        super::FinalUnary::Truncate(truncate.convert()),
                        Box::new(value.convert()),
                    )
                } else {
                    let size = value.size().final_value().unwrap();
                    let size =
                        NumberNonZeroUnsigned::new(size.get() - truncate.lsb)
                            .unwrap();
                    FinalExprElement::Op(
                        size,
                        super::FinalUnary::Truncate(FinalTruncate::new(
                            truncate.lsb,
                            size,
                        )),
                        Box::new(value.convert()),
                    )
                }
            }
            Self::Op(_src, size, unary, value) => FinalExprElement::Op(
                size.possible_value().unwrap(),
                unary.convert(),
                Box::new(value.convert()),
            ),
            Self::UserCall(_, call) => {
                FinalExprElement::UserCall(call.convert())
            }
            Self::New(_, param0, param1) => FinalExprElement::New(
                Box::new(param0.convert()),
                param1.map(|param| param.convert()).map(Box::new),
            ),
            Self::CPool(_, params) => FinalExprElement::CPool(
                params.into_iter().map(|x| x.convert()).collect(),
            ),
        }
    }
    //return the size of the value with unary ops apply to it
    pub fn size_mut<'a>(&'a mut self) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Value(value) => value.size_mut(),
            Self::DeReference(_, deref, _) => {
                Box::new(FieldSizeMutRef::from(&mut deref.size))
            }
            Self::Truncate(_, trunc, _) => {
                Box::new(FieldSizeMutRef::from(&mut trunc.size))
            }
            Self::Reference(_, size, _)
            | Self::Op(_, size, _, _)
            | Self::UserCall(size, _) => Box::new(FieldSizeMutRef::from(size)),
            Self::New(_, _, _) => todo!(),
            Self::CPool(_, _) => todo!(),
        }
    }
    pub fn size(&self) -> FieldSize {
        match self {
            Self::Value(value) => value.size(),
            Self::DeReference(_, deref, _) => deref.size,
            Self::Truncate(_, trunc, _) => trunc.size,
            Self::Reference(_, size, _) => *size,
            Self::Op(_, size, _, _) => *size,
            Self::UserCall(size, _call) => *size,
            Self::New(_, _, _) => todo!(),
            Self::CPool(_, _) => todo!(),
        }
    }
}

pub type FinalExprValue = semantic::execution::ExprValue;
#[derive(Debug, Clone)]
pub enum ExprValue {
    Int(Span, FieldSize, Number),
    DisVar(Span, FieldSize, Rc<disassembly::Variable>),
    TokenField(FieldSize, GlobalReference<TokenField>),
    InstStart(FieldSize, GlobalReference<InstStart>),
    InstNext(FieldSize, GlobalReference<InstNext>),
    Varnode(GlobalReference<Varnode>),
    //context may be extend (like zext and assembly) based on the context
    Context(FieldSize, GlobalReference<Context>),
    Bitrange(FieldSize, GlobalReference<Bitrange>),
    Table(GlobalReference<Table>),
    ExeVar(Span, Rc<Variable>),
    Param(Span, Rc<Parameter>),
}

impl ExprValue {
    pub fn new_int(src: Span, value: Number) -> Self {
        let value = match value {
            Number::Positive(value) => value,
            Number::Negative(_) => todo!(),
        };
        let new_size = NumberUnsigned::BITS - value.leading_zeros();
        //if result is 0, this is 1 bit
        let new_size =
            NumberNonZeroUnsigned::new(new_size.max(1).into()).unwrap();
        let new_size = FieldSize::new_unsized()
            .set_possible_min()
            .set_min(new_size)
            .unwrap();
        Self::Int(src, new_size, Number::Positive(value))
    }
    pub fn new_token_field(
        src: Span,
        value: &GlobalElement<TokenField>,
    ) -> Self {
        Self::TokenField(
            value.exec_value_len().set_possible_min(),
            GlobalReference::from_element(value, src),
        )
    }
    pub fn new_inst_start(
        src: Span,
        len: FieldSize,
        value: &GlobalElement<InstStart>,
    ) -> Self {
        Self::InstStart(len, value.reference_from(src))
    }
    pub fn new_inst_next(
        src: Span,
        len: FieldSize,
        value: &GlobalElement<InstNext>,
    ) -> Self {
        Self::InstNext(len, value.reference_from(src))
    }
    pub fn new_varnode(src: Span, value: &GlobalElement<Varnode>) -> Self {
        Self::Varnode(GlobalReference::from_element(value, src))
    }
    pub fn new_context(src: Span, value: &GlobalElement<Context>) -> Self {
        Self::Context(
            value.exec_out_value_bits(),
            GlobalReference::from_element(value, src),
        )
    }
    pub fn new_bitrange(src: Span, value: &GlobalElement<Bitrange>) -> Self {
        let size = FieldSize::new_unsized()
            .set_possible_min()
            .set_min(value.range.len_bits())
            .unwrap()
            .set_possible_min();
        Self::Bitrange(size, GlobalReference::from_element(value, src))
    }
    pub fn src(&self) -> &Span {
        match self {
            Self::Int(src, _, _)
            | Self::DisVar(src, _, _)
            | Self::ExeVar(src, _)
            | Self::Param(src, _) => src,
            Self::TokenField(_, x) => x.location(),
            Self::Varnode(x) => x.location(),
            Self::Context(_, x) => x.location(),
            Self::Bitrange(_, x) => x.location(),
            Self::Table(x) => x.location(),
            Self::InstStart(_, x) => x.location(),
            Self::InstNext(_, x) => x.location(),
        }
    }
    pub fn size_mut<'a>(&'a mut self) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Int(_, size, _)
            | Self::DisVar(_, size, _)
            | Self::Context(size, _)
            | Self::Bitrange(size, _)
            | Self::InstStart(size, _)
            | Self::InstNext(size, _)
            | Self::TokenField(size, _) => {
                Box::new(FieldSizeMutRef::from(size))
            }
            Self::Varnode(var) => Box::new(FieldSizeMutOwned::from(
                FieldSize::new_bytes(var.element().len_bytes),
            )),
            Self::Table(value) => Box::new(value.element()),
            Self::ExeVar(_, value) => Box::new(value.len()),
            Self::Param(_, value) => Box::new(value.size()),
        }
    }
    pub fn size(&self) -> FieldSize {
        match self {
            Self::Int(_, size, _)
            | Self::DisVar(_, size, _)
            | Self::Context(size, _)
            | Self::Bitrange(size, _)
            | Self::InstStart(size, _)
            | Self::InstNext(size, _)
            | Self::TokenField(size, _) => *size,
            Self::Varnode(var) => FieldSize::new_bytes(var.element().len_bytes),
            Self::Table(value) => *value
                .element()
                .export()
                .borrow()
                .as_ref()
                .unwrap()
                .size()
                .unwrap(),
            Self::ExeVar(_, value) => value.len().get(),
            Self::Param(_, value) => value.size().get(),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        match self {
            Self::Varnode(_) | Self::Param(_, _) => (),
            //don't call table solve directly, let the main loop do it
            Self::Table(_) => (),
            Self::ExeVar(_, var) => var.solve(solved)?,
            Self::Int(src, size, _) | Self::DisVar(src, size, _) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(src, file!(), line!())
                }
            }
            Self::TokenField(size, ass) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(
                        ass.location(),
                        file!(),
                        line!(),
                    );
                }
            }
            Self::Context(size, x) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(
                        x.location(),
                        file!(),
                        line!(),
                    )
                }
            }
            Self::Bitrange(size, x) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(
                        x.location(),
                        file!(),
                        line!(),
                    )
                }
            }
            Self::InstStart(size, x) => {
                if !size.is_final() {
                    solved.iam_not_finished_location(
                        x.location(),
                        file!(),
                        line!(),
                    )
                }
            }
            Self::InstNext(size, x) => {
                if !size.is_final() {
                    solved.iam_not_finished_location(
                        x.location(),
                        file!(),
                        line!(),
                    )
                }
            }
        }
        Ok(())
    }
    pub fn convert(self) -> FinalExprValue {
        match self {
            Self::Int(_, _, value) => FinalExprValue::Int(value),
            Self::DisVar(_, _, value) => {
                FinalExprValue::DisVar(value.convert())
            }
            Self::TokenField(_, value) => {
                FinalExprValue::TokenField(value.convert_reference())
            }
            Self::Varnode(value) => FinalExprValue::Varnode(value),
            Self::Context(_, value) => {
                FinalExprValue::Context(value.convert_reference())
            }
            Self::Bitrange(_, value) => FinalExprValue::Bitrange(value),
            Self::Table(value) => {
                FinalExprValue::Table(value.convert_reference())
            }
            Self::ExeVar(_, value) => FinalExprValue::ExeVar(value.convert()),
            //Self::Param(_, value) => FinalExprValue::Param(value.convert()),
            Self::Param(_, value) => FinalExprValue::ExeVar(value.convert()),
            Self::InstStart(_, x) => FinalExprValue::InstStart(x),
            Self::InstNext(_, x) => FinalExprValue::InstNext(x),
        }
    }
}
