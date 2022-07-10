use crate::semantic::inner::execution::FinalTruncate;
use crate::semantic::inner::FieldSizeCell;
use crate::semantic::inner::FieldSizeMut;
use crate::semantic::inner::FIELD_SIZE_BOOL;
use std::rc::Rc;

use crate::base::{IntTypeU, NonZeroTypeU};
use crate::semantic;
use crate::semantic::assembly;
use crate::semantic::execution::{Binary, ExecutionError};
use crate::semantic::inner::pcode_macro::Parameter;
use crate::semantic::inner::{disassembly, FieldSize, SolverStatus, Table};
use crate::{InputSource, Varnode};

use super::ExecutionExport;
use super::{AddrDereference, Truncate, Unary, UserCall, Variable};

pub type FinalExpr = semantic::execution::Expr;
#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(InputSource, FieldSize, Binary, Box<Expr>, Box<Expr>),
}
impl Expr {
    pub fn new_value(value: ExprElement) -> Self {
        Self::Value(value)
    }
    pub fn new_op(
        src: InputSource,
        op: Binary,
        left: Expr,
        mut right: Expr,
    ) -> Self {
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
    pub fn src(&self) -> &InputSource {
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
    pub fn size_mut<'a>(&'a mut self) -> FieldSizeCell<'a> {
        match self {
            Expr::Value(value) => value.size_mut(),
            Expr::Op(_, out_size, _, _, _) => out_size.into(),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let (src, mut out_size, op, mut left, mut right) = match self {
            Expr::Value(value) => return value.solve(solved),
            _ => {
                //TODO make this less akward and remove the unecessary deref
                //and recreation of boxes
                //akwardly move values from self, replacing with a dummy value
                let mut bureaucracy = Expr::Value(ExprElement::Value(
                    ExprValue::Int(self.src().clone(), FieldSize::default(), 0),
                ));
                std::mem::swap(self, &mut bureaucracy);
                match bureaucracy {
                    Expr::Op(src, out_size, op, left, right) => {
                        (src, out_size, op, left, right)
                    }
                    _ => unreachable!(),
                }
            }
        };

        left.solve(solved)?;
        right.solve(solved)?;

        use ExprElement as Ele;
        use ExprValue as Value;

        let mut modified = false;
        //TODO make the moves and mut ref more elegant
        *self = match (*left, op, *right) {
            //if two Integer, calculate it and replace self with the result.
            (
                Expr::Value(Ele::Value(Value::Int(src, _, left))),
                op,
                Expr::Value(Ele::Value(Value::Int(_, _, right))),
            ) => {
                solved.i_did_a_thing();
                solved.iam_not_finished_location(&src);
                let value = op.execute(left, right).ok_or_else(|| {
                    //TODO better error
                    ExecutionError::InvalidExport
                })?;
                let value = Value::new_int(src.clone(), value);
                //replace self with our new value
                Expr::Value(Ele::Value(value))
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
                .map(|bits| bits.get() == integer.count_ones().into())
                .unwrap_or(false)
                && value
                    .size()
                    .final_value()
                    .map(|bits| bits.get() >= integer.count_ones().into())
                    .unwrap_or(true) =>
            {
                solved.i_did_a_thing();
                solved.iam_not_finished_location(&src);
                let size = out_size.final_value().unwrap();
                Expr::Value(ExprElement::Truncate(
                    src.clone(),
                    Truncate::new(0, size),
                    Box::new(value),
                ))
            }

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
                .map(|(out, val)| {
                    val.get() >= lsb && out.get() == (val.get() - lsb)
                })
                .unwrap_or(false) =>
            {
                solved.i_did_a_thing();
                solved.iam_not_finished_location(&src);
                let size = out_size.final_value().unwrap();
                //take the value from self, and put on the new self
                //safe because the self is overwriten after
                Expr::Value(Ele::Truncate(
                    src.clone(),
                    Truncate::new(lsb, size),
                    Box::new(value),
                ))
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
                let error = || ExecutionError::VarSize(src.clone());
                modified |= FieldSize::all_same_size(&mut [
                    (&mut out_size).into(),
                    left.size_mut(),
                    right.size_mut(),
                ])
                .ok_or_else(error)?;
                if out_size.is_undefined() {
                    solved.iam_not_finished_location(&src)
                }
                Expr::Op(src, out_size, op, Box::new(left), Box::new(right))
            }

            //output is the same size in bits of the left (rotated) element
            //right (rotated value) can have any size
            (mut left, Binary::Lsl | Binary::Lsr | Binary::Asr, right) => {
                let error = || ExecutionError::VarSize(src.clone());
                modified |= FieldSize::all_same_size(&mut [
                    (&mut out_size).into(),
                    left.size_mut(),
                ])
                .ok_or_else(error)?;
                if out_size.is_undefined() {
                    solved.iam_not_finished_location(&src);
                }
                Expr::Op(src, out_size, op, Box::new(left), Box::new(right))
            }

            //left and right can have any size, output can have any size because
            //is always value false/true, 0/1
            (left, Binary::And | Binary::Xor | Binary::Or, right) => {
                if out_size.is_undefined() {
                    solved.iam_not_finished_location(&src);
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
                //Both sides need to have the same number of bits.
                //output can have any size because is always 0/1
                let error = || ExecutionError::VarSize(src.clone());
                modified |= FieldSize::all_same_size(&mut [
                    left.size_mut(),
                    right.size_mut(),
                ])
                .ok_or_else(error)?;
                if left.size().is_undefined() || right.size().is_undefined() {
                    solved.iam_not_finished_location(&src);
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
    Assembly(InputSource, Rc<assembly::Assembly>),
    //Varnode(InputSource, Rc<Varnode>),
    Table(InputSource, Rc<Table>),
    //Param(InputSource, Rc<Parameter>),
}

impl ReferencedValue {
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Assembly(src, _)
            //| Self::Varnode(src, _)
            | Self::Table(src, _) => src,
        }
    }
    pub fn convert(self) -> FinalReferencedValue {
        match self {
            Self::Assembly(_, value) => FinalReferencedValue::Assembly(value),
            //Self::Varnode(_, _value) => todo!(),
            Self::Table(_, value) => {
                FinalReferencedValue::Table(value.convert())
            } //Self::Param(_, value) => FinalExprValue::Param(value.convert()),
        }
    }
}

pub type FinalExprElement = semantic::execution::ExprElement;
#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ExprValue),
    UserCall(FieldSize, UserCall),
    Reference(InputSource, FieldSize, ReferencedValue),
    DeReference(InputSource, AddrDereference, Box<Expr>),
    Truncate(InputSource, Truncate, Box<Expr>),
    Op(InputSource, FieldSize, Unary, Box<Expr>),
    //TODO allow full expr??
    New(InputSource, Box<Expr>, Option<Box<Expr>>),
    //TODO allow full expr??
    CPool(InputSource, Vec<Expr>),
}
impl ExprElement {
    pub fn new_truncate(
        src: InputSource,
        truncate: Truncate,
        expr: Expr,
    ) -> Self {
        Self::Truncate(src, truncate, Box::new(expr))
    }
    pub fn new_deref(
        src: InputSource,
        deref: AddrDereference,
        mut addr: Expr,
    ) -> Self {
        //addr expr, need to be the space_addr size
        addr.size_mut().set(deref.space.memory().addr_size());
        Self::DeReference(src, deref, Box::new(addr))
    }
    pub fn new_op(src: InputSource, mut op: Unary, expr: Expr) -> Self {
        let size = match &mut op {
            Unary::FloatNan => {
                //the output can be one bit (true/false)
                FieldSize::new_unsized().set_possible_min()
            }
            _ => FieldSize::new_unsized(),
        };
        Self::Op(src, size, op, Box::new(expr))
    }
    pub fn src(&self) -> &InputSource {
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
            Self::Reference(src, size, ReferencedValue::Table(_src, table)) => {
                let error = || ExecutionError::VarSize(src.clone());
                //if the table reference is return space references
                //(like varnode) update the output size with the addr size
                match table.export().borrow().as_ref().unwrap(/*TODO*/) {
                    ExecutionExport::Reference(_, space) => {
                        modified |= size
                            .update_action(|size| {
                                size.intersection(space.memory().addr_size())
                            })
                            .ok_or_else(error)?;
                    }
                    ExecutionExport::None
                    | ExecutionExport::DissasemblyValue(_)
                    | ExecutionExport::Value(_)
                    | ExecutionExport::Multiple(_) => (/*TODO*/),
                }
            }
            Self::Reference(_, _, ReferencedValue::Assembly(_, _)) => {
                (/*TODO*/)
            }
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
                    solved.iam_not_finished_location(&src);
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
                //the input and output have the same number of bits
                modified |= FieldSize::all_same_size(&mut [
                    size.into(),
                    input.size_mut(),
                ])
                .ok_or_else(error)?;
                //if one can be min size, both can be
                if size.possible_min() || input.size().possible_min() {
                    let set_min =
                        |size: FieldSize| Some(size.set_possible_min());
                    modified |= size.update_action(set_min).unwrap();
                    modified |=
                        input.size_mut().update_action(set_min).unwrap();
                }
                if size.is_undefined() {
                    solved.iam_not_finished_location(src);
                }
                input.solve(solved)?;
            }
            Self::Op(src, size, Unary::Popcount, input) => {
                let error = || ExecutionError::VarSize(src.clone());
                //the output min size is: log2(bit_len(input) + 1)
                if let Some(input_num_bits) = input.size().final_value() {
                    //equivalent to log2(bit_len(input) + 1)
                    let output_min =
                        IntTypeU::BITS - input_num_bits.get().leading_zeros();
                    if let Some(output_min) =
                        NonZeroTypeU::new(output_min.into())
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
                    solved.iam_not_finished_location(src);
                }
                input.solve(solved)?;
            }

            Self::Op(
                src,
                size,
                Unary::Zext | Unary::Sext | Unary::SignTrunc,
                input,
            ) => {
                let error = || ExecutionError::VarSize(src.clone());
                //input size need to be bigger or eq to the input size
                modified |= size
                    .update_action(|size| size.set_min(input.size().min()))
                    .ok_or_else(error)?;
                //and vise-versa
                modified |= input
                    .size_mut()
                    .update_action(|size| size.set_max(size.max()))
                    .ok_or_else(error)?;

                if size.is_undefined() || input.size().is_undefined() {
                    solved.iam_not_finished_location(src);
                }
                input.solve(solved)?;
            }
            Self::Op(
                src,
                size,
                Unary::Float2Float | Unary::Int2Float,
                input,
            ) => {
                //input and output can have any size
                if size.is_undefined() || input.size().is_undefined() {
                    solved.iam_not_finished_location(src);
                }
                input.solve(solved)?;
            }
            Self::Op(src, size, Unary::FloatNan, input) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(src);
                }
                input.solve(solved)?;
            }
            Self::UserCall(size, call) => {
                if size.is_undefined() {
                    solved.iam_not_finished_location(call.src());
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
                        NonZeroTypeU::new(size.get() - truncate.lsb).unwrap();
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
            Self::CPool(_, mut params) => FinalExprElement::CPool(
                params.drain(..).map(|x| x.convert()).collect(),
            ),
        }
    }
    //return the size of the value with unary ops apply to it
    pub fn size_mut<'a>(&'a mut self) -> FieldSizeCell<'a> {
        match self {
            Self::Value(value) => value.size_mut(),
            Self::DeReference(_, deref, _) => (&mut deref.size).into(),
            Self::Truncate(_, trunc, _) => (&mut trunc.size).into(),
            Self::Reference(_, size, _)
            | Self::Op(_, size, _, _)
            | Self::UserCall(size, _) => size.into(),
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
    Int(InputSource, FieldSize, IntTypeU),
    DisVar(InputSource, FieldSize, Rc<disassembly::Variable>),
    Assembly(InputSource, FieldSize, Rc<assembly::Assembly>),
    Varnode(InputSource, Rc<Varnode>),
    //context may be extend (like zext and assembly) based on the context
    Context(InputSource, FieldSize, Rc<Varnode>),
    BitRange(InputSource, FieldSize, Rc<Varnode>),
    Table(InputSource, Rc<Table>),
    ExeVar(InputSource, Rc<Variable>),
    Param(InputSource, Rc<Parameter>),
}

impl ExprValue {
    pub fn new_int(src: InputSource, value: IntTypeU) -> Self {
        let new_size = IntTypeU::BITS - value.leading_zeros();
        //if result is 0, this is 1 bit
        let new_size = NonZeroTypeU::new(new_size.max(1).into()).unwrap();
        let new_size = FieldSize::new_unsized()
            .set_possible_min()
            .set_min(new_size)
            .unwrap();
        Self::Int(src, new_size, value)
    }
    pub fn new_assembly(
        src: InputSource,
        value: Rc<assembly::Assembly>,
    ) -> Self {
        Self::Assembly(src, value.value_size(), value)
    }
    pub fn new_varnode(src: InputSource, value: Rc<Varnode>) -> Self {
        match &value.varnode_type {
            semantic::varnode::VarnodeType::Memory(_) => {
                Self::Varnode(src, value)
            }
            semantic::varnode::VarnodeType::BitRange(x) => {
                let size = FieldSize::new_unsized()
                    .set_min(x.value_bits())
                    .unwrap()
                    .set_possible_min();
                Self::BitRange(src, size, value)
            }
            semantic::varnode::VarnodeType::Context(x) => {
                //if context meaning translate into regular varnode
                if x.attach
                    .borrow()
                    .as_ref()
                    .map(|meaning| meaning.is_variable())
                    .unwrap_or(false)
                {
                    Self::Varnode(src, value)
                } else {
                    let size = FieldSize::new_unsized()
                        .set_min(x.value_bits())
                        .unwrap()
                        .set_possible_min();
                    Self::Context(src, size, value)
                }
            }
        }
    }
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Int(src, _, _)
            | Self::DisVar(src, _, _)
            | Self::Assembly(src, _, _)
            | Self::Varnode(src, _)
            | Self::Context(src, _, _)
            | Self::BitRange(src, _, _)
            | Self::Table(src, _)
            | Self::ExeVar(src, _)
            | Self::Param(src, _) => src,
        }
    }
    pub fn size_mut<'a>(&'a mut self) -> FieldSizeCell<'a> {
        match self {
            Self::Int(_, size, _)
            | Self::DisVar(_, size, _)
            | Self::Context(_, size, _)
            | Self::BitRange(_, size, _)
            | Self::Assembly(_, size, _) => size.into(),
            Self::Varnode(_, var) => {
                FieldSize::new_bits(var.value_bits()).into()
            }
            Self::Table(_, value) => value.export().into(),
            Self::ExeVar(_, value) => value.size().into(),
            Self::Param(_, value) => value.size().into(),
        }
    }
    pub fn size(&self) -> FieldSize {
        match self {
            Self::Int(_, size, _)
            | Self::DisVar(_, size, _)
            | Self::Context(_, size, _)
            | Self::BitRange(_, size, _)
            | Self::Assembly(_, size, _) => *size,
            Self::Varnode(_, var) => FieldSize::new_bits(var.value_bits()),
            Self::Table(_, value) => {
                *value.export().borrow().as_ref().unwrap().size().unwrap()
            }
            Self::ExeVar(_, value) => value.size().get(),
            Self::Param(_, value) => value.size().get(),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        match self {
            //int, context, assembly and bitrange have the possible min, so it
            //doesn't block the solve loop
            Self::Int(_, _, _)
            | Self::DisVar(_, _, _)
            | Self::Varnode(_, _)
            | Self::Context(_, _, _)
            | Self::BitRange(_, _, _)
            | Self::Param(_, _) => Ok(()),
            //inst_start/inst_next could be updated externally
            Self::Assembly(src, size, ass) => {
                if size
                    .update_action(|size| size.intersection(ass.value_size()))
                    .ok_or_else(|| ExecutionError::VarSize(src.clone()))?
                {
                    solved.i_did_a_thing();
                }
                Ok(())
            }
            //don't call table solve directly, let the main loop do it
            Self::Table(_, _) => Ok(()),
            Self::ExeVar(_, var) => var.solve(solved),
        }
    }
    pub fn convert(self) -> FinalExprValue {
        match self {
            Self::Int(_, _, value) => FinalExprValue::Int(value),
            Self::DisVar(_, _, value) => {
                FinalExprValue::DisVar(value.convert())
            }
            Self::Assembly(_, _, value) => FinalExprValue::Assembly(value),
            Self::Varnode(_, value) => FinalExprValue::Varnode(value),
            Self::Context(_, _, value) => FinalExprValue::Varnode(value),
            Self::BitRange(_, _, value) => FinalExprValue::Varnode(value),
            Self::Table(_, value) => FinalExprValue::Table(value.convert()),
            Self::ExeVar(_, value) => FinalExprValue::ExeVar(value.convert()),
            //Self::Param(_, value) => FinalExprValue::Param(value.convert()),
            Self::Param(_, value) => FinalExprValue::ExeVar(value.convert()),
        }
    }
}
