use crate::semantic::execution::{Export as FinalExport, ExportConst};
use crate::semantic::inner::pattern::Pattern;
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::semantic::table::ExecutionExport;
use crate::syntax::block::execution::op::ByteRangeLsb;
use crate::{
    ExecutionError, Number, NumberNonZeroUnsigned, Span, VarSizeError,
};

use super::{
    Execution, Expr, ExprElement, ExprNumber, ExprValue, FieldSize,
    FieldSizeMut, MemoryLocation, ReadScope, Variable,
};

/// Changes allowed:
/// Const -> Value -> Reference -> Multiple
#[derive(Clone, Copy, Debug, Default)]
pub enum ExportLen {
    //don't return
    #[default]
    None,
    // value that is known at Dissassembly time
    Const(FieldSize),
    // value that can be know at execution time
    Value(FieldSize),
    // References/registers and other mem locations, all with the same size
    Reference(FieldSize),
    // multiple source, can by any kind of return, value or address,
    // but all with the same size
    Multiple(FieldSize),
}

#[derive(Clone, Debug)]
pub enum Export {
    Const {
        len_bits: FieldSize,
        location: Span,
        export: ExportConst,
    },
    Value(Expr),
    Reference {
        addr: Expr,
        memory: MemoryLocation,
    },
}

impl ExportLen {
    pub fn export_nothing(&self) -> bool {
        matches!(self, Self::None)
    }
    pub fn size(&self) -> Option<&FieldSize> {
        match self {
            Self::None => None,
            Self::Const(size)
            | Self::Value(size)
            | Self::Reference(size)
            | Self::Multiple(size) => Some(size),
        }
    }
    pub fn size_mut(&mut self) -> Option<&mut FieldSize> {
        match self {
            Self::None => None,
            Self::Const(size)
            | Self::Value(size)
            | Self::Reference(size)
            | Self::Multiple(size) => Some(size),
        }
    }
    fn set_len(self, other_len: FieldSize) -> Option<Self> {
        match self {
            Self::None => None,
            Self::Const(len) => Some(Self::Const(len.intersection(other_len)?)),
            Self::Value(len) => Some(Self::Value(len.intersection(other_len)?)),
            Self::Reference(len) => {
                Some(Self::Reference(len.intersection(other_len)?))
            }
            Self::Multiple(len) => {
                Some(Self::Multiple(len.intersection(other_len)?))
            }
        }
    }
    pub fn combine(self, other: Self) -> Option<Self> {
        match (self, other) {
            //if both return nothing, the result is to return nothing
            (Self::None, Self::None) => Some(Self::None),
            (Self::None, _) | (_, Self::None) => None,
            // const can be transformed into anything else, as long its the
            // same len
            (Self::Const(len_a), type_b) | (type_b, Self::Const(len_a)) => {
                type_b.set_len(len_a)
            }
            // Value can be anything but Const
            (Self::Value(len_a), type_b) | (type_b, Self::Value(len_a)) => {
                type_b.set_len(len_a)
            }
            (Self::Reference(len_a), type_b)
            | (type_b, Self::Reference(len_a)) => type_b.set_len(len_a),
            (Self::Multiple(len_a), Self::Multiple(len_b)) => {
                Some(Self::Multiple(len_a.intersection(len_b)?))
            }
        }
    }
    //TODO from Option into a Result
    pub fn final_size(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::None => None,
            Self::Const(len) => Some(len.possible_value().unwrap()),
            Self::Value(len) => Some(len.possible_value().unwrap()),
            Self::Reference(len) => Some(len.possible_value().unwrap()),
            Self::Multiple(len) => Some(len.possible_value().unwrap()),
        }
    }
    pub fn convert(self) -> ExecutionExport {
        match self {
            ExportLen::None => ExecutionExport::None,
            ExportLen::Const(x) => {
                ExecutionExport::Const(x.possible_value().unwrap())
            }
            ExportLen::Value(x) => {
                ExecutionExport::Value(x.possible_value().unwrap())
            }
            ExportLen::Reference(x) => {
                ExecutionExport::Reference(x.possible_value().unwrap())
            }
            ExportLen::Multiple(x) => {
                ExecutionExport::Multiple(x.possible_value().unwrap())
            }
        }
    }
}

impl ExportConst {
    pub fn from_read_scope(
        sleigh: &Sleigh,
        read_scope: ReadScope,
        location: Span,
    ) -> Result<Self, Box<ExecutionError>> {
        use ReadScope::*;
        match read_scope {
            ExeVar(var) => Ok(ExportConst::ExeVar(var)),
            DisVar(var) => Ok(ExportConst::DisVar(var)),
            TokenField(ass) => Ok(ExportConst::TokenField(ass)),
            Context(cont) => Ok(ExportConst::Context(cont)),
            InstStart => Ok(ExportConst::InstructionStart),
            Table(expr_table) => {
                let table = sleigh.table(expr_table);
                match *table.export.borrow() {
                    Some(ExportLen::Const(_) | ExportLen::Value(_)) => {
                        Ok(ExportConst::Table(expr_table))
                    }
                    //TODO more specific error
                    //a const can only export const or context
                    Some(_) => Err(Box::new(ExecutionError::InvalidRef(
                        location.clone(),
                    ))),
                    //to constructors are available yet, can't use this table
                    None => Err(Box::new(ExecutionError::InvalidRef(
                        location.clone(),
                    ))),
                }
            }
            x => todo!("Error export invalid const {:#?}", x),
        }
    }
}

impl Export {
    pub fn new_value(
        sleigh: &Sleigh,
        pattern: &Pattern,
        execution: &Execution,
        expr: Expr,
    ) -> Result<Self, Box<ExecutionError>> {
        match expr {
            //if the value is just an varnode, then is actually a reference
            Expr::Value(ExprElement::Value(ExprValue::Varnode(
                varnode_expr,
            ))) => {
                let varnode = sleigh.varnode(varnode_expr.id);
                let number = ExprNumber::new(
                    varnode_expr.location.clone(),
                    Number::Positive(varnode.address),
                );
                Export::new_reference(
                    sleigh,
                    pattern,
                    execution,
                    Expr::Value(ExprElement::Value(ExprValue::Int(number))),
                    MemoryLocation {
                        space: varnode.space,
                        size: FieldSize::new_bytes(varnode.len_bytes),
                        src: varnode_expr.location.clone(),
                    },
                )
            }
            expr => Ok(Self::Value(expr)),
        }
    }
    pub fn new_const(
        sleigh: &Sleigh,
        _pattern: &Pattern,
        read_scope: ReadScope,
        size: ByteRangeLsb,
        src: Span,
    ) -> Result<Self, Box<ExecutionError>> {
        let value =
            ExportConst::from_read_scope(sleigh, read_scope, src.clone())?;
        let size = NumberNonZeroUnsigned::new(size.value).unwrap(/*TODO*/);
        // TODO: if the value is a disassembly variable, we define it's len
        //if let ExportConst::DisVar(id) = value {
        //    let variable = &pattern.disassembly_variables[id.0];
        //    let value_type = variable
        //        .value_type
        //        .get()
        //        .set_len(size)
        //        .ok_or(ExecutionError::InvalidExport)?;
        //    variable.value_type.set(value_type);
        //}
        let size = FieldSize::new_bytes(size);
        Ok(Self::Const {
            len_bits: size,
            location: src,
            export: value,
        })
    }
    pub fn new_reference(
        sleigh: &Sleigh,
        _pattern: &Pattern,
        execution: &Execution,
        mut addr: Expr,
        memory: MemoryLocation,
    ) -> Result<Self, Box<ExecutionError>> {
        // TODO: if the addr is a single Disassembly variable, it affects
        // how it's printed
        //if let Expr::Value(ExprElement::Value(ExprValue::DisVar(variable))) =
        //    &addr
        //{
        //    let variable = &pattern.disassembly_variables[variable.id.0];
        //    let value_type = variable
        //        .value_type
        //        .get()
        //        .set_space(memory.space)
        //        .ok_or(ExecutionError::InvalidExport)?;
        //    variable.value_type.set(value_type);
        //}
        //addr expr is the addr to access the space, so it need to be space
        //addr size or smaller
        let space = sleigh.space(memory.space);
        let src = addr.src().clone();
        let modified = addr
            .size_mut(sleigh, &execution.vars)
            .update_action(|size| size.set_max_bytes(space.addr_bytes));

        let _ = modified.ok_or_else(|| VarSizeError::AddressTooBig {
            address_size: addr.size(sleigh, &execution.vars),
            space_bytes: space.addr_bytes,
            location: src,
        })?;
        Ok(Self::Reference { addr, memory })
    }
    pub fn return_type(
        &self,
        sleigh: &Sleigh,
        variables: &[Variable],
    ) -> ExportLen {
        match self {
            Export::Value(value) => {
                ExportLen::Value(value.size(sleigh, variables))
            }
            Export::Reference { addr: _, memory } => {
                ExportLen::Reference(memory.size)
            }
            Export::Const {
                len_bits: len,
                location: _,
                export: _,
            } => ExportLen::Const(*len),
        }
    }
    pub fn src(&self) -> &Span {
        match self {
            Self::Value(expr) | Export::Reference { addr: expr, .. } => {
                expr.src()
            }
            Self::Const { location, .. } => location,
        }
    }
    pub fn output_size(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> FieldSize {
        match self {
            Self::Value(expr) => expr.size(sleigh, &execution.vars),
            //TODO verify this
            Self::Reference { addr: _, memory } => memory.size,
            Self::Const { len_bits: len, .. } => *len,
        }
    }
    pub fn output_size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        variables: &'a [Variable],
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Value(expr) => expr.size_mut(sleigh, variables),
            //TODO verify this
            Self::Reference { addr: _, memory } => Box::new(&mut memory.size),
            Self::Const { len_bits, .. } => Box::new(len_bits),
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        match self {
            Self::Const { .. } => Ok(()),
            Self::Value(expr) => expr.solve(sleigh, variables, solved),
            Self::Reference { addr, memory } => {
                addr.solve(sleigh, variables, solved)?;
                memory.solve(solved);
                if addr.size(sleigh, variables).is_undefined() {
                    solved.iam_not_finished(addr.src(), file!(), line!());
                }
                Ok(())
            }
        }
    }
    pub fn convert(self) -> FinalExport {
        match self {
            Self::Const {
                len_bits,
                location,
                export,
            } => FinalExport::Const {
                len_bits: len_bits.possible_value().unwrap(),
                location,
                export,
            },
            Self::Value(expr) => FinalExport::Value(expr.convert()),
            Self::Reference { addr, memory } => FinalExport::Reference {
                addr: addr.convert(),
                memory: memory.convert(),
            },
        }
    }
}
