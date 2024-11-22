use crate::execution::DynamicValueType;
use crate::semantic::execution::Export as FinalExport;
use crate::semantic::execution::ExportLen as FinalExportLen;
use crate::semantic::inner::pattern::Pattern;
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::{
    AttachVarnodeId, ExecutionError, Number, NumberNonZeroUnsigned, SpaceId,
    Span, TableId, VarSizeError,
};

use super::ExprContext;
use super::ExprDisVar;
use super::ExprIntDynamic;
use super::ExprTokenField;
use super::ReadScope;
use super::{
    Execution, Expr, ExprElement, ExprNumber, ExprValue, FieldSize,
    FieldSizeMut, FieldSizeTableExport, FieldSizeUnmutable, MemoryLocation,
};

/// Changes allowed:
/// Const -> Value -> Reference
#[derive(Clone, Copy, Debug)]
pub enum TableExportType {
    //don't return
    None,
    // value that is known at Dissassembly time
    Const(FieldSize),
    // value that can be know at execution time
    Value(FieldSize),
    // References/registers and other mem locations, all with the same size
    Reference {
        len: FieldSize,
        // None means multiple diferent SpaceIds used
        space: Option<SpaceId>,
        // mean some constructors also export values
        also_values: bool,
    },
}

#[derive(Clone, Debug)]
pub enum Export {
    Reference {
        addr: Expr,
        memory: MemoryLocation,
    },
    AttachVarnode {
        location: Span,
        attach_value: DynamicValueType,
        attach_id: AttachVarnodeId,
    },
    Table {
        location: Span,
        table_id: TableId,
    },

    /// const, NOTE const zext or lsb_trunk the inner value
    Const {
        bytes: NumberNonZeroUnsigned,
        // TODO delete this, is just the len of value
        input_len: FieldSize,
        value: Expr,
    },

    /// other complex expressions
    Value(Expr),
}

impl TableExportType {
    pub fn export_nothing(&self) -> bool {
        matches!(self, Self::None)
    }
    pub fn size(&self) -> Option<&FieldSize> {
        match self {
            Self::None => None,
            Self::Const(len)
            | Self::Value(len)
            | Self::Reference { len, .. } => Some(len),
        }
    }
    pub fn size_mut(&mut self) -> Option<&mut FieldSize> {
        match self {
            Self::None => None,
            Self::Const(len)
            | Self::Value(len)
            | Self::Reference { len, .. } => Some(len),
        }
    }
    pub fn combine(self, other: Self) -> Option<Self> {
        match (self, other) {
            //if both return nothing, the result is to return nothing
            (Self::None, Self::None) => Some(Self::None),
            // one constructor exporting and other don't is forbidden
            (Self::None, _) | (_, Self::None) => None,

            // both constants become a constant
            (Self::Const(len_a), Self::Const(len_b)) => {
                Some(Self::Const(len_a.intersection(len_b)?))
            }

            // if one value and a constant, result in a value
            (
                Self::Const(len_a) | Self::Value(len_a),
                Self::Const(len_b) | Self::Value(len_b),
            ) => Some(Self::Value(len_a.intersection(len_b)?)),

            // export a reference and also a value
            (
                Self::Value(len_a) | Self::Const(len_a),
                Self::Reference {
                    len,
                    space,
                    also_values: _,
                },
            )
            | (
                Self::Reference {
                    len,
                    space,
                    also_values: _,
                },
                Self::Value(len_a) | Self::Const(len_a),
            ) => Some(Self::Reference {
                len: len_a.intersection(len)?,
                space,
                also_values: true,
            }),

            // combine both references
            (
                Self::Reference {
                    len,
                    space,
                    also_values,
                },
                Self::Reference {
                    len: other_len,
                    space: other_space,
                    also_values: other_also_values,
                },
            ) => Some(Self::Reference {
                len: len.intersection(other_len)?,
                // only if both uses the same space
                space: space
                    .zip(other_space)
                    .filter(|(a, b)| a == b)
                    .map(|(a, _b)| a),
                // if one export values, result also do
                also_values: also_values | other_also_values,
            }),
        }
    }
    //TODO from Option into a Result
    pub fn final_size(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::None => None,
            Self::Const(len) => Some(len.possible_value().unwrap()),
            Self::Value(len) => Some(len.possible_value().unwrap()),
            Self::Reference { len, .. } => Some(len.possible_value().unwrap()),
        }
    }
    pub fn convert(self) -> Option<FinalExportLen> {
        match self {
            Self::None => None,
            Self::Const(x) => {
                Some(FinalExportLen::Const(x.possible_value().unwrap()))
            }
            Self::Value(x) => {
                Some(FinalExportLen::Value(x.possible_value().unwrap()))
            }
            Self::Reference { len, .. } => {
                Some(FinalExportLen::Reference(len.possible_value().unwrap()))
            }
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
            Expr::Value(ExprElement::Value {
                location,
                value: ExprValue::Varnode(varnode_expr),
            }) => {
                let varnode = sleigh.varnode(varnode_expr);
                let number = ExprNumber::new(Number::Positive(varnode.address));
                Export::new_reference(
                    sleigh,
                    pattern,
                    execution,
                    Expr::Value(ExprElement::Value {
                        location: location.clone(),
                        value: ExprValue::Int(number),
                    }),
                    MemoryLocation {
                        space: varnode.space,
                        size: FieldSize::new_bytes(varnode.len_bytes),
                        location,
                    },
                )
            }

            // if the value is just an token_field or context that translate to varnode,
            // then is actually a reference
            Expr::Value(ExprElement::Value {
                location,
                value: ExprValue::VarnodeDynamic(var_dyn),
            }) => Ok(Export::AttachVarnode {
                location: location.clone(),
                attach_value: var_dyn.attach_value,
                attach_id: var_dyn.attach_id,
            }),

            // exporting a table don't change the inner exported value
            Expr::Value(ExprElement::Value {
                location,
                value: ExprValue::Table(table_id),
            }) => {
                let table = sleigh.table(table_id);
                match *table.export.borrow() {
                    // don't allow export a table that don't export
                    Some(TableExportType::None) | None => Err(Box::new(
                        ExecutionError::WriteInvalidTable(location),
                    )),
                    _ => Ok(Export::Table { table_id, location }),
                }
            }

            expr => Ok(Self::Value(expr)),
        }
    }

    pub fn new_const(
        sleigh: &Sleigh,
        execution: &Execution,
        _pattern: &Pattern,
        read_scope: ReadScope,
        size: crate::syntax::block::execution::op::ByteRangeLsb,
        src: Span,
    ) -> Result<Self, Box<ExecutionError>> {
        let value = ExprValue::from_read_scope(sleigh, read_scope);
        Ok(Self::Const {
            input_len: value.size(sleigh, execution),
            value: Expr::Value(ExprElement::Value {
                location: src.clone(),
                value,
            }),
            bytes: size.value.try_into().unwrap(/* TODO error? */),
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
            .size_mut(sleigh, execution)
            .update_action(|size| size.set_max_bytes(space.addr_bytes));

        // memory can be any size, and the size of the space is possible
        // but ignore if not possible, because it can also be smaller
        let _ = addr.size_mut(sleigh, execution).update_action(|s| {
            s.set_possible_bytes(sleigh.space(memory.space).addr_bytes)
        });

        let _ = modified.ok_or_else(|| VarSizeError::AddressTooBig {
            address_size: addr.size(sleigh, execution),
            space_bytes: space.addr_bytes,
            location: src,
        })?;
        Ok(Self::Reference { addr, memory })
    }
    pub fn return_type(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> TableExportType {
        match self {
            Export::Const { value, .. } | Export::Value(value) => match value {
                Expr::Value(ExprElement::Value {
                    value:
                        ExprValue::Context(ExprContext { size, .. })
                        | ExprValue::TokenField(ExprTokenField { size, .. })
                        | ExprValue::DisVar(ExprDisVar { size, .. })
                        | ExprValue::IntDynamic(ExprIntDynamic { size, .. }),
                    ..
                }) => TableExportType::Const(*size),
                Expr::Value(ExprElement::Value {
                    value: ExprValue::InstStart(_),
                    ..
                }) => {
                    // TODO error here? Or make addr_bytes always available?
                    TableExportType::Const(FieldSize::new_bytes(
                        sleigh.addr_bytes().unwrap(),
                    ))
                }

                // any other expr is a value being exportd
                value => TableExportType::Value(value.size(sleigh, execution)),
            },

            Export::Reference { addr: _, memory } => {
                TableExportType::Reference {
                    len: memory.size,
                    space: Some(memory.space),
                    also_values: false,
                }
            }

            Export::AttachVarnode {
                attach_value: _,
                location: _,
                attach_id,
            } => {
                let attach_bytes = sleigh.attach_varnodes_len_bytes(*attach_id);
                let attach_varnodes = sleigh.attach_varnode(*attach_id);
                let varnode_id = attach_varnodes.0[0].1;
                let varnode = sleigh.varnode(varnode_id);
                TableExportType::Reference {
                    len: FieldSize::new_bytes(attach_bytes),
                    space: Some(varnode.space),
                    also_values: false,
                }
            }

            Export::Table {
                table_id,
                location: _,
            } => {
                let table = sleigh.table(*table_id);
                table.export.borrow().to_owned().unwrap()
            }
        }
    }
    pub fn src(&self) -> &Span {
        match self {
            Self::Const { value: expr, .. }
            | Self::Value(expr)
            | Export::Reference { addr: expr, .. } => expr.src(),
            Self::Table { location, .. }
            | Export::AttachVarnode { location, .. } => location,
        }
    }
    pub fn output_size(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> FieldSize {
        match self {
            Self::Const { bytes, .. } => FieldSize::new_bytes(*bytes),
            Self::Value(expr) => expr.size(sleigh, execution),
            //TODO verify this
            Self::Reference { addr: _, memory } => memory.size,
            Self::AttachVarnode { attach_id, .. } => {
                let attach_bytes = sleigh.attach_varnodes_len_bytes(*attach_id);
                FieldSize::new_bytes(attach_bytes)
            }
            Self::Table {
                table_id,
                location: _,
            } => {
                let table = sleigh.table(*table_id);
                *table.export.borrow().unwrap().size().unwrap()
            }
        }
    }
    pub fn output_size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        variables: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Const { bytes, .. } => {
                Box::new(FieldSizeUnmutable(FieldSize::new_bytes(*bytes)))
            }
            Self::Value(expr) => expr.size_mut(sleigh, variables),
            //TODO verify this
            Self::Reference { addr: _, memory } => Box::new(&mut memory.size),
            Self::AttachVarnode { attach_id, .. } => {
                let attach_bytes = sleigh.attach_varnodes_len_bytes(*attach_id);
                Box::new(FieldSizeUnmutable::from(FieldSize::new_bytes(
                    attach_bytes,
                )))
            }
            Self::Table {
                location: _,
                table_id,
            } => {
                let table = sleigh.table(*table_id);
                Box::new(FieldSizeTableExport(&table.export))
            }
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), Box<ExecutionError>> {
        match self {
            Self::Const {
                bytes: _,
                value,
                input_len,
            } => {
                // input can have any size
                value.solve(sleigh, execution, solved)?;
                *input_len = value.size(sleigh, execution);
                Ok(())
            }
            Self::AttachVarnode { .. } => Ok(()),
            Self::Value(expr) => {
                if hack_export_simple_disassembly_value(expr, sleigh, execution)
                {
                    solved.i_did_a_thing();
                }
                expr.solve(sleigh, execution, solved)
            }
            Self::Reference { addr, memory } => {
                addr.solve(sleigh, execution, solved)?;
                memory.solve(solved);
                if addr.size(sleigh, execution).is_undefined() {
                    solved.iam_not_finished(addr.src(), file!(), line!());
                }
                Ok(())
            }
            Self::Table { .. } => Ok(()),
        }
    }
    pub fn convert(self) -> FinalExport {
        match self {
            Self::Const {
                bytes,
                input_len,
                value,
            } => {
                let value_bits = input_len.possible_value().unwrap();
                let bits = bytes.get() * 8;
                if bits > value_bits.get() {
                    // if export is bigger then the value, zext it
                    FinalExport::Value(crate::semantic::execution::Expr::Value(
                        crate::semantic::execution::ExprElement::Op(
                            crate::semantic::execution::ExprUnaryOp {
                                location: value.src().clone(),
                                op: crate::semantic::execution::Unary::Zext(
                                    bits.try_into().unwrap(),
                                ),
                                input: Box::new(value.convert()),
                            },
                        ),
                    ))
                } else {
                    // if export is smaller then the value, truncate it
                    FinalExport::Value(crate::semantic::execution::Expr::Value(
                        crate::semantic::execution::ExprElement::Op(
                            crate::semantic::execution::ExprUnaryOp {
                                location: value.src().clone(),
                                op: crate::semantic::execution::Unary::TakeLsb(
                                    bytes,
                                ),
                                input: Box::new(value.convert()),
                            },
                        ),
                    ))
                }
            }
            Self::Value(expr) => FinalExport::Value(expr.convert()),
            Self::Reference { addr, memory } => FinalExport::Reference {
                addr: addr.convert(),
                memory: memory.convert(),
            },
            Self::AttachVarnode {
                location,
                attach_value,
                attach_id,
            } => FinalExport::AttachVarnode {
                location,
                attach_value,
                attach_id,
            },

            Self::Table { location, table_id } => {
                FinalExport::Table { location, table_id }
            }
        }
    }
}

// HACK allow tables to export disassembly values without any clue to the required size of it
// eg:
// ```
// # RIP/EIP relative address - NOTE: export of size 0 is intentional so it may be adjusted
// pcRelSimm32: addr	is simm32 [ addr=inst_next+simm32; ] { export addr; }
// ```
fn hack_export_simple_disassembly_value(
    expr: &mut Expr,
    sleigh: &Sleigh,
    execution: &Execution,
) -> bool {
    // export size need to be undefined
    if !expr.size(sleigh, execution).is_fully_undefined() {
        return false;
    }

    // exported value need to be a simple disassembly variable
    let Expr::Value(ExprElement::Value {
        location: _,
        value: ExprValue::DisVar(dis_expr),
    }) = expr
    else {
        return false;
    };

    // set the variable to have the possible len of 64bits
    dis_expr.size = dis_expr
        .size
        .set_possible_bits(64.try_into().unwrap())
        .unwrap();
    true
}
