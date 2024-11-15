use std::cell::RefCell;
use std::ops::Range;

use crate::execution::{DynamicValueType, ExprVarnodeDynamic};
use crate::semantic::disassembly;
use crate::semantic::execution::{
    BlockId, Build, RefTable, RefTokenField, ReferencedValue, VariableId,
    WriteValue,
};
use crate::semantic::inner::execution::{
    Block, ExprBitrange, ExprNumber, ExprTokenField, Unary,
};
use crate::semantic::inner::pattern::Pattern;
use crate::semantic::inner::pcode_macro::PcodeMacro;
use crate::semantic::inner::{GlobalScope, Sleigh};
use crate::semantic::{InstNext, InstStart, SpaceId, TableId};
use crate::{
    syntax, BitrangeId, ContextId, Number, NumberNonZeroUnsigned,
    NumberUnsigned, Span, TokenFieldId, VarSizeError, VarnodeId,
};

use super::{
    Assignment, AssignmentOp, BranchCall, CpuBranch, Execution, ExecutionError,
    Export, Expr, ExprCPool, ExprDisVar, ExprElement, ExprIntDynamic, ExprNew,
    ExprUnaryOp, ExprValue, FieldSize, LocalGoto, MacroParamAssignment,
    MemWrite, MemoryLocation, Reference, Statement, TableExportType, UserCall,
};

#[derive(Clone, Debug)]
pub enum ReadScope {
    TokenField(TokenFieldId),
    InstStart,
    InstNext,
    Varnode(VarnodeId),
    Context(ContextId),
    Bitrange(BitrangeId),
    Table(TableId),
    DisVar(disassembly::VariableId),
    ExeVar(VariableId),
}

pub trait ExecutionBuilder {
    fn sleigh(&self) -> &Sleigh;
    fn pattern(&self) -> &Pattern;
    fn execution(&self) -> &Execution;
    fn execution_mut(&mut self) -> &mut Execution;
    fn read_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ReadScope, Box<ExecutionError>>;
    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteValue, Box<ExecutionError>>;
    fn table(
        &self,
        name: &str,
        src: &Span,
    ) -> Result<TableId, Box<ExecutionError>> {
        self.sleigh()
            .get_global(name)
            .ok_or_else(|| Box::new(ExecutionError::MissingRef(src.clone())))?
            .table()
            .ok_or_else(|| Box::new(ExecutionError::InvalidRef(src.clone())))
    }
    fn space(
        &self,
        name: &str,
        src: &Span,
    ) -> Result<SpaceId, Box<ExecutionError>> {
        self.sleigh()
            .get_global(name)
            .ok_or_else(|| Box::new(ExecutionError::MissingRef(src.clone())))?
            .space()
            .ok_or_else(|| Box::new(ExecutionError::InvalidRef(src.clone())))
    }
    fn current_block(&self) -> BlockId;
    //TODO rename this
    fn inner_set_curent_block(&mut self, block: BlockId);
    fn set_current_block(&mut self, block: BlockId) {
        let current_id = self.current_block();
        if let Some(old) = self
            .execution_mut()
            .block_mut(current_id)
            .next
            .replace(block)
        {
            panic!("multiple next, old: {old:?}")
        }
        self.inner_set_curent_block(block)
    }
    fn create_variable(
        &mut self,
        name: &str,
        src: &Span,
        size: Option<FieldSize>,
        explicit: bool,
    ) -> Result<VariableId, Box<ExecutionError>> {
        let var = self.execution_mut().create_variable(
            name.to_owned(),
            src.clone(),
            size,
            explicit,
        )?;
        // TODO only create a declare if explicit?
        self.insert_statement(Statement::Declare(var));
        Ok(var)
    }
    fn insert_statement(&mut self, statement: Statement) {
        let current_block_id = self.current_block();
        self.execution_mut()
            .block_mut(current_block_id)
            .statements
            .push(RefCell::new(statement));
    }
    fn extend(
        &mut self,
        input: syntax::block::execution::Execution,
    ) -> Result<(), Box<ExecutionError>> {
        //start by creating all the blocks
        for statement in input.statements.iter() {
            if let syntax::block::execution::Statement::Label(label) = statement
            {
                self.execution_mut()
                    .new_block(label.name.to_owned())
                    .ok_or_else(|| {
                        Box::new(ExecutionError::DuplicatedLabel(
                            label.src.clone(),
                        ))
                    })?;
            }
        }

        //convert all the other statements
        for statement in input.statements.into_iter() {
            match statement {
                syntax::block::execution::Statement::Label(x) => {
                    //finding label means changing block
                    let new_current_block =
                        self.execution().block_by_name(&x.name).unwrap();
                    self.set_current_block(new_current_block);
                }
                syntax::block::execution::Statement::Delayslot(x) => {
                    self.insert_statement(Statement::Delayslot(x.0));
                }
                syntax::block::execution::Statement::Export(x) => {
                    let export = self.new_export(x)?;
                    self.insert_statement(Statement::Export(export));
                }
                syntax::block::execution::Statement::Build(x) => {
                    let build = self.new_build(x)?;
                    self.insert_statement(Statement::Build(build));
                }
                syntax::block::execution::Statement::Branch(x) => {
                    let label = matches!(
                        x.dst,
                        syntax::block::execution::branch::BranchDst::Label(_)
                    );
                    if label {
                        let goto = self.new_local_goto(x)?;
                        self.insert_statement(Statement::LocalGoto(goto));
                    } else {
                        let branch = self.new_cpu_branch(x)?;
                        self.insert_statement(Statement::CpuBranch(branch));
                    }
                }
                syntax::block::execution::Statement::Call(x) => {
                    self.new_call_statement(x)?;
                }
                syntax::block::execution::Statement::Declare(x) => {
                    let size = x
                        .size
                        .map(|size| {
                            NumberNonZeroUnsigned::new(size.value)
                                .map(FieldSize::new_bytes)
                                .ok_or_else(|| {
                                    Box::new(ExecutionError::InvalidVarLen(
                                        size.src,
                                    ))
                                })
                        })
                        .transpose()?;
                    self.create_variable(&x.name, &x.src, size, true)?;
                }
                syntax::block::execution::Statement::Assignment(x) => {
                    let assignment = self.new_assignment(x)?;
                    self.insert_statement(assignment);
                }
                syntax::block::execution::Statement::MemWrite(x) => {
                    let assignment = self.new_mem_write(x)?;
                    self.insert_statement(Statement::MemWrite(assignment));
                }
            }
        }
        //update blocks based on the last statement
        for block in &mut self.execution_mut().blocks {
            let Some(last_statement) = block.statements.last() else {
                continue;
            };
            let next = match &*last_statement.borrow() {
                //this block ends with unconditional local_jmp, this replace the
                //next block
                Statement::LocalGoto(LocalGoto { cond: None, dst }) => {
                    //remove the goto, the next block will tha it's place
                    Some(Some(*dst))
                }
                //If the last is export or unconditional cpu branch,
                //this becames an return block, so next is None
                Statement::Export(_)
                | Statement::CpuBranch(CpuBranch { cond: None, .. }) => {
                    Some(None)
                }
                _ => None,
            };
            if let Some(next_block) = next {
                block.next = next_block;
                if let Some(_block_id) = next_block {
                    block.statements.pop();
                }
            }
        }
        //find the return type for this execution
        let return_type = {
            let execution = self.execution();
            let mut iter = execution
                .blocks
                .iter()
                //only blocks with no next block can export
                .filter(|block| block.next.is_none())
                .filter_map(|block| block.statements.last())
                //if the last statement is export, convert to export size
                .filter_map(|statement| match &*statement.borrow() {
                    Statement::Export(exp) => {
                        Some(exp.return_type(self.sleigh(), self.execution()))
                    }
                    _ => None,
                });
            //FUTURE: replace this with try_reduce:
            //`.try_reduce(|acc, item| acc.combine(item));`
            match iter.next() {
                Some(first) => iter
                    .try_fold(first, |acc, item| acc.combine(item))
                    .map(Some),
                None => Some(None),
            }
        };
        self.execution_mut().return_value = match return_type {
            //short circuit, AKA invalid combination of return types
            None => return Err(Box::new(ExecutionError::InvalidExport)),
            //there are no returns
            Some(None) => TableExportType::None,
            //some return type
            Some(Some(ret)) => ret,
        };
        Ok(())
    }

    fn new_build(
        &mut self,
        input: syntax::block::execution::Build,
    ) -> Result<Build, Box<ExecutionError>> {
        let table_id = self.table(&input.table_name, &input.src)?;
        Ok(Build {
            table: table_id,
            location: input.src,
        })
    }
    fn new_export(
        &mut self,
        input: syntax::block::execution::export::Export,
    ) -> Result<Export, Box<ExecutionError>> {
        use syntax::block::execution::export::Export as RawExport;
        match input {
            RawExport::Value(value) => {
                let value = self.new_expr(value)?;
                Export::new_value(
                    self.sleigh(),
                    self.pattern(),
                    self.execution(),
                    value,
                )
            }
            RawExport::Reference { space, addr } => {
                let addr = self.new_expr(addr)?;
                let deref = self.new_addr_derefence(&space)?;
                Export::new_reference(
                    self.sleigh(),
                    self.pattern(),
                    self.execution(),
                    addr,
                    deref,
                )
            }
            RawExport::Const { size, value, src } => {
                let read_scope = self.read_scope(&value, &src)?;
                Export::new_const(
                    self.sleigh(),
                    self.execution(),
                    self.pattern(),
                    read_scope,
                    size,
                    src,
                )
            }
        }
    }
    fn new_call_statement(
        &mut self,
        input: syntax::block::execution::UserCall,
    ) -> Result<(), Box<ExecutionError>> {
        let params = input
            .params
            .into_iter()
            .map(|param| self.new_expr(param))
            .collect::<Result<Vec<_>, _>>()?;
        let global = self
            .sleigh()
            .get_global(&input.name)
            .ok_or_else(|| ExecutionError::MissingRef(input.src.clone()))?;
        match global {
            GlobalScope::UserFunction(x) => {
                self.insert_statement(Statement::UserCall(UserCall::new(
                    self.sleigh(),
                    self.execution(),
                    params,
                    x,
                    input.src,
                )));
                Ok(())
            }
            GlobalScope::PcodeMacro(macro_id) => {
                // TODO create an alias system, so variable and block names
                // don't colide
                // TODO make pcode_macro use RefCell to avoid this clone
                let pmacro = self.sleigh().pcode_macro(macro_id).clone();

                // if the macro have more then one block, split blocks
                let (block_offset, next_block) =
                    if pmacro.execution.blocks.len() == 1 {
                        (None, None)
                    } else {
                        let current_block_name = self
                            .execution()
                            .block(self.current_block())
                            .name
                            .to_owned();
                        // block to go after the pmacro
                        let next_block = BlockId(self.execution().blocks.len());
                        self.execution_mut().blocks.push(Block::new_empty(
                            Some(current_block_name.unwrap_or_else(|| {
                                format!("{}_after", &pmacro.name)
                                    .into_boxed_str()
                            })),
                        ));

                        // mapping macro -> execution blocks
                        let block_offset = self.execution().blocks.len();
                        // create all the macro blocks
                        self.execution_mut().blocks.extend(
                            pmacro.execution.blocks.iter().map(|b| {
                                Block::new_empty(Some(
                                    b.name.clone().unwrap_or_else(|| {
                                        format!("{}_entry", &pmacro.name)
                                            .into_boxed_str()
                                    }),
                                ))
                            }),
                        );
                        (Some(block_offset), Some(next_block))
                    };

                // mapping variable -> execution variable
                let variables_map = self.map_variables(&pmacro, &params);

                // assign the values to the params
                for (param_id, param) in params.iter().enumerate() {
                    let old_var_id = pmacro.params[param_id];
                    let new_var = &variables_map[old_var_id.0];
                    match new_var {
                        VariableAlias::Parameter(variable_id) => self
                            .insert_statement(Statement::MacroParamAssignment(
                                MacroParamAssignment::new(
                                    *variable_id,
                                    param.clone(),
                                ),
                            )),
                        VariableAlias::SubVarnode(_, _)
                        | VariableAlias::Alias(_)
                        | VariableAlias::NewVariable(_) => {}
                    }
                }

                // populate the blocks
                for (i, block) in
                    pmacro.execution.blocks.as_slice().iter().enumerate()
                {
                    if let Some(block_offset) = block_offset {
                        let block_id = BlockId(i + block_offset);
                        self.set_current_block(block_id);
                    }

                    for statement in block.statements.iter() {
                        let statement = statement.borrow();
                        let new_statement = match &*statement {
                            Statement::LocalGoto(goto) => {
                                let block_id =
                                    BlockId(goto.dst.0 + block_offset.unwrap());
                                let cond = goto.cond.as_ref().map(|cond| {
                                    translate_expr(cond, &variables_map)
                                });
                                Statement::LocalGoto(LocalGoto {
                                    cond,
                                    dst: block_id,
                                })
                            }
                            Statement::CpuBranch(x) => {
                                Statement::CpuBranch(CpuBranch {
                                    cond: x.cond.as_ref().map(|x| {
                                        translate_expr(x, &variables_map)
                                    }),
                                    dst: translate_expr(&x.dst, &variables_map),
                                    ..x.clone()
                                })
                            }
                            Statement::UserCall(x) => {
                                Statement::UserCall(UserCall {
                                    params: x
                                        .params
                                        .iter()
                                        .map(|x| {
                                            translate_expr(x, &variables_map)
                                        })
                                        .collect(),
                                    ..x.clone()
                                })
                            }
                            Statement::Assignment(x) => {
                                let (var, op) = translate_write(
                                    self.sleigh(),
                                    &x.var,
                                    &x.location,
                                    &variables_map,
                                )?;
                                if let Some((_new_op, _old_op)) =
                                    op.as_ref().zip(x.op.as_ref())
                                {
                                    todo!("consiliate two operation on macro");
                                }
                                Statement::Assignment(Assignment {
                                    right: translate_expr(
                                        &x.right,
                                        &variables_map,
                                    ),
                                    var,
                                    op: x.op.clone().or(op),
                                    ..x.clone()
                                })
                            }
                            Statement::MacroParamAssignment(x) => {
                                let var = match variables_map[x.var.0] {
                                    // TODO error, can't assign a value to anything other then a variable
                                    VariableAlias::SubVarnode(_, _)
                                    | VariableAlias::Parameter(_)
                                    | VariableAlias::Alias(_) => panic!(),
                                    VariableAlias::NewVariable(id) => id,
                                };

                                Statement::MacroParamAssignment(
                                    MacroParamAssignment::new(
                                        var,
                                        translate_expr(
                                            &x.right,
                                            &variables_map,
                                        ),
                                    ),
                                )
                            }
                            Statement::MemWrite(x) => {
                                Statement::MemWrite(MemWrite {
                                    addr: translate_expr(
                                        &x.addr,
                                        &variables_map,
                                    ),
                                    right: translate_expr(
                                        &x.right,
                                        &variables_map,
                                    ),
                                    ..x.clone()
                                })
                            }
                            Statement::Declare(old_id) => {
                                match variables_map[old_id.0].clone() {
                                    VariableAlias::SubVarnode(_, _)
                                    | VariableAlias::Parameter(_)
                                    | VariableAlias::Alias(_) => panic!(),
                                    VariableAlias::NewVariable(id) => {
                                        Statement::Declare(id)
                                    }
                                }
                            }
                            x @ Statement::Delayslot(_) => x.clone(),
                            Statement::Export(_) | Statement::Build(_) => {
                                unreachable!()
                            }
                        };
                        self.insert_statement(new_statement);
                    }
                }
                // next block after the call is the previous block
                if let Some(next_block) = next_block {
                    self.set_current_block(next_block);
                }
                Ok(())
            }
            _ => Err(Box::new(ExecutionError::InvalidRef(input.src))),
        }
    }
    fn new_call_expr(
        &mut self,
        input: syntax::block::execution::UserCall,
    ) -> Result<ExprElement, Box<ExecutionError>> {
        let params = input
            .params
            .into_iter()
            .map(|param| self.new_expr(param))
            .collect::<Result<Vec<_>, _>>()?;
        let global = self
            .sleigh()
            .get_global(&input.name)
            .ok_or_else(|| ExecutionError::MissingRef(input.src.clone()))?;
        match global {
            GlobalScope::UserFunction(function) => {
                Ok(ExprElement::UserCall(UserCall::new(
                    self.sleigh(),
                    self.execution(),
                    params,
                    function,
                    input.src,
                )))
            }
            GlobalScope::PcodeMacro(x) => {
                let pmacro = self.sleigh().pcode_macro(x);
                todo!("user defined {} exports?", &pmacro.name)
            }
            _ => Err(Box::new(ExecutionError::InvalidRef(input.src))),
        }
    }

    fn new_assignment(
        &mut self,
        input: syntax::block::execution::assignment::Assignment,
    ) -> Result<Statement, Box<ExecutionError>> {
        let right = self.new_expr(input.right)?;
        let var = self.write_scope(&input.ident, &input.src).ok();
        match (var, input.local) {
            //variable don't exists, create it
            (None, local) => {
                //the var size is defined if ByteRangeLsb is present
                //add the var creation statement
                let size = match &input.op {
                    Some(
                        syntax::block::execution::assignment::OpLeft::ByteRangeLsb(x),
                    ) => {
                        Some(FieldSize::new_bytes(x.value.try_into().unwrap()))
                    }
                    Some(_) => todo!("create var with this op?"),
                    None => None,
                };
                let new_var_id = self.create_variable(
                    &input.ident,
                    &input.src,
                    size,
                    local,
                )?;
                Ok(Statement::Assignment(Assignment::new(
                    input.src.clone(),
                    WriteValue::Local {
                        id: new_var_id,
                        creation: local,
                    },
                    None,
                    input.src,
                    right,
                )))
            }
            //variable exists, with local is error
            (Some(_), true) => {
                Err(Box::new(ExecutionError::InvalidVarDeclare(input.src)))
            }
            //Assign to varnode
            (Some(WriteValue::Varnode(var)), false) => {
                let op =
                    input.op.map(|op| self.new_truncate(op)).transpose()?;
                let addr = WriteValue::Varnode(var);
                Ok(Statement::Assignment(Assignment::new(
                    input.src.clone(),
                    addr,
                    op,
                    input.src,
                    right,
                )))
            }
            //variable exists, just return it
            (Some(var), false) => {
                let op =
                    input.op.map(|op| self.new_truncate(op)).transpose()?;
                Ok(Statement::Assignment(Assignment::new(
                    input.src.clone(),
                    var,
                    op,
                    input.src,
                    right,
                )))
            }
        }
    }
    fn new_mem_write(
        &mut self,
        input: syntax::block::execution::assignment::MemWrite,
    ) -> Result<MemWrite, Box<ExecutionError>> {
        let mem = self.new_addr_derefence(&input.mem)?;
        let addr = self.new_expr(input.addr)?;
        let right = self.new_expr(input.right)?;
        Ok(MemWrite::new(
            self.sleigh(),
            self.execution(),
            addr,
            mem,
            input.src,
            right,
        ))
    }
    fn new_truncate(
        &self,
        input: syntax::block::execution::assignment::OpLeft,
    ) -> Result<AssignmentOp, Box<ExecutionError>> {
        use syntax::block::execution::assignment::OpLeft;
        //TODO genertic error here
        let error = Box::new(ExecutionError::BitRangeZero);
        let ass = match input {
            OpLeft::BitRange(range) => {
                let size =
                    NumberNonZeroUnsigned::new(range.n_bits).ok_or(error)?;
                AssignmentOp::BitRange(
                    range.lsb_bit..range.lsb_bit + size.get(),
                )
            }
            OpLeft::ByteRangeMsb(msb) => AssignmentOp::TrunkLsb {
                bytes: msb.value,
                output_size: FieldSize::default(),
            },
            OpLeft::ByteRangeLsb(lsb) => {
                AssignmentOp::TakeLsb(lsb.value.try_into().unwrap())
            }
        };
        Ok(ass)
    }
    fn new_cpu_branch_dst(
        &mut self,
        input: syntax::block::execution::branch::BranchDst,
    ) -> Result<(bool, Expr), Box<ExecutionError>> {
        use syntax::block::execution::branch::BranchDst::*;
        Ok(match input {
            Label(_) => unreachable!(),
            Cpu { direct, expr } => (direct, self.new_expr(expr)?),
        })
    }
    fn new_cpu_branch(
        &mut self,
        input: syntax::block::execution::branch::Branch,
    ) -> Result<CpuBranch, Box<ExecutionError>> {
        let cond = input.cond.map(|x| self.new_expr(x)).transpose()?;
        let call = input.call;
        let (direct, dst) = self.new_cpu_branch_dst(input.dst)?;

        Ok(CpuBranch::new(
            self.sleigh(),
            self.execution(),
            cond,
            call,
            direct,
            dst,
        ))
    }
    fn new_local_goto(
        &mut self,
        input: syntax::block::execution::branch::Branch,
    ) -> Result<LocalGoto, Box<ExecutionError>> {
        let cond = input.cond.map(|x| self.new_expr(x)).transpose()?;
        if !matches!(input.call, BranchCall::Goto) {
            return Err(Box::new(ExecutionError::InvalidLocalGoto));
        }
        let dst = match input.dst {
            syntax::block::execution::branch::BranchDst::Label(x) => {
                self.execution().block_by_name(&x.name).ok_or_else(|| {
                    Box::new(ExecutionError::MissingLabel(x.src.clone()))
                })?
            }
            _ => unreachable!(),
        };
        LocalGoto::new(self.sleigh(), self.execution(), cond, dst)
    }
    fn new_expr_element(
        &mut self,
        input: syntax::block::execution::expr::ExprElement,
    ) -> Result<ExprElement, Box<ExecutionError>> {
        use syntax::block::execution::expr::ExprElement as RawExprElement;
        match input {
            RawExprElement::Value(syntax::Value::Number(src, value)) => {
                Ok(ExprElement::Value {
                    location: src,
                    value: ExprValue::Int(ExprNumber::new(value)),
                })
            }
            RawExprElement::Value(syntax::Value::Ident(src, value)) => {
                let value = self.read_scope(&value, &src)?;
                // HACK add auto trunk if inst_*
                match value {
                    value @ (ReadScope::InstStart | ReadScope::InstNext) => {
                        let mut size = FieldSize::new_unsized()
                            .set_min_bits(1.try_into().unwrap())
                            .unwrap();
                        if let Some(addr_bytes) = self.sleigh().addr_bytes() {
                            size = size
                                .set_max_bytes(addr_bytes)
                                .unwrap()
                                .set_possible_bytes(addr_bytes)
                                .unwrap();
                        }
                        Ok(ExprElement::new_op(
                            src.clone(),
                            Unary::TrunkLsb { trunk: 0, size },
                            Expr::Value(ExprElement::Value {
                                location: src.clone(),
                                value: ExprValue::from_read_scope(
                                    self.sleigh(),
                                    value,
                                ),
                            }),
                        ))
                    }
                    value => Ok(ExprElement::Value {
                        location: src,
                        value: ExprValue::from_read_scope(self.sleigh(), value),
                    }),
                }
            }
            RawExprElement::Reference(src, size, value) => {
                let ref_bytes = size
                    .map(|x| {
                        //TODO non generic error here
                        NumberNonZeroUnsigned::new(x.value).ok_or_else(|| {
                            Box::new(ExecutionError::BitRangeZero)
                        })
                    })
                    .transpose()?;
                let value = reference_scope(
                    self.read_scope(&value, &src)?,
                    src,
                    ref_bytes,
                    self.sleigh(),
                )?;
                Ok(value)
            }
            RawExprElement::Op(src, raw_op, input) => {
                let input = self.new_expr(*input)?;
                self.new_op_unary(&raw_op, src, input)
            }
            RawExprElement::New(src, param0, param1) => {
                let param0 = self.new_expr(*param0).map(Box::new)?;
                let param1 = param1
                    .map(|param| self.new_expr(*param).map(Box::new))
                    .transpose()?;
                Ok(ExprElement::New(ExprNew {
                    location: src,
                    first: param0,
                    second: param1,
                }))
            }
            RawExprElement::CPool(src, params) => {
                let params = params
                    .into_iter()
                    .map(|param| self.new_expr(param))
                    .collect::<Result<_, _>>()?;
                Ok(ExprElement::CPool(ExprCPool {
                    location: src,
                    params,
                }))
            }
            RawExprElement::UserCall(call) => self.new_call_expr(call),
            RawExprElement::Ambiguous1 {
                name,
                param,
                param_src,
            } => {
                //can be one of two possibilities:
                if let Ok(value) = self.read_scope(&name, &param_src) {
                    //first: value with ByteRangeMsb operator
                    let value = Expr::Value(ExprElement::Value {
                        location: param_src.clone(),
                        value: ExprValue::from_read_scope(self.sleigh(), value),
                    });
                    Ok(ExprElement::new_trunk_lsb(param_src, param, value))
                } else {
                    //second: (user_?)function call with one parameter
                    //we know this is not a primitive function, macro
                    //probably never exports, so this can only be a
                    //user_function
                    self.new_call_expr(syntax::block::execution::UserCall::new(
                        name,
                        param_src.clone(),
                        vec![syntax::block::execution::expr::Expr::Value(
                            syntax::block::execution::expr::ExprElement::Value(
                                crate::syntax::Value::Number(
                                    param_src,
                                    Number::Positive(param),
                                ),
                            ),
                        )],
                    ))
                }
            }
        }
    }
    fn new_expr(
        &mut self,
        input: syntax::block::execution::expr::Expr,
    ) -> Result<Expr, Box<ExecutionError>> {
        use syntax::block::execution::expr::Expr as RawExpr;
        match input {
            RawExpr::Value(value) => {
                self.new_expr_element(value).map(Expr::Value)
            }
            RawExpr::Op(src, op, left, right) => {
                let left = self.new_expr(*left)?;
                let right = self.new_expr(*right)?;
                Ok(Expr::new_op(
                    self.sleigh(),
                    self.execution(),
                    src,
                    op,
                    left,
                    right,
                ))
            }
        }
    }
    fn new_op_unary(
        &self,
        input: &syntax::block::execution::op::Unary,
        src: Span,
        expr: Expr,
    ) -> Result<ExprElement, Box<ExecutionError>> {
        use syntax::block::execution::op::Unary as Op;
        let to_nonzero =
            //TODO generic error here
            |x: NumberUnsigned| NumberNonZeroUnsigned::new(x).ok_or_else(||Box::new(ExecutionError::BitRangeZero));
        let op = match input {
            Op::ByteRangeMsb(x) => {
                return Ok(ExprElement::new_trunk_lsb(src, x.value, expr))
            }
            Op::ByteRangeLsb(x) => {
                let lsb_bits = NumberNonZeroUnsigned::new(x.value * 8).unwrap();
                let lsb_len = FieldSize::new_bits(lsb_bits);
                match expr {
                    // NOTE, Lsb on and Int/DisassemblyVar/TokenField/Bitrange just
                    // set the len
                    // except if TokenField have the value translated into a varnode
                    Expr::Value(ExprElement::Value {
                        location,
                        value:
                            ExprValue::Int(ExprNumber {
                                size: _,
                                number: value,
                            }),
                    }) => {
                        if u64::from(value.bits_required()) > lsb_bits.get() {
                            return Err(Box::new(ExecutionError::VarSize(
                                VarSizeError::TakeLsbTooSmall {
                                    lsb: x.value.try_into().unwrap(),
                                    input: lsb_len,
                                    location,
                                },
                            )));
                        }
                        return Ok(ExprElement::Value {
                            location,
                            value: ExprValue::Int(ExprNumber {
                                //TODO error
                                size: lsb_len,
                                number: value,
                            }),
                        });
                    }
                    Expr::Value(ExprElement::Value {
                        location,
                        value: ExprValue::DisVar(ExprDisVar { size: _, id }),
                    }) => {
                        return Ok(ExprElement::Value {
                            location,
                            value: ExprValue::DisVar(ExprDisVar {
                                size: lsb_len,
                                id,
                            }),
                        });
                    }
                    Expr::Value(ExprElement::Value {
                        location,
                        value:
                            ExprValue::TokenField(ExprTokenField { size: _, id }),
                    }) => {
                        let tf = self.sleigh().token_field(id);
                        if let Some(crate::token::TokenFieldAttach::Varnode(
                            att_var_id,
                        )) = tf.attach
                        {
                            let value =
                                ExprValue::VarnodeDynamic(ExprVarnodeDynamic {
                                    attach_id: att_var_id,
                                    attach_value: DynamicValueType::TokenField(
                                        id,
                                    ),
                                });
                            return Ok(ExprElement::new_op(
                                x.src.clone(),
                                Unary::TakeLsb(x.value.try_into().unwrap()),
                                Expr::Value(ExprElement::Value {
                                    location,
                                    value,
                                }),
                            ));
                        } else if tf.bits.len() > lsb_bits {
                            // if tf is bigger then the lsb, this is a regular take_lsb op
                            return Ok(ExprElement::new_op(
                                x.src.clone(),
                                Unary::TakeLsb(x.value.try_into().unwrap()),
                                Expr::Value(ExprElement::Value {
                                    location,
                                    value: ExprValue::TokenField(
                                        ExprTokenField {
                                            size: FieldSize::new_unsized()
                                                .set_min_bits(tf.bits.len())
                                                .unwrap()
                                                .set_possible_min(),
                                            id,
                                        },
                                    ),
                                }),
                            ));
                        } else {
                            // otherwise the lsb just define the tf size
                            let value = ExprValue::TokenField(ExprTokenField {
                                size: lsb_len,
                                id,
                            });
                            return Ok(ExprElement::Value { location, value });
                        }
                    }
                    Expr::Value(ExprElement::Value {
                        location,
                        value: ExprValue::Bitrange(ExprBitrange { size: _, id }),
                    }) => {
                        return Ok(ExprElement::Value {
                            location,
                            value: ExprValue::Bitrange(ExprBitrange {
                                size: lsb_len,
                                id,
                            }),
                        });
                    }
                    _ => {
                        return Ok(ExprElement::new_take_lsb(
                            src,
                            to_nonzero(x.value)?,
                            expr,
                        ));
                    }
                }
            }
            Op::BitRange(range) => {
                return Ok(ExprElement::new_bitrange(
                    src,
                    range.lsb_bit,
                    to_nonzero(range.n_bits)?,
                    expr,
                ))
            }
            Op::Dereference(x) => {
                return Ok(ExprElement::new_op(
                    src,
                    Unary::Dereference(self.new_addr_derefence(x)?),
                    expr,
                ))
            }
            Op::Negation => Unary::Negation,
            Op::BitNegation => Unary::BitNegation,
            Op::Negative => Unary::Negative,
            Op::FloatNegative => Unary::FloatNegative,
            Op::Popcount => Unary::Popcount(FieldSize::new_unsized()),
            Op::Lzcount => Unary::Lzcount(FieldSize::new_unsized()),
            Op::Zext => Unary::Zext(FieldSize::new_unsized()),
            Op::Sext => Unary::Sext(FieldSize::new_unsized()),
            Op::FloatNan => Unary::FloatNan(FieldSize::new_bool()),
            Op::FloatAbs => Unary::FloatAbs,
            Op::FloatSqrt => Unary::FloatSqrt,
            Op::Int2Float => Unary::Int2Float(FieldSize::new_unsized()),
            Op::Float2Float => Unary::Float2Float(FieldSize::new_unsized()),
            Op::SignTrunc => Unary::SignTrunc(FieldSize::new_unsized()),
            Op::FloatCeil => Unary::FloatCeil,
            Op::FloatFloor => Unary::FloatFloor,
            Op::FloatRound => Unary::FloatRound,
        };
        Ok(ExprElement::new_op(src, op, expr))
    }
    fn new_addr_derefence(
        &self,
        input: &syntax::block::execution::op::AddrDereference,
    ) -> Result<MemoryLocation, Box<ExecutionError>> {
        let space = input
            .space
            .as_ref()
            .map(|x| self.space(&x.name, &x.src))
            .unwrap_or_else(|| {
            self.sleigh()
                .default_space()
                .ok_or_else(|| Box::new(ExecutionError::DefaultSpace))
        })?;
        //size will be the lsb of size, if specified, otherwise  we can't know
        //the size directly
        let size = match input.size.as_ref() {
            Some(size) => FieldSize::new_bytes(
                NumberNonZeroUnsigned::new(size.value)
                    .ok_or_else(|| Box::new(ExecutionError::BitRangeZero))?,
            ),
            None => FieldSize::new_unsized(),
        };
        Ok(MemoryLocation {
            space,
            size,
            location: input.src.clone(),
        })
    }

    #[allow(private_interfaces)]
    fn map_variables<'a>(
        &mut self,
        pmacro: &PcodeMacro,
        params: &'a [Expr],
    ) -> Vec<VariableAlias<'a>> {
        pmacro
            .execution
            .variables
            .iter()
            .enumerate()
            .map(|(var_id, var)| {
                let var_id = VariableId(var_id);
                // if the variable is a parameter and the parameter is a value,
                // just make the variable an alias to the original value
                let param_id = pmacro
                    .params
                    .iter()
                    .position(|param_id| *param_id == var_id);
                if let Some(param_id) = param_id {
                    let param = &params[param_id];
                    // HACK if the parameter is a single value, the param is simply
                    // replaced
                    if let Expr::Value(ExprElement::Value {
                        location: _,
                        value,
                    }) = param
                    {
                        return VariableAlias::Alias(value);
                    }

                    // a varnode with bitrange, became a bitrange assignment
                    if let Expr::Value(ExprElement::Op(ExprUnaryOp {
                        op: Unary::BitRange { range, .. },
                        input,
                        ..
                    })) = param
                    {
                        // NOTE this only apply to:
                        // * Varnode
                        // * TokenField that that translate to varnode
                        // * Table that export a memory location
                        match &**input {
                            Expr::Value(ExprElement::Value {
                                value: value @ ExprValue::Varnode(_),
                                ..
                            }) => {
                                return VariableAlias::SubVarnode(
                                    value,
                                    range.clone(),
                                )
                            }
                            Expr::Value(ExprElement::Value {
                                value: value @ ExprValue::TokenField(tf_expr),
                                ..
                            }) => {
                                let tf = self.sleigh().token_field(tf_expr.id);
                                if let Some(
                                    crate::token::TokenFieldAttach::Varnode(_),
                                ) = tf.attach
                                {
                                    return VariableAlias::SubVarnode(
                                        value,
                                        range.clone(),
                                    );
                                }
                            }
                            Expr::Value(ExprElement::Value {
                                value: value @ ExprValue::Table(table_id),
                                ..
                            }) => {
                                let table = self.sleigh().table(*table_id);
                                let table_export = table.export.borrow();
                                if let Some(TableExportType::Reference {
                                    len: _,
                                    space: _,
                                    also_values: _,
                                }) = &*table_export
                                {
                                    return VariableAlias::SubVarnode(
                                        value,
                                        range.clone(),
                                    );
                                }
                            }
                            // can't translate anything else into a writable bitrange
                            _ => {}
                        }
                    }

                    // otherwise just create a variable and assign the param value to it
                    let id = self
                        .execution_mut()
                        .create_variable(
                            format!("{}_{}", &pmacro.name, &var.name),
                            var.src.clone(),
                            Some(var.size.get()),
                            var.explicit,
                        )
                        .unwrap();
                    VariableAlias::Parameter(id)
                } else {
                    // if just a variable, create a new variable
                    let id = self
                        .execution_mut()
                        .create_variable(
                            format!("{}_{}", &pmacro.name, &var.name),
                            var.src.clone(),
                            Some(var.size.get()),
                            var.explicit,
                        )
                        .unwrap();
                    VariableAlias::NewVariable(id)
                }
            })
            .collect()
    }
}

fn reference_scope(
    read: ReadScope,
    src: Span,
    ref_bytes: Option<NumberNonZeroUnsigned>,
    sleigh: &Sleigh,
) -> Result<ExprElement, Box<ExecutionError>> {
    match read {
        ReadScope::TokenField(id) => Ok(ExprElement::Reference(Reference {
            location: src.clone(),
            len: ref_bytes.map(FieldSize::new_bytes).unwrap_or_default(),
            value: ReferencedValue::TokenField(RefTokenField {
                location: src.clone(),
                id,
            }),
        })),
        //TODO What is a reference to inst_start/inst_next? Just the
        //value?
        ReadScope::InstStart => {
            let element = ExprElement::Value {
                location: src.clone(),
                value: ExprValue::InstStart(InstStart),
            };
            if let Some(ref_bytes) = ref_bytes {
                Ok(ExprElement::new_take_lsb(
                    src,
                    ref_bytes,
                    Expr::Value(element),
                ))
            } else {
                Ok(element)
            }
        }
        ReadScope::InstNext => {
            let element = ExprElement::Value {
                location: src.clone(),
                value: ExprValue::InstNext(InstNext),
            };
            if let Some(ref_bytes) = ref_bytes {
                Ok(ExprElement::new_take_lsb(
                    src,
                    ref_bytes,
                    Expr::Value(element),
                ))
            } else {
                Ok(element)
            }
        }
        ReadScope::Varnode(id) => {
            let varnode = sleigh.varnode(id);
            let size =
                ref_bytes.map(FieldSize::new_bytes).unwrap_or_else(|| {
                    let space = sleigh.space(varnode.space);
                    FieldSize::new_bytes(space.addr_bytes)
                });
            Ok(ExprElement::Value {
                location: src,
                value: ExprValue::Int(ExprNumber {
                    size,
                    number: Number::Positive(varnode.address),
                }),
            })
        }
        ReadScope::Table(id) => Ok(ExprElement::Reference(Reference {
            location: src.clone(),
            len: ref_bytes.map(FieldSize::new_bytes).unwrap_or_default(),
            value: ReferencedValue::Table(RefTable { location: src, id }),
        })),
        _ => Err(Box::new(ExecutionError::InvalidRef(src))),
    }
}

#[derive(Clone)]
enum VariableAlias<'a> {
    Alias(&'a ExprValue),
    SubVarnode(&'a ExprValue, Range<NumberUnsigned>),
    Parameter(VariableId),
    NewVariable(VariableId),
}

fn translate_expr(expr: &Expr, variables_map: &[VariableAlias<'_>]) -> Expr {
    match expr {
        Expr::Value(value) => {
            Expr::Value(translate_expr_element(value, variables_map))
        }
        Expr::Op(op) => {
            let left = translate_expr(&op.left, variables_map);
            let right = translate_expr(&op.right, variables_map);
            Expr::Op(crate::semantic::inner::execution::ExprBinaryOp {
                location: op.location.clone(),
                output_size: op.output_size,
                op: op.op,
                left: Box::new(left),
                right: Box::new(right),
            })
        }
    }
}

fn translate_expr_element(
    expr: &ExprElement,
    variables_map: &[VariableAlias<'_>],
) -> ExprElement {
    match expr {
        ExprElement::Value { location, value } => {
            translate_value(location, value, variables_map)
        }
        ExprElement::UserCall(call) => ExprElement::UserCall(UserCall {
            params: call
                .params
                .iter()
                .map(|x| translate_expr(x, variables_map))
                .collect(),
            ..call.clone()
        }),
        ExprElement::Op(x) => ExprElement::Op(super::ExprUnaryOp {
            input: Box::new(translate_expr(&x.input, variables_map)),
            ..x.clone()
        }),
        ExprElement::New(x) => ExprElement::New(ExprNew {
            first: Box::new(translate_expr(&x.first, variables_map)),
            second: x
                .second
                .as_ref()
                .map(|x| Box::new(translate_expr(x, variables_map))),
            location: x.location.clone(),
        }),
        ExprElement::CPool(x) => ExprElement::CPool(ExprCPool {
            location: x.location.clone(),
            params: x
                .params
                .iter()
                .map(|x| translate_expr(x, variables_map))
                .collect(),
        }),
        x @ ExprElement::Reference(_) => x.clone(),
    }
}

fn translate_value(
    location: &Span,
    expr: &ExprValue,
    variables_map: &[VariableAlias<'_>],
) -> ExprElement {
    match expr {
        ExprValue::TokenField(_)
        | ExprValue::Table(_)
        | ExprValue::DisVar(_) => unreachable!(),

        ExprValue::ExeVar(id) => match variables_map[id.0].clone() {
            VariableAlias::Alias(x) => ExprElement::Value {
                location: location.clone(),
                value: x.clone(),
            },
            VariableAlias::SubVarnode(varnode, bits) => {
                ExprElement::Op(ExprUnaryOp {
                    location: location.clone(),
                    op: Unary::BitRange {
                        range: bits.clone(),
                        size: FieldSize::new_unsized()
                            .set_min_bits(
                                (bits.end - bits.start).try_into().unwrap(),
                            )
                            .unwrap()
                            .set_possible_min(),
                    },
                    input: Box::new(Expr::Value(ExprElement::Value {
                        location: location.clone(),
                        value: varnode.clone(),
                    })),
                })
            }
            VariableAlias::Parameter(id) | VariableAlias::NewVariable(id) => {
                ExprElement::Value {
                    location: location.clone(),
                    value: ExprValue::ExeVar(id),
                }
            }
        },

        x @ (ExprValue::Varnode(_)
        | ExprValue::Context(_)
        | ExprValue::Bitrange(_)
        | ExprValue::InstStart(_)
        | ExprValue::InstNext(_)
        | ExprValue::Int(_)) => ExprElement::Value {
            location: location.clone(),
            value: x.clone(),
        },
        ExprValue::IntDynamic(_) => todo!(),
        ExprValue::VarnodeDynamic(_) => todo!(),
    }
}

fn translate_write(
    sleigh: &Sleigh,
    expr: &WriteValue,
    location: &Span,
    variables_map: &[VariableAlias<'_>],
) -> Result<(WriteValue, Option<AssignmentOp>), Box<ExecutionError>> {
    match expr {
        WriteValue::Local { id, creation: _ } => match variables_map[id.0]
            .clone()
        {
            VariableAlias::SubVarnode(ExprValue::Varnode(varnode), bits) => {
                let value = WriteValue::Varnode(*varnode);
                Ok((value, Some(AssignmentOp::BitRange(bits))))
            }
            VariableAlias::SubVarnode(
                ExprValue::TokenField(token_id),
                bits,
            ) => {
                let token_field = sleigh.token_field(token_id.id);
                let Some(crate::token::TokenFieldAttach::Varnode(attach_id)) =
                    token_field.attach
                else {
                    todo!();
                };
                let value = WriteValue::TokenField {
                    token_field_id: token_id.id,
                    attach_id,
                };
                Ok((value, Some(AssignmentOp::BitRange(bits))))
            }
            VariableAlias::SubVarnode(ExprValue::Table(table_id), bits) => {
                let value = table_write(sleigh, *table_id, location)?;
                Ok((value, Some(AssignmentOp::BitRange(bits))))
            }
            VariableAlias::SubVarnode(_, _) => unreachable!(),
            VariableAlias::Alias(value) => {
                match value {
                    // TODO verify those assumptions
                    ExprValue::Int(_)
                    | ExprValue::InstStart(_)
                    | ExprValue::InstNext(_)
                    | ExprValue::Context(_)
                    | ExprValue::Bitrange(_)
                    | ExprValue::DisVar(_) => panic!(),

                    ExprValue::ExeVar(id) => {
                        let value = WriteValue::Local {
                            id: *id,
                            creation: false,
                        };
                        Ok((value, None))
                    }
                    ExprValue::Varnode(id) => {
                        let value = WriteValue::Varnode(*id);
                        Ok((value, None))
                    }
                    ExprValue::TokenField(tf_expr) => {
                        let token_field = sleigh.token_field(tf_expr.id);
                        let Some(crate::token::TokenFieldAttach::Varnode(
                            attach_id,
                        )) = token_field.attach
                        else {
                            todo!();
                        };
                        let value = WriteValue::TokenField {
                            token_field_id: tf_expr.id,
                            attach_id,
                        };
                        Ok((value, None))
                    }
                    ExprValue::VarnodeDynamic(ExprVarnodeDynamic {
                        attach_id,
                        attach_value:
                            DynamicValueType::TokenField(token_field_id),
                    }) => {
                        let value = WriteValue::TokenField {
                            token_field_id: *token_field_id,
                            attach_id: *attach_id,
                        };
                        Ok((value, None))
                    }
                    ExprValue::VarnodeDynamic(ExprVarnodeDynamic {
                        attach_id: _,
                        attach_value: DynamicValueType::Context(_context_id),
                    }) => {
                        todo!();
                    }
                    ExprValue::Table(id) => {
                        let table = table_write(sleigh, *id, location)?;
                        Ok((table, None))
                    }
                    ExprValue::IntDynamic(ExprIntDynamic { .. }) => {
                        panic!()
                    }
                }
            }
            VariableAlias::Parameter(id) | VariableAlias::NewVariable(id) => {
                Ok((
                    WriteValue::Local {
                        id,
                        creation: false,
                    },
                    None,
                ))
            }
        },
        x @ (WriteValue::Varnode(_)
        | WriteValue::Bitrange(_)
        | WriteValue::TokenField { .. }
        | WriteValue::TableExport(_)) => Ok((x.clone(), None)),
    }
}

pub fn table_write(
    sleigh: &Sleigh,
    table_id: TableId,
    location: &Span,
) -> Result<WriteValue, Box<ExecutionError>> {
    let table = sleigh.table(table_id);
    let table = table.export.borrow();
    let Some(TableExportType::Reference {
        len: _,
        space: _,
        also_values: _,
    }) = &*table
    else {
        return Err(Box::new(ExecutionError::WriteInvalidTable(
            location.clone(),
        )));
    };
    Ok(WriteValue::TableExport(table_id))
}
