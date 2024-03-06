use crate::semantic::execution::{
    BlockId, Build, ExprExeVar, ExprTable, ReferencedValue, Unary, VariableId,
    WriteValue,
};
use crate::semantic::inner::execution::{ExprNumber, ExprUnaryOp};
use crate::semantic::inner::Sleigh;
use crate::semantic::{GlobalScope, SpaceId, TableId};
use crate::{
    disassembly, syntax, Number, NumberNonZeroUnsigned, NumberUnsigned, Span,
};

use super::{
    Assignment, BranchCall, CpuBranch, Execution, ExecutionError, Export,
    ExportConst, ExportLen, Expr, ExprCPool, ExprDisVar, ExprElement, ExprNew,
    FieldSize, LocalGoto, MacroCall, MemWrite, MemoryLocation, ReadValue,
    Reference, Statement, Truncate, UserCall,
};

pub trait ExecutionBuilder {
    fn sleigh(&self) -> &Sleigh;
    fn disassembly_var(
        &mut self,
        id: disassembly::VariableId,
    ) -> &mut disassembly::Variable;
    fn execution(&self) -> &Execution;
    fn execution_mut(&mut self) -> &mut Execution;
    fn read_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ReadValue, Box<ExecutionError>>;
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
        let _ = self
            .execution_mut()
            .block_mut(current_id)
            .next
            .replace(block)
            .map_or((), |_| panic!("multiple next"));
        self.inner_set_curent_block(block)
    }
    fn create_variable(
        &mut self,
        name: &str,
        src: &Span,
        explicit: bool,
    ) -> Result<VariableId, Box<ExecutionError>> {
        let var = self.execution_mut().create_variable(
            name.to_owned(),
            src.clone(),
            explicit,
        )?;
        self.insert_statement(Statement::Declare(var));
        Ok(var)
    }
    fn insert_statement(&mut self, statement: Statement) {
        let current_block_id = self.current_block();
        self.execution_mut()
            .block_mut(current_block_id)
            .statements
            .push(statement);
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
                    let call = self.new_call_statement(x)?;
                    self.insert_statement(call);
                }
                syntax::block::execution::Statement::Declare(x) => {
                    let var_id = self.create_variable(&x.name, &x.src, true)?;
                    let var = self.execution_mut().variable_mut(var_id);
                    if let Some(size) = x.size {
                        let size = NumberNonZeroUnsigned::new(size.value)
                            .map(FieldSize::new_bytes)
                            .ok_or_else(|| {
                                Box::new(ExecutionError::InvalidVarLen(
                                    size.src,
                                ))
                            })?;
                        var.size.set(size);
                    }
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
        for block in self.execution_mut().blocks.iter_mut() {
            let last_statement = block.statements.last();
            match last_statement {
                //this block ends with unconditional local_jmp, this replace the
                //next block
                Some(Statement::LocalGoto(LocalGoto { cond: None, dst })) => {
                    //remove the goto, the next block will tha it's place
                    block.next = Some(*dst);
                    block.statements.pop();
                }
                //If the last is export or unconditional cpu branch,
                //this becames an return block, so next is None
                Some(Statement::Export(_))
                | Some(Statement::CpuBranch(CpuBranch {
                    cond: None, ..
                })) => block.next = None,
                _ => (),
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
                //if the last statement is export, convert to export size
                .filter_map(|block| match block.statements.last() {
                    Some(Statement::Export(exp)) => {
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
            Some(None) => ExportLen::None,
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
        let location = input.src;
        Ok(Build {
            table: ExprTable {
                location,
                id: table_id,
            },
        })
    }
    fn new_export_const(
        &mut self,
        input: &str,
        src: &Span,
    ) -> Result<ExportConst, Box<ExecutionError>> {
        use ReadValue::*;
        match self.read_scope(input, src)? {
            ExeVar(var) => Ok(ExportConst::ExeVar(var.id)),
            DisVar(var) => Ok(ExportConst::DisVar(var.id)),
            TokenField(ass) => Ok(ExportConst::TokenField(ass.id)),
            Context(cont) => Ok(ExportConst::Context(cont.id)),
            InstStart(_) => Ok(ExportConst::InstructionStart),
            Table(expr_table) => {
                let table = self.sleigh().table(expr_table.id);
                match *table.export.borrow() {
                    Some(ExportLen::Const(_) | ExportLen::Value(_)) => {
                        Ok(ExportConst::Table(expr_table.id))
                    }
                    //TODO more specific error
                    //a const can only export const or context
                    Some(_) => {
                        Err(Box::new(ExecutionError::InvalidRef(src.clone())))
                    }
                    //to constructors are available yet, can't use this table
                    None => {
                        Err(Box::new(ExecutionError::InvalidRef(src.clone())))
                    }
                }
            }
            x => todo!("Error export invalid const {:#?}", x),
        }
    }
    fn new_export(
        &mut self,
        input: syntax::block::execution::export::Export,
    ) -> Result<Export, Box<ExecutionError>> {
        use syntax::block::execution::export::Export as RawExport;
        match input {
            RawExport::Value(value) => {
                let value = self.new_expr(value)?;
                match &value {
                    //if the value is just an varnode, then is actually a reference
                    Expr::Value(ExprElement::Value(ReadValue::Varnode(
                        varnode_expr,
                    ))) => {
                        let varnode = self.sleigh().varnode(varnode_expr.id);
                        Export::new_reference(
                            self.sleigh(),
                            self.execution(),
                            Expr::Value(ExprElement::Value(ReadValue::Int(
                                ExprNumber::new(
                                    varnode_expr.location.clone(),
                                    Number::Positive(varnode.address),
                                ),
                            ))),
                            MemoryLocation {
                                space: varnode.space,
                                size: FieldSize::new_bytes(varnode.len_bytes),
                                src: varnode_expr.location.clone(),
                            },
                        )
                    }
                    _ => Ok(Export::new_value(value)),
                }
            }
            RawExport::Reference { space, addr } => {
                let addr = self.new_expr(addr)?;
                let deref = self.new_addr_derefence(&space)?;
                // if the addr is a single Disassembly variable, it affects
                // how it's printed
                if let Expr::Value(ExprElement::Value(ReadValue::DisVar(
                    variable,
                ))) = &addr
                {
                    let variable = self.disassembly_var(variable.id);
                    variable.value_type =
                        variable.value_type.set_space(deref.space).ok_or_else(
                            || Box::new(ExecutionError::InvalidExport),
                        )?;
                }
                Export::new_reference(
                    self.sleigh(),
                    self.execution(),
                    addr,
                    deref,
                )
            }
            RawExport::Const { size, value, src } => {
                let value = self.new_export_const(&value, &src)?;
                let size =
                    NumberNonZeroUnsigned::new(size.value).unwrap(/*TODO*/);
                // if the value is a disassembly variable, we define it's len
                if let ExportConst::DisVar(id) = value {
                    let variable = self.disassembly_var(id);
                    variable.value_type =
                        variable.value_type.set_len(size).ok_or_else(|| {
                            Box::new(ExecutionError::InvalidExport)
                        })?;
                }
                let size = FieldSize::new_bytes(size);
                Ok(Export::new_const(size, src, value))
            }
        }
    }
    fn new_call_statement(
        &mut self,
        input: syntax::block::execution::UserCall,
    ) -> Result<Statement, Box<ExecutionError>> {
        let params = input
            .params
            .into_iter()
            .map(|param| self.new_expr(param))
            .collect::<Result<Vec<_>, _>>()?;
        match self.sleigh().get_global(&input.name).ok_or_else(|| {
            Box::new(ExecutionError::MissingRef(input.src.clone()))
        })? {
            GlobalScope::UserFunction(x) => {
                Ok(Statement::UserCall(UserCall::new(
                    self.sleigh(),
                    self.execution(),
                    params,
                    x,
                    input.src,
                )))
            }
            GlobalScope::PcodeMacro(x) => {
                Ok(Statement::MacroCall(MacroCall::new(params, x)))
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
        match self.sleigh().get_global(&input.name).ok_or_else(|| {
            Box::new(ExecutionError::MissingRef(input.src.clone()))
        })? {
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
        let mut right = self.new_expr(input.right)?;
        let var = self.write_scope(&input.ident, &input.src).ok();
        match (var, input.local) {
            //variable don't exists, create it
            (None, _) => {
                //the var size is defined if ByteRangeLsb is present
                //add the var creation statement
                let new_var_id =
                    self.create_variable(&input.ident, &input.src, false)?;
                let new_var = self.execution_mut().variable_mut(new_var_id);
                match &input.op {
                    Some(
                        syntax::block::execution::assignment::OpLeft::ByteRangeLsb(x),
                    ) => {
                        let new_len = new_var.size.get().set_final_value(
                                NumberNonZeroUnsigned::new(x.value * 8)
                                    .unwrap(),
                            )
                            .unwrap();
                        new_var.size.set(new_len);
                    }
                    Some(_) => todo!("create var with this op?"),
                    None => (),
                }
                Ok(Statement::Assignment(Assignment::new(
                    WriteValue::Local(ExprExeVar {
                        id: new_var_id,
                        location: input.src.clone(),
                    }),
                    None,
                    input.src,
                    right,
                )))
            }
            //variable exists, with local is error
            (Some(_), true) => {
                Err(Box::new(ExecutionError::InvalidVarDeclare(input.src)))
            }
            //variable exists, just return it
            (Some(var), false) => {
                let op = input
                    .op
                    .map(|op| self.new_assignment_op(op))
                    .transpose()?;
                match var {
                    //Assign to varnode is actually a mem write
                    WriteValue::Varnode(var) => {
                        let var_ele = self.sleigh().varnode(var.id);
                        let mem = MemoryLocation {
                            space: var_ele.space,
                            size: FieldSize::new_bytes(var_ele.len_bytes),
                            src: input.src.clone(),
                        };
                        let addr = Expr::Value(ExprElement::Value(
                            ReadValue::Int(ExprNumber {
                                location: input.src.clone(),
                                size: FieldSize::new_bytes(
                                    self.sleigh().addr_bytes().ok_or_else(
                                        || {
                                            Box::new(
                                                ExecutionError::InvalidRef(
                                                    input.src.clone(),
                                                ),
                                            )
                                        },
                                    )?,
                                ),
                                number: Number::Positive(var_ele.address),
                            }),
                        ));
                        {
                            let right = &mut right;
                            let sleigh = self.sleigh();
                            let execution = self.execution();
                            right.size_mut(sleigh, execution).update_action(
                                |size| {
                                    size.intersection(FieldSize::new_bytes(
                                        var_ele.len_bytes,
                                    ))
                                },
                            );
                        }
                        Ok(Statement::MemWrite(MemWrite::new(
                            self.sleigh(),
                            self.execution(),
                            addr,
                            mem,
                            input.src,
                            right,
                        )))
                    }
                    var => Ok(Statement::Assignment(Assignment::new(
                        var, op, input.src, right,
                    ))),
                }
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
    fn new_assignment_op(
        &self,
        input: syntax::block::execution::assignment::OpLeft,
    ) -> Result<Truncate, Box<ExecutionError>> {
        use syntax::block::execution::assignment::OpLeft;
        //TODO genertic error here
        let error = Box::new(ExecutionError::BitRangeZero);
        let ass = match input {
            OpLeft::BitRange(range) => {
                let size =
                    NumberNonZeroUnsigned::new(range.n_bits).ok_or(error)?;
                Truncate::new(range.lsb_bit, size)
            }
            OpLeft::ByteRangeMsb(msb) => Truncate::new_msb(msb.value),
            OpLeft::ByteRangeLsb(lsb) => {
                let size =
                    NumberNonZeroUnsigned::new(lsb.value).ok_or(error)?;
                Truncate::new_lsb(size)
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
            RawExprElement::Value(syntax::Value::Number(src, value)) => Ok(
                ExprElement::Value(ReadValue::Int(ExprNumber::new(src, value))),
            ),
            RawExprElement::Value(syntax::Value::Ident(src, value)) => {
                // some values have hidden properties
                let value = match self.read_scope(&value, &src)? {
                    ReadValue::TokenField(tf) => {
                        // token_field and bitrange auto expand to the size required
                        let field = self.sleigh().token_field(tf.id);
                        return Ok(ExprElement::Op(ExprUnaryOp {
                            location: tf.location.clone(),
                            output_size: FieldSize::new_unsized()
                                .set_min_bits(field.bits.len())
                                .unwrap(),
                            op: Unary::Zext,
                            input: Box::new(Expr::Value(ExprElement::Value(
                                ReadValue::TokenField(tf),
                            ))),
                        }));
                    }
                    ReadValue::Bitrange(bt) => {
                        let bitrange = self.sleigh().bitrange(bt.id);
                        return Ok(ExprElement::Op(ExprUnaryOp {
                            location: bt.location.clone(),
                            output_size: FieldSize::new_unsized()
                                .set_min_bits(bitrange.bits.len())
                                .unwrap(),
                            op: Unary::Zext,
                            input: Box::new(Expr::Value(ExprElement::Value(
                                ReadValue::Bitrange(bt),
                            ))),
                        }));
                    }
                    value => value,
                };
                Ok(ExprElement::Value(value))
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
                let value = match self.read_scope(&value, &src)? {
                    ReadValue::TokenField(ass) => {
                        ExprElement::Reference(Reference {
                            location: src,
                            len: ref_bytes
                                .map(FieldSize::new_bytes)
                                .unwrap_or_default(),
                            value: ReferencedValue::TokenField(ass),
                        })
                    }
                    //TODO What is a reference to inst_start/inst_next? Just the
                    //value?
                    ReadValue::InstStart(x) => {
                        let element =
                            ExprElement::Value(ReadValue::InstStart(x));
                        if let Some(ref_bytes) = ref_bytes {
                            ExprElement::Truncate(
                                src,
                                Truncate::new_lsb(ref_bytes),
                                Box::new(Expr::Value(element)),
                            )
                        } else {
                            element
                        }
                    }
                    ReadValue::InstNext(x) => {
                        let element =
                            ExprElement::Value(ReadValue::InstNext(x));
                        if let Some(ref_bytes) = ref_bytes {
                            ExprElement::Truncate(
                                src,
                                Truncate::new_lsb(ref_bytes),
                                Box::new(Expr::Value(element)),
                            )
                        } else {
                            element
                        }
                    }
                    ReadValue::Varnode(var) => {
                        let varnode = self.sleigh().varnode(var.id);
                        ExprElement::Value(ReadValue::Int(ExprNumber {
                            location: src,
                            size: ref_bytes
                                .map(FieldSize::new_bytes)
                                .unwrap_or_else(|| {
                                    let space =
                                        self.sleigh().space(varnode.space);
                                    FieldSize::new_bytes(space.addr_bytes)
                                }),
                            number: Number::Positive(varnode.address),
                        }))
                    }
                    ReadValue::Table(table) => {
                        ExprElement::Reference(Reference {
                            location: src,
                            len: ref_bytes
                                .map(FieldSize::new_bytes)
                                .unwrap_or_default(),
                            value: ReferencedValue::Table(table),
                        })
                    }
                    _ => return Err(Box::new(ExecutionError::InvalidRef(src))),
                };
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
                    Ok(ExprElement::new_truncate(
                        param_src,
                        Truncate::new_msb(param),
                        Expr::Value(ExprElement::Value(value)),
                    ))
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
                return Ok(ExprElement::Truncate(
                    src,
                    Truncate::new_msb(x.value),
                    Box::new(expr),
                ))
            }
            Op::ByteRangeLsb(x) => match expr {
                //NOTE, Lsb on and Int/DisassemblyVar just set the len
                Expr::Value(ExprElement::Value(ReadValue::Int(
                    ExprNumber {
                        location: src,
                        size: len,
                        number: value,
                    },
                ))) => {
                    return Ok(ExprElement::Value(ReadValue::Int(
                        ExprNumber {
                            location: src,
                            //TODO error
                            size: len
                                .intersection(FieldSize::new_bytes(
                                    NumberNonZeroUnsigned::new(x.value)
                                        .unwrap(),
                                ))
                                .unwrap(),
                            number: value,
                        },
                    )));
                }
                Expr::Value(ExprElement::Value(ReadValue::DisVar(
                    ExprDisVar { location, size, id },
                ))) => {
                    return Ok(ExprElement::Value(ReadValue::DisVar(
                        ExprDisVar {
                            location,
                            //TODO error
                            size: size
                                .intersection(FieldSize::new_bytes(
                                    NumberNonZeroUnsigned::new(x.value)
                                        .unwrap(),
                                ))
                                .unwrap(),
                            id,
                        },
                    )));
                }
                _ => {
                    return Ok(ExprElement::Truncate(
                        src,
                        Truncate::new_lsb(to_nonzero(x.value)?),
                        Box::new(expr),
                    ));
                }
            },
            Op::BitRange(range) => {
                return Ok(ExprElement::Truncate(
                    src,
                    Truncate::new(range.lsb_bit, to_nonzero(range.n_bits)?),
                    Box::new(expr),
                ))
            }
            Op::Dereference(x) => {
                return Ok(ExprElement::DeReference(
                    src,
                    self.new_addr_derefence(x)?,
                    Box::new(expr),
                ))
            }
            Op::Negation => Unary::Negation,
            Op::BitNegation => Unary::BitNegation,
            Op::Negative => Unary::Negative,
            Op::FloatNegative => Unary::FloatNegative,
            Op::Popcount => Unary::Popcount,
            Op::Lzcount => Unary::Lzcount,
            Op::Zext => Unary::Zext,
            Op::Sext => Unary::Sext,
            Op::FloatNan => Unary::FloatNan,
            Op::FloatAbs => Unary::FloatAbs,
            Op::FloatSqrt => Unary::FloatSqrt,
            Op::Int2Float => Unary::Int2Float,
            Op::Float2Float => Unary::Float2Float,
            Op::SignTrunc => Unary::SignTrunc,
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
            src: input.src.clone(),
        })
    }
}
