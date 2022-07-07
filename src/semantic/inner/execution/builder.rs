use std::rc::Rc;

use crate::base::{NonZeroTypeU, Value};
use crate::semantic::inner::execution::{
    AddrDereference, BranchCall, ExecutionError, Unary,
};
use crate::semantic::inner::{GlobalScope, Table};
use crate::syntax::block;

use super::*;

pub trait ExecutionBuilder<'a> {
    fn sleigh(&self) -> &Sleigh<'a>;
    fn execution(&self) -> &Execution;
    fn execution_mut(&mut self) -> &mut Execution;
    fn read_scope(
        &mut self,
        name: &'a str,
    ) -> Result<ExprValue, ExecutionError>;
    fn write_scope(
        &mut self,
        name: &'a str,
    ) -> Result<WriteValue, ExecutionError>;
    fn table(&self, name: &'a str) -> Result<Rc<Table>, ExecutionError> {
        let src = self.sleigh().input_src(name);
        self.sleigh()
            .get_global(name)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
            .unwrap_table()
            .ok_or(ExecutionError::InvalidRef(src.clone()))
    }
    fn space(&self, name: &'a str) -> Result<Rc<Space>, ExecutionError> {
        let src = self.sleigh().input_src(name);
        self.sleigh()
            .get_global(name)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
            .unwrap_space()
            .ok_or(ExecutionError::InvalidRef(src.clone()))
    }
    fn current_block(&self) -> Rc<Block>;
    fn current_block_mut(&mut self) -> &mut Rc<Block>;
    fn block(&self, name: &'a str) -> Option<Rc<Block>> {
        self.execution().block(name)
    }
    fn create_block(&mut self, name: &'a str) -> Option<Rc<Block>> {
        self.execution_mut().new_block(name)
    }
    fn set_current_block(&mut self, block: Rc<Block>) {
        {
            let current = self.current_block();
            let mut next = current.next.borrow_mut();
            next.replace(Rc::clone(&block))
                .map_or((), |_| panic!("multiple next"));
        }
        let current_block = self.current_block_mut();
        *current_block = block
    }
    fn variable(&self, name: &str) -> Option<&Rc<Variable>> {
        self.execution().variable(name)
    }
    fn create_variable(
        &mut self,
        name: &'a str,
        //scope: VariableScope,
    ) -> Result<Rc<Variable>, ExecutionError> {
        let src = self.sleigh().input_src(name);
        let var = self.execution_mut().create_variable(name, src)?;
        self.insert_statement(Statement::Declare(Rc::clone(&var)));
        Ok(var)
    }
    fn insert_statement(&mut self, statement: Statement) {
        let current_block = self.current_block();
        let mut statements = current_block.statements.borrow_mut();
        statements.push(statement);
    }
    fn extend(
        &mut self,
        mut input: block::execution::Execution<'a>,
    ) -> Result<(), ExecutionError> {
        //start by creating all the blocks
        for statement in input.statements.iter() {
            match statement {
                block::execution::Statement::Label(label) => {
                    let src = self.sleigh().input_src(label.name);
                    self.create_block(label.name)
                        .ok_or(ExecutionError::DuplicatedLabel(src))?;
                }
                _ => (),
            }
        }

        //convert all the other statements
        for statement in input.statements.drain(..) {
            match statement {
                block::execution::Statement::Label(x) => {
                    //finding label means changing block
                    let new_current_block = self.block(x.name).unwrap();
                    self.set_current_block(new_current_block);
                }
                block::execution::Statement::Delayslot(x) => {
                    self.insert_statement(Statement::Delayslot(x.0));
                }
                block::execution::Statement::Export(x) => {
                    let export = self.new_export(x)?;
                    self.insert_statement(Statement::Export(export));
                }
                block::execution::Statement::Build(x) => {
                    let build = self.new_build(x)?;
                    self.insert_statement(Statement::Build(build));
                }
                block::execution::Statement::Branch(x) => {
                    let label = matches!(
                        x.dst,
                        block::execution::branch::BranchDst::Label(_)
                    );
                    if label {
                        let goto = self.new_local_goto(x)?;
                        self.insert_statement(Statement::LocalGoto(goto));
                    } else {
                        let branch = self.new_cpu_branch(x)?;
                        self.insert_statement(Statement::CpuBranch(branch));
                    }
                }
                block::execution::Statement::Call(x) => {
                    let call = self.new_call_statement(x)?;
                    self.insert_statement(call);
                }
                block::execution::Statement::Declare(x) => {
                    if let Some(true) = x.size.map(|size| size.value == 0) {
                        let src = self.sleigh().input_src(x.ident);
                        return Err(ExecutionError::InvalidRef(src));
                    }
                    //variable without initialization is assumed to zeroed
                    self.create_variable(x.ident)?;
                }
                block::execution::Statement::Assignment(x) => {
                    let assignment = self.new_assignment(x)?;
                    self.insert_statement(Statement::Assignment(assignment));
                }
                block::execution::Statement::MemWrite(x) => {
                    let assignment = self.new_mem_write(x)?;
                    self.insert_statement(Statement::MemWrite(assignment));
                }
            }
        }
        //update blocks based on the last statement
        for block in self.execution().blocks() {
            let statements = block.statements.borrow();
            let last_statement = statements.last();
            match last_statement {
                //this block ends with unconditional local_jmp, this replace the
                //next block
                Some(Statement::LocalGoto(LocalGoto { cond: None, dst })) => {
                    //remove the goto, the next block will tha it's place
                    *block.next.borrow_mut() = Some(Rc::clone(dst));
                    drop(statements);
                    block.statements.borrow_mut().pop();
                }
                //If the last is export or unconditional cpu branch,
                //this becames an return block, so next is None
                Some(Statement::Export(_))
                | Some(Statement::CpuBranch(CpuBranch {
                    cond: None, ..
                })) => {
                    *block.next.borrow_mut() = None;
                }
                _ => (),
            }
        }
        //find the return type for this execution
        let return_type = self
            .execution()
            .blocks()
            //only blocks with no next block can export
            .filter(|block| block.next.borrow().is_none())
            //if the last statement is export, convert to export size
            .filter_map(|block| match block.statements.borrow().last() {
                Some(Statement::Export(exp)) => Some(exp.return_type()),
                _ => None,
            })
            .try_reduce(|acc, item| acc.combine(item));
        self.execution_mut().return_value = match return_type {
            //short circuit, AKA invalid combination of return types
            None => {
                return Err(ExecutionError::InvalidExport)
            },
            //there are no returns
            Some(None) => ExecutionExport::None,
            //some return type
            Some(Some(ret)) => ret,
        };
        Ok(())
    }

    fn new_build(
        &mut self,
        input: block::execution::Build<'a>,
    ) -> Result<Build, ExecutionError> {
        let table = self.table(input.table)?;
        //let var = self
        //    .variable(input.table)
        //    .map(|x| Ok(x))
        //    .unwrap_or_else(|| self.create_variable(input.table))?;
        let build = Build::new(
            table,
            //var
        );
        Ok(build)
    }
    fn new_export_const(
        &mut self,
        input: &'a str,
    ) -> Result<ExportConst, ExecutionError> {
        use ExprValue::*;
        match self.read_scope(input)? {
            //Int(InputSource, FieldSize, IntTypeU) => todo!(),
            DisVar(src, _size, dis) => Ok(ExportConst::DisVar(src, dis)),
            Assembly(src, _size, ass) => Ok(ExportConst::Assembly(src, ass)),
            _ => todo!("Error export invalid const"),
        }
    }
    fn new_export(
        &mut self,
        input: block::execution::export::Export<'a>,
    ) -> Result<Export, ExecutionError> {
        use block::execution::export::Export as RawExport;
        match input {
            //RawExportValue::Unique{} => todo!(),
            RawExport::Value(value) => {
                let value = self.new_expr(value)?;
                Ok(Export::new_value(value))
            }
            RawExport::Reference { space, addr } => {
                let addr = self.new_expr(addr)?;
                let deref = self.new_addr_derefence(&space)?;
                Ok(Export::new_reference(addr, deref))
            }
            RawExport::Const { size, value } => {
                let value = self.new_export_const(value)?;
                let size = NonZeroTypeU::new(size.value).unwrap(/*TODO*/);
                let size = FieldSize::new_bytes(size);
                Ok(Export::new_const(size, value))
            }
        }
    }
    fn new_call_statement(
        &mut self,
        mut input: block::execution::UserCall<'a>,
    ) -> Result<Statement, ExecutionError> {
        let params = input
            .params
            .drain(..)
            .map(|param| self.new_expr(param))
            .collect::<Result<Vec<_>, _>>()?;
        let src = self.sleigh().input_src(input.function);
        match self
            .sleigh()
            .get_global(input.function)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
        {
            GlobalScope::UserFunction(x) => Ok(Statement::UserCall(
                UserCall::new(src, params, Rc::clone(x)),
            )),
            GlobalScope::PcodeMacro(x) => {
                Ok(Statement::MacroCall(MacroCall::new(params, Rc::clone(x))))
            }
            _ => Err(ExecutionError::InvalidRef(src)),
        }
    }
    fn new_call_expr(
        &mut self,
        mut input: block::execution::UserCall<'a>,
    ) -> Result<ExprElement, ExecutionError> {
        let params = input
            .params
            .drain(..)
            .map(|param| self.new_expr(param))
            .collect::<Result<Vec<_>, _>>()?;
        let src = self.sleigh().input_src(input.function);
        match self
            .sleigh()
            .get_global(input.function)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
        {
            GlobalScope::UserFunction(x) => Ok(ExprElement::UserCall(
                //TODO better min output handler
                FIELD_SIZE_BOOL,
                UserCall::new(src, params, Rc::clone(x)),
            )),
            GlobalScope::PcodeMacro(x) => {
                todo!("user defined {} exports?", x.name)
            }
            _ => Err(ExecutionError::InvalidRef(src)),
        }
    }

    fn new_assignment(
        &mut self,
        input: block::execution::assignment::Assignment<'a>,
    ) -> Result<Assignment, ExecutionError> {
        let right = self.new_expr(input.right)?;
        let var = self.write_scope(input.ident).ok();
        let src = self.sleigh().input_src(input.ident);
        let (var, op) = match (var, input.local) {
            //variable don't exists, create it
            (None, _) => {
                //the var size is defined if ByteRangeLsb is present
                //add the var creation statement
                let new_var = self.create_variable(input.ident)?;
                match &input.op {
                    Some(
                        block::execution::assignment::OpLeft::ByteRangeLsb(x),
                    ) => {
                        new_var
                            .size()
                            .update_action(|size| {
                                size.set_final_value(
                                    NonZeroTypeU::new(x.value * 8).unwrap(),
                                )
                            })
                            .unwrap();
                    }
                    Some(_) => todo!("create var with this op?"),
                    None => (),
                }
                (WriteValue::ExeVar(src, new_var), None)
            }
            //variable exists, with local is error
            (Some(_), true) => {
                return Err(ExecutionError::InvalidVarDeclare(src));
            }
            //variable exists, just return it
            (Some(var), false) => (
                var,
                input
                    .op
                    .map(|op| self.new_assignment_op(op))
                    .transpose()?
                    .flatten(),
            ),
        };
        let src = self.sleigh().input_src(input.ident);
        Ok(Assignment::new(var, op, src, right))
    }
    fn new_mem_write(
        &mut self,
        input: block::execution::assignment::MemWrite<'a>,
    ) -> Result<MemWrite, ExecutionError> {
        let mem = self.new_addr_derefence(&input.mem)?;
        let addr = self.new_expr(input.addr)?;
        let src = self.sleigh().input_src(input.src);
        let right = self.new_expr(input.right)?;
        Ok(MemWrite::new(addr, mem, src, right))
    }
    fn new_assignment_op(
        &self,
        input: block::execution::assignment::OpLeft,
    ) -> Result<Option<Truncate>, ExecutionError> {
        use block::execution::assignment::OpLeft;
        //TODO genertic error here
        let error = ExecutionError::BitRangeZero;
        let ass = match input {
            OpLeft::BitRange(range) => {
                let size = NonZeroTypeU::new(range.n_bits).ok_or(error)?;
                Some(Truncate::new(range.lsb_bit, size))
            }
            OpLeft::ByteRangeMsb(msb) => Some(Truncate::new_msb(msb.value)),
            OpLeft::ByteRangeLsb(lsb) => {
                let size = NonZeroTypeU::new(lsb.value).ok_or(error)?;
                Some(Truncate::new_lsb(size))
            }
        };
        Ok(ass)
    }
    fn new_cpu_branch_dst(
        &mut self,
        input: block::execution::branch::BranchDst<'a>,
    ) -> Result<(bool, Expr), ExecutionError> {
        use block::execution::branch::BranchDst::*;
        Ok(match input {
            Label(_) => unreachable!(),
            Cpu { direct, expr } => (direct, self.new_expr(expr)?),
        })
    }
    fn new_cpu_branch(
        &mut self,
        input: block::execution::branch::Branch<'a>,
    ) -> Result<CpuBranch, ExecutionError> {
        let cond = input.cond.map(|x| self.new_expr(x)).transpose()?;
        let call = input.call;
        let (direct, dst) = self.new_cpu_branch_dst(input.dst)?;

        Ok(CpuBranch::new(
            cond,
            call,
            direct,
            dst,
            self.sleigh().exec_addr_size(),
        ))
    }
    fn new_local_goto(
        &mut self,
        input: block::execution::branch::Branch<'a>,
    ) -> Result<LocalGoto, ExecutionError> {
        let cond = input.cond.map(|x| self.new_expr(x)).transpose()?;
        if !matches!(input.call, BranchCall::Goto) {
            return Err(ExecutionError::InvalidLocalGoto);
        }
        let dst = match input.dst {
            block::execution::branch::BranchDst::Label(x) => {
                self.block(x.name).ok_or(ExecutionError::MissingLabel(
                    self.sleigh().input_src(x.name),
                ))?
            }
            _ => unreachable!(),
        };
        Ok(LocalGoto::new(cond, dst)?)
    }
    fn new_expr_element(
        &mut self,
        input: block::execution::expr::ExprElement<'a>,
    ) -> Result<ExprElement, ExecutionError> {
        use block::execution::expr::ExprElement as RawExprElement;
        match input {
            RawExprElement::Value(Value::Number(src, value)) => {
                let src = self.sleigh().input_src(src);
                Ok(ExprElement::Value(ExprValue::new_int(src, value)))
            }
            RawExprElement::Value(Value::Ident(value)) => {
                self.read_scope(value).map(ExprElement::Value)
            }
            RawExprElement::Reference(src, size, value) => {
                let src = self.sleigh().input_src(src);
                let size = size
                    .map(|x| {
                        //TODO non generic error here
                        NonZeroTypeU::new(x.value)
                            .ok_or(ExecutionError::BitRangeZero)
                    })
                    .transpose()?
                    .map(FieldSize::new_bytes);
                let value = match self.read_scope(value)? {
                    ExprValue::Assembly(ass_src, _, ass) => {
                        ExprElement::Reference(
                            src,
                            size.unwrap_or_default(),
                            ReferencedValue::Assembly(ass_src, ass),
                        )
                    }
                    ExprValue::Varnode(src, var) => {
                        match &var.varnode_type {
                            semantic::varnode::VarnodeType::Memory(mem) => {
                                ExprElement::Value(ExprValue::Int(
                                    src,
                                    size.unwrap_or(
                                        mem.space.memory().addr_size(),
                                    ),
                                    mem.offset,
                                ))
                            }
                            semantic::varnode::VarnodeType::BitRange(_)
                            | semantic::varnode::VarnodeType::Context(_) => {
                                return Err(ExecutionError::InvalidRef(
                                    src.clone(/*TODO make better error*/),
                                ));
                            }
                        }
                    }
                    ExprValue::Table(table_src, table) => {
                        ExprElement::Reference(
                            src,
                            size.unwrap_or_default(),
                            ReferencedValue::Table(table_src, table),
                        )
                    }
                    //ExprValue::Param(_, _) => todo!(),
                    _ => {
                        return Err(ExecutionError::InvalidRef(
                            self.sleigh().input_src(value),
                        ))
                    }
                };
                Ok(value)
            }
            RawExprElement::Op(src, raw_op, input) => {
                let src = self.sleigh().input_src(src);
                let op = self.new_op_unary(&raw_op)?;
                let input = self.new_expr(*input)?;
                Ok(ExprElement::new_op(src, op, input))
            }
            RawExprElement::New(src, param0, param1) => {
                let src = self.sleigh().input_src(src);
                let param0 = self.new_expr(*param0).map(Box::new)?;
                let param1 = param1
                    .map(|param| self.new_expr(*param).map(Box::new))
                    .transpose()?;
                Ok(ExprElement::New(src, param0, param1))
            }
            RawExprElement::CPool(src, mut params) => {
                let src = self.sleigh().input_src(src);
                let params = params
                    .drain(..)
                    .map(|param| self.new_expr(param))
                    .collect::<Result<_, _>>()?;
                Ok(ExprElement::CPool(src, params))
            }
            RawExprElement::UserCall(call) => self.new_call_expr(call),
            RawExprElement::Ambiguous1 {
                name,
                param,
                param_src,
            } => {
                //can be one of two possibilities:
                if let Ok(value) = self.read_scope(name) {
                    //first: value with ByteRangeMsb operator
                    Ok(ExprElement::new_op(
                        self.sleigh().input_src(param_src),
                        Unary::Truncate(Truncate::new_msb(param)),
                        Expr::Value(ExprElement::Value(value)),
                    ))
                } else {
                    //second: (user_?)function call with one parameter
                    //we know this is not a primitive function, macro
                    //probably never exports, so this can only be a
                    //user_function
                    self.new_call_expr(block::execution::UserCall::new(
                        name,
                        vec![block::execution::expr::Expr::Value(
                            block::execution::expr::ExprElement::Value(
                                crate::base::Value::Number(param_src, param),
                            ),
                        )],
                    ))
                }
            }
        }
    }
    fn new_expr(
        &mut self,
        input: block::execution::expr::Expr<'a>,
    ) -> Result<Expr, ExecutionError> {
        use block::execution::expr::Expr as RawExpr;
        match input {
            RawExpr::Value(value) => {
                self.new_expr_element(value).map(Expr::Value)
            }
            RawExpr::Op(src, op, left, right) => {
                let src = self.sleigh().input_src(src);
                let left = self.new_expr(*left)?;
                let right = self.new_expr(*right)?;
                Ok(Expr::new_op(src, op, left, right))
            }
        }
    }
    fn new_op_unary(
        &self,
        input: &block::execution::op::Unary<'a>,
    ) -> Result<Unary, ExecutionError> {
        use block::execution::op::Unary as Op;
        let to_nonzero =
            //TODO generic error here
            |x: IntTypeU| NonZeroTypeU::new(x).ok_or(ExecutionError::BitRangeZero);
        let op = match input {
            Op::ByteRangeMsb(x) => Unary::Truncate(Truncate::new_msb(x.value)),
            Op::ByteRangeLsb(x) => {
                Unary::Truncate(Truncate::new_lsb(to_nonzero(x.value)?))
            }
            Op::BitRange(range) => Unary::Truncate(Truncate::new(
                range.lsb_bit,
                to_nonzero(range.n_bits)?,
            )),
            Op::Dereference(x) => {
                self.new_addr_derefence(x).map(Unary::Dereference)?
            }
            Op::Negation => Unary::Negation,
            Op::BitNegation => Unary::BitNegation,
            Op::Negative => Unary::Negative,
            Op::FloatNegative => Unary::FloatNegative,
            Op::Popcount => Unary::Popcount,
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
        Ok(op)
    }
    fn new_addr_derefence(
        &self,
        input: &block::execution::op::AddrDereference<'a>,
    ) -> Result<AddrDereference, ExecutionError> {
        let space = input
            .space
            .as_ref()
            .map(|x| self.space(x.name))
            .unwrap_or_else(|| {
                self.sleigh()
                    .default_space()
                    .ok_or(ExecutionError::DefaultSpace)
            })?;
        //size will be the lsb of size, if specified, otherwise  we can't know
        //the size directly
        let size = match input.size {
            Some(size) => FieldSize::new_bytes(
                NonZeroTypeU::new(size.value)
                    .ok_or(ExecutionError::BitRangeZero)?,
            ),
            None => FieldSize::new_unsized(),
        };
        let src = self.sleigh().input_src(input.src);
        Ok(AddrDereference::new(space, size, src))
    }
}
