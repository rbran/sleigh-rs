use std::rc::Rc;

use crate::semantic::inner::execution::{
    AddrDereference, BranchCall, ExecutionError, Unary,
};
use crate::semantic::inner::{GlobalScope, Table};
use crate::semantic::GlobalElement;
use crate::syntax::{block, Value};
use crate::{Number, NumberNonZeroUnsigned};

use super::*;

pub trait ExecutionBuilder {
    fn sleigh(&self) -> &Sleigh;
    fn execution(&self) -> &Execution;
    fn execution_mut(&mut self) -> &mut Execution;
    fn read_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ExprValue, ExecutionError>;
    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteValue, ExecutionError>;
    fn table<'b>(
        &'b self,
        name: &str,
        src: &Span,
    ) -> Result<&'b GlobalElement<Table>, ExecutionError> {
        self.sleigh()
            .get_global(name)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
            .unwrap_table()
            .ok_or(ExecutionError::InvalidRef(src.clone()))
    }
    fn space<'b>(
        &'b self,
        name: &str,
        src: &Span,
    ) -> Result<&'b GlobalElement<Space>, ExecutionError> {
        self.sleigh()
            .get_global(name)
            .ok_or(ExecutionError::MissingRef(src.clone()))?
            .unwrap_space()
            .ok_or(ExecutionError::InvalidRef(src.clone()))
    }
    fn current_block(&self) -> Rc<Block>;
    fn current_block_mut(&mut self) -> &mut Rc<Block>;
    fn block(&self, name: &str) -> Option<Rc<Block>> {
        self.execution().block(name)
    }
    fn create_block(&mut self, name: &str) -> Option<Rc<Block>> {
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
        name: &str,
        src: &Span,
    ) -> Result<Rc<Variable>, ExecutionError> {
        let var = self.execution_mut().create_variable(name, src.clone())?;
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
        input: block::execution::Execution,
    ) -> Result<(), ExecutionError> {
        //start by creating all the blocks
        for statement in input.statements.iter() {
            match statement {
                block::execution::Statement::Label(label) => {
                    self.create_block(&label.name).ok_or_else(|| {
                        ExecutionError::DuplicatedLabel(label.src.clone())
                    })?;
                }
                _ => (),
            }
        }

        //convert all the other statements
        for statement in input.statements.into_iter() {
            match statement {
                block::execution::Statement::Label(x) => {
                    //finding label means changing block
                    let new_current_block = self.block(&x.name).unwrap();
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
                    let var = self.create_variable(&x.name, &x.src)?;
                    if let Some(size) = x.size {
                        let size = NumberNonZeroUnsigned::new(size.value)
                            .map(FieldSize::new_bytes)
                            .ok_or_else(|| {
                                ExecutionError::InvalidVarLen(size.src)
                            })?;
                        var.len().set(size);
                    }
                }
                block::execution::Statement::Assignment(x) => {
                    let assignment = self.new_assignment(x)?;
                    self.insert_statement(assignment);
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
        let return_type = {
            let mut iter = self
                .execution()
                .blocks()
                //only blocks with no next block can export
                .filter(|block| block.next.borrow().is_none())
                //if the last statement is export, convert to export size
                .filter_map(|block| match block.statements.borrow().last() {
                    Some(Statement::Export(exp)) => Some(exp.return_type()),
                    _ => None,
                });
            //FUTURE: replace this with try_reduce:
            //`.try_reduce(|acc, item| acc.combine(item));`
            match iter.next() {
                Some(first) => iter
                    .try_fold(first, |acc, item| acc.combine(item))
                    .map(|x| Some(x)),
                None => Some(None),
            }
        };
        self.execution_mut().return_value = match return_type {
            //short circuit, AKA invalid combination of return types
            None => return Err(ExecutionError::InvalidExport),
            //there are no returns
            Some(None) => ExecutionExport::None,
            //some return type
            Some(Some(ret)) => ret,
        };
        Ok(())
    }

    fn new_build(
        &mut self,
        input: block::execution::Build,
    ) -> Result<Build, ExecutionError> {
        let table = self.table(&input.table_name, &input.src)?;
        Ok(Build::new(GlobalReference::from_element(
            table,
            input.src.clone(),
        )))
    }
    fn new_export_const(
        &mut self,
        input: &str,
        src: &Span,
    ) -> Result<ExportConst, ExecutionError> {
        use ExprValue::*;
        match self.read_scope(input, src)? {
            DisVar(src, _size, dis) => {
                Ok(ExportConst::DisVar(src, dis.clone()))
            }
            TokenField(_size, ass) => Ok(ExportConst::TokenField(ass.clone())),
            Context(_size, cont) => Ok(ExportConst::Context(cont.clone())),
            Table(table) => match *table.element().export().borrow() {
                Some(ExecutionExport::Const(_export)) => {
                    Ok(ExportConst::Table(table.clone()))
                }
                //TODO more specific error
                //a const export can only use a table that also export const
                Some(_) => Err(ExecutionError::InvalidRef(src.clone())),
                //to constructors are available yet, can't use this table
                None => Err(ExecutionError::InvalidRef(src.clone())),
            },
            x => todo!("Error export invalid const {:#?}", x),
        }
    }
    fn new_export(
        &mut self,
        input: block::execution::export::Export,
    ) -> Result<Export, ExecutionError> {
        use block::execution::export::Export as RawExport;
        match input {
            //RawExportValue::Unique{} => todo!(),
            RawExport::Value(value) => {
                let value = self.new_expr(value)?;
                //if the value is just an varnode, then is actually a reference
                match value {
                    Expr::Value(ExprElement::Value(ExprValue::Varnode(
                        varnode,
                    ))) => Ok(Export::new_reference(
                        Expr::Value(ExprElement::Value(ExprValue::new_int(
                            varnode.location().clone(),
                            Number::Positive(varnode.element().offset),
                        ))),
                        AddrDereference::new(
                            GlobalReference::from_element(
                                &varnode.element().space,
                                varnode.location().clone(),
                            ),
                            FieldSize::new_bytes(varnode.element().len_bytes),
                            varnode.location().clone(),
                        ),
                    )),
                    _ => Ok(Export::new_value(value)),
                }
            }
            RawExport::Reference { space, addr } => {
                let addr = self.new_expr(addr)?;
                let deref = self.new_addr_derefence(&space)?;
                Ok(Export::new_reference(addr, deref))
            }
            RawExport::Const { size, value, src } => {
                let value = self.new_export_const(&value, &src)?;
                let size =
                    NumberNonZeroUnsigned::new(size.value).unwrap(/*TODO*/);
                let size = FieldSize::new_bytes(size);
                Ok(Export::new_const(size, value))
            }
        }
    }
    fn new_call_statement(
        &mut self,
        input: block::execution::UserCall,
    ) -> Result<Statement, ExecutionError> {
        let params = input
            .params
            .into_iter()
            .map(|param| self.new_expr(param))
            .collect::<Result<Vec<_>, _>>()?;
        match self
            .sleigh()
            .get_global(&input.name)
            .ok_or(ExecutionError::MissingRef(input.src.clone()))?
        {
            GlobalScope::UserFunction(x) => {
                Ok(Statement::UserCall(UserCall::new(
                    params,
                    GlobalReference::from_element(x, input.src),
                )))
            }
            GlobalScope::PcodeMacro(x) => {
                Ok(Statement::MacroCall(MacroCall::new(
                    params,
                    GlobalReference::from_element(x, input.src),
                )))
            }
            _ => Err(ExecutionError::InvalidRef(input.src)),
        }
    }
    fn new_call_expr(
        &mut self,
        input: block::execution::UserCall,
    ) -> Result<ExprElement, ExecutionError> {
        let params = input
            .params
            .into_iter()
            .map(|param| self.new_expr(param))
            .collect::<Result<Vec<_>, _>>()?;
        match self
            .sleigh()
            .get_global(&input.name)
            .ok_or_else(|| ExecutionError::MissingRef(input.src.clone()))?
        {
            GlobalScope::UserFunction(x) => Ok(ExprElement::UserCall(
                //TODO better min output handler
                FIELD_SIZE_BOOL,
                UserCall::new(
                    params,
                    GlobalReference::from_element(x, input.src),
                ),
            )),
            GlobalScope::PcodeMacro(x) => {
                todo!("user defined {} exports?", x.name)
            }
            _ => Err(ExecutionError::InvalidRef(input.src)),
        }
    }

    fn new_assignment(
        &mut self,
        input: block::execution::assignment::Assignment,
    ) -> Result<Statement, ExecutionError> {
        let mut right = self.new_expr(input.right)?;
        let var = self.write_scope(&input.ident, &input.src).ok();
        match (var, input.local) {
            //variable don't exists, create it
            (None, _) => {
                //the var size is defined if ByteRangeLsb is present
                //add the var creation statement
                let new_var = self.create_variable(&input.ident, &input.src)?;
                match &input.op {
                    Some(
                        block::execution::assignment::OpLeft::ByteRangeLsb(x),
                    ) => {
                        let new_len = new_var
                            .len()
                            .get()
                            .set_final_value(
                                NumberNonZeroUnsigned::new(x.value * 8)
                                    .unwrap(),
                            )
                            .unwrap();
                        new_var.len().set(new_len);
                    }
                    Some(_) => todo!("create var with this op?"),
                    None => (),
                }
                Ok(Statement::Assignment(Assignment::new(
                    WriteValue::ExeVar(input.src.clone(), new_var),
                    None,
                    input.src,
                    right,
                )))
            }
            //variable exists, with local is error
            (Some(_), true) => {
                Err(ExecutionError::InvalidVarDeclare(input.src))
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
                        let var_ele = var.element();
                        let mem = AddrDereference::new(
                            GlobalReference::from_element(
                                &var_ele.space,
                                var.location().clone(),
                            ),
                            FieldSize::new_bytes(var_ele.len_bytes),
                            var.location().clone(),
                        );
                        let addr = Expr::Value(ExprElement::Value(
                            ExprValue::new_int(
                                var.location().clone(),
                                Number::Positive(var_ele.offset),
                            ),
                        ));
                        right.size_mut().update_action(|size| {
                            size.intersection(FieldSize::new_bytes(
                                var_ele.len_bytes,
                            ))
                        });
                        Ok(Statement::MemWrite(MemWrite::new(
                            addr, mem, input.src, right,
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
        input: block::execution::assignment::MemWrite,
    ) -> Result<MemWrite, ExecutionError> {
        let mem = self.new_addr_derefence(&input.mem)?;
        let addr = self.new_expr(input.addr)?;
        let right = self.new_expr(input.right)?;
        Ok(MemWrite::new(addr, mem, input.src, right))
    }
    fn new_assignment_op(
        &self,
        input: block::execution::assignment::OpLeft,
    ) -> Result<Truncate, ExecutionError> {
        use block::execution::assignment::OpLeft;
        //TODO genertic error here
        let error = ExecutionError::BitRangeZero;
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
        input: block::execution::branch::BranchDst,
    ) -> Result<(bool, Expr), ExecutionError> {
        use block::execution::branch::BranchDst::*;
        Ok(match input {
            Label(_) => unreachable!(),
            Cpu { direct, expr } => (direct, self.new_expr(expr)?),
        })
    }
    fn new_cpu_branch(
        &mut self,
        input: block::execution::branch::Branch,
    ) -> Result<CpuBranch, ExecutionError> {
        let cond = input.cond.map(|x| self.new_expr(x)).transpose()?;
        let call = input.call;
        let (direct, dst) = self.new_cpu_branch_dst(input.dst)?;

        Ok(CpuBranch::new(
            cond,
            call,
            direct,
            dst,
            //TODO error here
            self.sleigh().exec_addr_size().cloned().unwrap(),
        ))
    }
    fn new_local_goto(
        &mut self,
        input: block::execution::branch::Branch,
    ) -> Result<LocalGoto, ExecutionError> {
        let cond = input.cond.map(|x| self.new_expr(x)).transpose()?;
        if !matches!(input.call, BranchCall::Goto) {
            return Err(ExecutionError::InvalidLocalGoto);
        }
        let dst = match input.dst {
            block::execution::branch::BranchDst::Label(x) => self
                .block(&x.name)
                .ok_or_else(|| ExecutionError::MissingLabel(x.src.clone()))?,
            _ => unreachable!(),
        };
        Ok(LocalGoto::new(cond, dst)?)
    }
    fn new_expr_element(
        &mut self,
        input: block::execution::expr::ExprElement,
    ) -> Result<ExprElement, ExecutionError> {
        use block::execution::expr::ExprElement as RawExprElement;
        match input {
            RawExprElement::Value(Value::Number(src, value)) => {
                Ok(ExprElement::Value(ExprValue::new_int(src, value)))
            }
            RawExprElement::Value(Value::Ident(src, value)) => {
                self.read_scope(&value, &src).map(ExprElement::Value)
            }
            RawExprElement::Reference(src, size, value) => {
                let ref_bytes = size
                    .map(|x| {
                        //TODO non generic error here
                        NumberNonZeroUnsigned::new(x.value)
                            .ok_or(ExecutionError::BitRangeZero)
                    })
                    .transpose()?;
                let value = match self.read_scope(&value, &src)? {
                    ExprValue::TokenField(_, ass) => ExprElement::Reference(
                        src,
                        ref_bytes.map(FieldSize::new_bytes).unwrap_or_default(),
                        ReferencedValue::TokenField(ass),
                    ),
                    //TODO What is a reference to inst_start/inst_next? Just the
                    //value?
                    ExprValue::InstStart(len, x) => {
                        let element =
                            ExprElement::Value(ExprValue::InstStart(len, x));
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
                    ExprValue::InstNext(len, x) => {
                        let element =
                            ExprElement::Value(ExprValue::InstNext(len, x));
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
                    ExprValue::Varnode(var) => {
                        ExprElement::Value(ExprValue::Int(
                            src,
                            ref_bytes.map(FieldSize::new_bytes).unwrap_or(
                                FieldSize::new_bytes(
                                    var.element().space.addr_bytes(),
                                ),
                            ),
                            Number::Positive(var.element().offset),
                        ))
                    }
                    ExprValue::Table(table) => ExprElement::Reference(
                        src,
                        ref_bytes.map(FieldSize::new_bytes).unwrap_or_default(),
                        ReferencedValue::Table(table),
                    ),
                    //ExprValue::Param(_, _) => todo!(),
                    _ => return Err(ExecutionError::InvalidRef(src)),
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
                Ok(ExprElement::New(src, param0, param1))
            }
            RawExprElement::CPool(src, params) => {
                let params = params
                    .into_iter()
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
                    self.new_call_expr(block::execution::UserCall::new(
                        name,
                        param_src.clone(),
                        vec![block::execution::expr::Expr::Value(
                            block::execution::expr::ExprElement::Value(
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
        input: block::execution::expr::Expr,
    ) -> Result<Expr, ExecutionError> {
        use block::execution::expr::Expr as RawExpr;
        match input {
            RawExpr::Value(value) => {
                self.new_expr_element(value).map(Expr::Value)
            }
            RawExpr::Op(src, op, left, right) => {
                let left = self.new_expr(*left)?;
                let right = self.new_expr(*right)?;
                Ok(Expr::new_op(src, op, left, right))
            }
        }
    }
    fn new_op_unary(
        &self,
        input: &block::execution::op::Unary,
        src: Span,
        expr: Expr,
    ) -> Result<ExprElement, ExecutionError> {
        use block::execution::op::Unary as Op;
        let to_nonzero =
            //TODO generic error here
            |x: NumberUnsigned| NumberNonZeroUnsigned::new(x).ok_or(ExecutionError::BitRangeZero);
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
                Expr::Value(ExprElement::Value(ExprValue::Int(
                    src,
                    len,
                    value,
                ))) => {
                    return Ok(ExprElement::Value(ExprValue::Int(
                        src,
                        //TODO error
                        len.intersection(FieldSize::new_bytes(
                            NumberNonZeroUnsigned::new(x.value).unwrap(),
                        ))
                        .unwrap(),
                        value,
                    )));
                }
                Expr::Value(ExprElement::Value(ExprValue::DisVar(
                    src,
                    len,
                    value,
                ))) => {
                    return Ok(ExprElement::Value(ExprValue::DisVar(
                        src,
                        //TODO error
                        len.intersection(FieldSize::new_bytes(
                            NumberNonZeroUnsigned::new(x.value).unwrap(),
                        ))
                        .unwrap(),
                        value,
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
        input: &block::execution::op::AddrDereference,
    ) -> Result<AddrDereference, ExecutionError> {
        let space = input
            .space
            .as_ref()
            .map(|x| self.space(&x.name, &x.src))
            .unwrap_or_else(|| {
            self.sleigh()
                .default_space()
                .ok_or(ExecutionError::DefaultSpace)
        })?;
        //size will be the lsb of size, if specified, otherwise  we can't know
        //the size directly
        let size = match input.size.as_ref() {
            Some(size) => FieldSize::new_bytes(
                NumberNonZeroUnsigned::new(size.value)
                    .ok_or(ExecutionError::BitRangeZero)?,
            ),
            None => FieldSize::new_unsized(),
        };
        Ok(AddrDereference::new(
            GlobalReference::from_element(space, input.src.clone()),
            size,
            input.src.clone(),
        ))
    }
}
