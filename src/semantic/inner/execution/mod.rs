use crate::semantic::varnode::Bitrange;
use crate::semantic::GlobalReference;
use core::cell::Cell;
use std::cell::RefCell;
use indexmap::IndexMap;
use std::rc::{Rc, Weak};

use crate::base::IntTypeU;
use crate::semantic;
use crate::semantic::execution::{self, BranchCall, ExecutionError};
use crate::{InputSource, Varnode};

use super::pcode_macro::{Parameter, PcodeMacroInstance};
use super::table::Table;
use super::token::TokenField;
use super::varnode::Context;
use super::{
    disassembly, PcodeMacro, Sleigh, SolverStatus, Space, UserFunction,
};

mod builder;
pub use builder::ExecutionBuilder;

mod expr;
pub use expr::*;

mod op;
pub use op::*;

mod len;
pub use len::*;

pub type FinalExportConst = semantic::execution::ExportConst;
#[derive(Clone, Debug)]
pub enum ExportConst {
    //Int(IntTypeU),
    DisVar(InputSource, Rc<disassembly::Variable>),
    TokenField(GlobalReference<TokenField>),
    Context(GlobalReference<Context>),
    Table(GlobalReference<Table>),
}
impl ExportConst {
    pub fn src(&self) -> &InputSource {
        match self {
            Self::DisVar(src, _) => src,
            Self::TokenField(x) => x.location(),
            Self::Context(x) => x.location(),
            Self::Table(x) => x.location(),
        }
    }
    pub fn convert(self) -> FinalExportConst {
        match self {
            Self::DisVar(_, variable) => {
                FinalExportConst::DisVar(variable.convert())
            }
            Self::TokenField(ass) => {
                FinalExportConst::TokenField(ass.convert_reference())
            }
            Self::Context(ass) => {
                FinalExportConst::Context(ass.convert_reference())
            }
            Self::Table(table) => {
                FinalExportConst::Table(table.convert_reference())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Export {
    Value(Expr),
    Reference(Expr, AddrDereference),
    Const(FieldSize, ExportConst),
}

pub type FinalExport = semantic::execution::Export;
impl Export {
    pub fn new_value(expr: Expr) -> Self {
        Self::Value(expr)
    }
    pub fn new_const(size: FieldSize, value: ExportConst) -> Self {
        Self::Const(size, value)
    }
    pub fn new_reference(mut addr: Expr, space: AddrDereference) -> Self {
        //addr expr is the addr to access the space, so it need to be space
        //addr size
        addr.size_mut()
            .update_action(|size| {
                size.intersection(
                    FieldSize::new_bytes(space.space.element().addr_bytes())
                )
            })
            .unwrap(/*TODO*/);
        Self::Reference(addr, space)
    }
    pub fn return_type(&self) -> ExecutionExport {
        match self {
            Export::Value(value) => ExecutionExport::Value(value.size()),
            Export::Reference(_addr, space) => {
                ExecutionExport::Reference(*space.output_size())
            }
            Export::Const(size, _) => ExecutionExport::Const(*size),
        }
    }
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Value(expr) | Export::Reference(expr, _) => expr.src(),
            Self::Const(_, con) => con.src(),
        }
    }
    pub fn output_size(&self) -> FieldSize {
        match self {
            Self::Value(expr) => expr.size(),
            //TODO verify this
            Self::Reference(_, deref) => *deref.output_size(),
            Self::Const(size, _) => *size,
        }
    }
    pub fn output_size_mut<'a>(&'a mut self) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Value(expr) => expr.size_mut(),
            //TODO verify this
            Self::Reference(_, deref) => {
                Box::new(FieldSizeMutRef::from(deref.output_size_mut()))
            }
            Self::Const(size, _) => Box::new(FieldSizeMutRef::from(size)),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        match self {
            Self::Const(_, _) => Ok(()),
            Self::Value(expr) => expr.solve(solved),
            Self::Reference(addr, deref) => {
                addr.solve(solved)?;
                deref.solve(solved);
                if addr.size().is_undefined() {
                    solved.iam_not_finished_location(
                        addr.src(),
                        file!(),
                        line!(),
                    );
                }
                Ok(())
            }
        }
    }
    pub fn convert(self) -> FinalExport {
        match self {
            Self::Const(size, value) => FinalExport::Const(
                size.possible_value().unwrap(),
                value.convert(),
            ),
            Self::Value(expr) => FinalExport::Value(expr.convert()),
            Self::Reference(expr, deref) => {
                FinalExport::Reference(expr.convert(), deref.convert())
            }
        }
    }
}

pub type FinalMemWrite = semantic::execution::MemWrite;
#[derive(Clone, Debug)]
pub struct MemWrite {
    pub addr: Expr,
    mem: AddrDereference,
    src: InputSource,
    pub right: Expr,
}
impl MemWrite {
    pub fn new(
        mut addr: Expr,
        mem: AddrDereference,
        src: InputSource,
        mut right: Expr,
    ) -> Self {
        //HACK: unexplained exeptions:
        //if the mem size is 1byte and value produced by right is 1bit, auto
        //zext it
        if mem
            .size
            .final_value()
            .map(|size| size.get() == 8)
            .unwrap_or(false)
            && right
                .size()
                .final_value()
                .map(|size| size.get() == 1)
                .unwrap_or(false)
        {
            right = Expr::Value(ExprElement::new_op(
                right.src().clone(),
                Unary::Zext,
                right,
            ))
        }

        //addr expr is the addr to access the space, so it need to be space
        //addr size
        addr.size_mut()
            .set(FieldSize::new_bytes(mem.space.element().addr_bytes()));
        Self {
            addr,
            mem,
            src,
            right,
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let error = || ExecutionError::VarSize(self.src.clone());

        self.right.solve(solved)?;
        //exception in case the right size is smaller then the left size,
        //truncate the right size with a msb(0)
        let left_size = self.mem.size.final_value();
        let right_size = self.right.size().final_value();
        if left_size
            .zip(right_size)
            .map(|(left, right)| left.get() < right.get())
            .unwrap_or(false)
        {
            //dummy value
            let mut taken = Expr::Value(ExprElement::Value(ExprValue::Int(
                self.src.clone(),
                FieldSize::new_unsized(),
                0.try_into().unwrap(),
            )));
            std::mem::swap(&mut self.right, &mut taken);
            self.right = Expr::Value(ExprElement::Truncate(
                taken.src().clone(),
                Truncate::new(0, left_size.unwrap()),
                Box::new(taken),
            ));
            self.right.solve(solved)?;
            solved.i_did_a_thing()
        }

        //if the left side is WriteAddr without a size, the only option is to
        //use the left side size
        if let Some(write_size) = self.mem.size.final_value() {
            //if left side size is known, right side will need to produce a
            //a value with size equal or smaller then that
            let modified = self.right.size_mut().update_action(|size| {
                size.set_max(write_size)?.set_possible_value(write_size)
            });
            if modified
                .ok_or_else(|| ExecutionError::VarSize(self.mem.src.clone()))?
            {
                solved.i_did_a_thing();
            }
        } else {
            //if the left side is WriteAddr without a size, the only option is
            //to use the right side size
            let modified = self
                .mem
                .size
                .update_action(|size| size.intersection(self.right.size()));
            if modified.ok_or_else(error)? {
                solved.i_did_a_thing();
            }
        }

        if self.addr.size().is_undefined() || self.right.size().is_undefined() {
            solved.iam_not_finished_location(self.right.src(), file!(), line!())
        }

        Ok(())
    }
    pub fn convert(self) -> FinalMemWrite {
        FinalMemWrite {
            addr: self.addr.convert(),
            mem: self.mem.convert(),
            right: self.right.convert(),
        }
    }
}

pub type FinalAssignment = semantic::execution::Assignment;
#[derive(Clone, Debug)]
pub struct Assignment {
    pub var: WriteValue,
    op: Option<Truncate>,
    src: InputSource,
    pub right: Expr,
}

impl Assignment {
    pub fn new(
        var: WriteValue,
        op: Option<Truncate>,
        src: InputSource,
        right: Expr,
    ) -> Self {
        Self {
            var,
            op,
            src,
            right,
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let error_src = self.right.src().clone();
        let error = || ExecutionError::VarSize(error_src.clone());

        self.right.solve(solved)?;

        //exception in case the right result is 1 bit and the left is
        //less then 1 byte, auto add zext the right side
        let left_size = self
            .op
            .as_ref()
            .map(|op| *op.output_size())
            .unwrap_or(self.var.size())
            .final_value();
        let right_size = self.right.size().final_value();
        if left_size
            .zip(right_size)
            .map(|(left, right)| left.get() == 8 && right.get() < 8)
            .unwrap_or(false)
        {
            let mut taken = Expr::Value(ExprElement::Value(ExprValue::Int(
                self.src.clone(),
                FieldSize::new_unsized(),
                0.try_into().unwrap(),
            )));
            std::mem::swap(&mut self.right, &mut taken);
            self.right = Expr::Value(ExprElement::Op(
                taken.src().clone(),
                FieldSize::new_bits(left_size.unwrap()),
                Unary::Zext,
                Box::new(taken),
            ));
            self.right.solve(solved)?;
            solved.i_did_a_thing()
        }

        //left and right sizes are the same
        if let Some(trunc) = &mut self.op {
            let modified = [
                &mut self.right.size_mut() as &mut dyn FieldSizeMut,
                &mut FieldSizeMutRef::from(trunc.output_size_mut()),
            ]
            .all_same_lenght();
            if modified.ok_or_else(error)? {
                solved.i_did_a_thing()
            }
        } else {
            let modified = [
                &mut self.right.size_mut() as &mut dyn FieldSizeMut,
                &mut self.var.size_mut() as &mut dyn FieldSizeMut,
            ]
            .all_same_lenght();

            //if right size is possible min, so does left if size is not defined
            if self.var.size().is_undefined()
                && self.right.size().possible_min()
            {
                if self
                    .var
                    .size_mut()
                    .update_action(|size| Some(size.set_possible_min()))
                    .unwrap()
                {
                    solved.i_did_a_thing();
                }
            }
            if modified.ok_or_else(error)? {
                solved.i_did_a_thing()
            }
        }

        if self.var.size().is_undefined() || self.right.size().is_undefined() {
            solved.iam_not_finished_location(self.right.src(), file!(), line!())
        }

        Ok(())
    }
    pub fn convert(self) -> FinalAssignment {
        FinalAssignment {
            var: self.var.convert(),
            op: self.op.map(|op| op.convert()),
            right: self.right.convert(),
        }
    }
}

pub type FinalWriteValue = execution::WriteValue;
#[derive(Clone, Debug)]
pub enum WriteValue {
    Varnode(GlobalReference<Varnode>),
    Bitrange(GlobalReference<Bitrange>),
    Table(GlobalReference<Table>),
    TokenField(GlobalReference<TokenField>),
    ExeVar(InputSource, Rc<Variable>),
    Param(InputSource, Rc<Parameter>),
}

impl WriteValue {
    pub fn size(&self) -> FieldSize {
        match self {
            Self::Varnode(var) => {
                FieldSize::new_bytes(var.element().len_bytes)
            }
            Self::Bitrange(var) => {
                FieldSize::new_bits(var.element().range.len_bits())
            }
            Self::Table(value) => *value
                .element()
                .export()
                .borrow()
                .as_ref()
                .unwrap()
                .size()
                .unwrap(),
            Self::TokenField(ass) => ass.element().exec_value_len(),
            Self::ExeVar(_, var) => var.len().get(),
            Self::Param(_, param) => param.size().get(),
        }
    }
    pub fn size_mut<'a>(&'a mut self) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Varnode(var) => Box::new(FieldSizeMutOwned::from(
                FieldSize::new_bytes(var.element().len_bytes),
            )),
            Self::Bitrange(var) => Box::new(FieldSizeMutOwned::from(
                FieldSize::new_bits(var.element().range.len_bits()),
            )),
            Self::Table(value) => Box::new(value.element()),
            Self::ExeVar(_, var) => Box::new(var.len()),
            //NOTE token_field is only valid if it contains attach to variable
            Self::TokenField(ass) => Box::new(FieldSizeMutOwned::from(
                ass.element().exec_value_len(),
            )),
            Self::Param(_, param) => Box::new(param.size()),
        }
    }
    pub fn solve(
        &self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        match self {
            //NOTE call table solve on the main loop only
            Self::Varnode(_)
            | Self::Bitrange(_)
            | Self::Param(_, _)
            | Self::TokenField(_)
            | Self::Table(_) => Ok(()),
            Self::ExeVar(_, var) => var.solve(solved),
        }
    }
    pub fn convert(self) -> FinalWriteValue {
        match self {
            Self::Varnode(var) => FinalWriteValue::Varnode(var),
            Self::Bitrange(var) => FinalWriteValue::Bitrange(var),
            Self::TokenField(ass) => {
                FinalWriteValue::TokenField(ass.convert_reference())
            }
            Self::Table(table) => {
                FinalWriteValue::TableExport(table.convert_reference())
            }
            Self::ExeVar(_, var) => FinalWriteValue::Local(var.convert()),
            //TODO param need to change? maybe replace it into a local variable
            Self::Param(_, param) => FinalWriteValue::Local(param.convert()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Build {
    pub table: GlobalReference<Table>,
}
impl Build {
    pub fn new(table: GlobalReference<Table>) -> Self {
        Self { table }
    }
    pub fn convert(self) -> semantic::execution::Build {
        semantic::execution::Build {
            table: self.table.convert_reference(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MacroCall {
    function: GlobalReference<PcodeMacro>,
    instance: Option<Rc<PcodeMacroInstance>>,
    pub params: Vec<Expr>,
}

impl MacroCall {
    pub fn new(
        params: Vec<Expr>,
        function: GlobalReference<PcodeMacro>,
    ) -> Self {
        Self {
            params,
            function,
            instance: None,
        }
    }
    pub fn solve<T>(&mut self, solved: &mut T) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        let mut correlated_params_instance = |params| {
            for (param, macro_param) in self.params.iter_mut().zip(params) {
                let src = param.src().clone();
                if param
                    .size_mut()
                    .update_action(|size| size.intersection(macro_param))
                    .ok_or(ExecutionError::VarSize(src))?
                {
                    solved.we_did_a_thing();
                }
                param.solve(solved)?;
            }
            Ok(())
        };
        //update the param expencted size with the macro expected param size
        //and solved it
        if let Some(instance) = &self.instance {
            let mut iter = instance.params().iter().map(|x| x.size().get());
            let iter: &mut dyn Iterator<Item = FieldSize> = &mut iter;
            correlated_params_instance(iter)?;
            instance.solve(solved).unwrap(/*TODO*/);
        } else {
            let element = self.function.element();
            let default_params = element.params();
            let mut iter = default_params.iter().map(|x| x.size().get());
            let iter: &mut dyn Iterator<Item = FieldSize> = &mut iter;
            correlated_params_instance(iter)?;
            //try to specialize the macro call
            self.function.element().solve(solved).unwrap(/*TODO*/);
            let params_size = self
                .params
                .iter()
                .map(|x| x.size().final_value())
                .collect::<Option<Vec<_>>>();
            if let Some(params_size) = params_size {
                self.instance =
                    Some(self.function.element().specialize(&params_size));
                if self.instance.is_none() {
                    solved.iam_not_finished_location(
                        &self.function.src,
                        file!(),
                        line!(),
                    )
                } else {
                    solved.i_did_a_thing();
                }
            }
        }
        Ok(())
    }
    pub fn convert(self) -> semantic::execution::MacroCall {
        let params = self.params.into_iter().map(|x| x.convert()).collect();
        semantic::execution::MacroCall::new(
            params,
            self.instance.unwrap().convert(),
        )
    }
}

#[derive(Clone, Debug)]
pub struct UserCall {
    pub function: GlobalReference<UserFunction>,
    pub params: Vec<Expr>,
}
impl std::ops::Deref for UserCall {
    type Target = GlobalReference<UserFunction>;
    fn deref(&self) -> &Self::Target {
        &self.function
    }
}
impl UserCall {
    pub fn new(
        mut params: Vec<Expr>,
        function: GlobalReference<UserFunction>,
    ) -> Self {
        //TODO how to handle user functions with variable number of parameter???
        //function.set_param_num(params.len()).unwrap(/*TODO*/);

        //params size is not very relevant
        params.iter_mut().for_each(|param| {
            //TODO improve the size speculation
            param
                .size_mut()
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        Self { params, function }
    }
    pub fn src(&self) -> &InputSource {
        self.function.location()
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        self.params
            .iter_mut()
            .map(|x| x.solve(solved))
            .collect::<Result<_, _>>()
    }
    pub fn convert(self) -> semantic::execution::UserCall {
        let params = self.params.into_iter().map(|x| x.convert()).collect();
        semantic::execution::UserCall::new(params, self.function)
    }
}

#[derive(Clone, Debug)]
pub struct LocalGoto {
    pub cond: Option<Expr>,
    pub dst: Rc<Block>,
}
impl LocalGoto {
    pub fn new(
        mut cond: Option<Expr>,
        dst: Rc<Block>,
    ) -> Result<Self, ExecutionError> {
        //condition can have any size, preferencially 1 bit for true/false
        cond.iter_mut().for_each(|cond| {
            cond.size_mut()
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        Ok(LocalGoto { cond, dst })
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        if let Some(cond) = self.cond.as_mut() {
            cond.solve(solved)?;
            if cond.size().is_undefined() {
                solved.iam_not_finished_location(cond.src(), file!(), line!());
            }
        }
        Ok(())
    }
    pub fn convert(self) -> semantic::execution::LocalGoto {
        let cond = self.cond.map(|x| x.convert());
        let dst = Rc::clone(&self.dst.result);
        semantic::execution::LocalGoto { cond, dst }
    }
}

#[derive(Clone, Debug)]
pub struct CpuBranch {
    pub cond: Option<Expr>,
    pub call: BranchCall,
    //TODO delete direct?
    direct: bool,
    pub dst: Expr,
    //TODO: HACK: this this is not adequated, it requires that the addr size
    //being deduced some how
    exec_addr_size: FieldSize,
}

impl CpuBranch {
    pub fn new(
        mut cond: Option<Expr>,
        call: BranchCall,
        direct: bool,
        dst: Expr,
        exec_addr_size: FieldSize,
    ) -> Self {
        //condition can have any size, preferencially 1 bit for true/false
        cond.iter_mut().for_each(|cond| {
            cond.size_mut()
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        CpuBranch {
            cond,
            call,
            direct,
            dst,
            exec_addr_size,
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let mut modified = false;
        if let Some(cond) = self.cond.as_mut() {
            cond.solve(solved)?;
            if cond.size().is_undefined() {
                solved.iam_not_finished_location(cond.src(), file!(), line!());
            }
        }
        //correlate the addr execution size and the jmp dest addr size
        let error = ExecutionError::VarSize(self.dst.src().clone());
        modified |= [
            &mut self.dst.size_mut() as &mut dyn FieldSizeMut,
            &mut FieldSizeMutRef::from(&mut self.exec_addr_size),
        ]
        .all_same_lenght()
        .ok_or_else(|| error)?;

        self.dst.solve(solved)?;
        if self.dst.size().is_undefined() {
            solved.iam_not_finished_location(self.dst.src(), file!(), line!());
        }

        if modified {
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(self) -> semantic::execution::CpuBranch {
        let cond = self.cond.map(|cond| cond.convert());
        let dst = self.dst.convert();
        semantic::execution::CpuBranch::new(cond, self.call, self.direct, dst)
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Delayslot(IntTypeU),
    Export(Export),
    CpuBranch(CpuBranch),
    LocalGoto(LocalGoto),
    MacroCall(MacroCall),
    UserCall(UserCall),
    Build(Build),
    Declare(Rc<Variable>),
    Assignment(Assignment),
    MemWrite(MemWrite),
}

impl Statement {
    pub fn solve<T>(&mut self, solved: &mut T) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        match self {
            Self::Build(_x) => (),
            Self::Delayslot(_) => (),
            Self::Export(x) => x.solve(solved)?,
            Self::Declare(_x) => (),
            Self::CpuBranch(x) => x.solve(solved)?,
            Self::LocalGoto(x) => x.solve(solved)?,
            Self::MacroCall(x) => x.solve(solved)?,
            Self::UserCall(x) => x.solve(solved)?,
            Self::Assignment(x) => x.solve(solved)?,
            Self::MemWrite(x) => x.solve(solved)?,
        }
        Ok(())
    }
    pub fn convert(self) -> semantic::execution::Statement {
        use semantic::execution::Statement as New;
        match self {
            Self::Delayslot(x) => New::Delayslot(x),
            Self::Export(x) => New::Export(x.convert()),
            Self::CpuBranch(x) => New::CpuBranch(x.convert()),
            Self::LocalGoto(x) => New::LocalGoto(x.convert()),
            Self::MacroCall(x) => New::MacroCall(x.convert()),
            Self::UserCall(x) => New::UserCall(x.convert()),
            Self::Build(x) => New::Build(x.convert()),
            Self::Declare(x) => New::Declare(x.convert()),
            Self::Assignment(x) => New::Assignment(x.convert()),
            Self::MemWrite(x) => New::MemWrite(x.convert()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    //None is entry block
    //pub name: Option<Rc<str>>,
    // empty means the entry point
    //parent: RefCell<Vec<Weak<Block>>>,
    // None means the block is an return
    pub next: RefCell<Option<Rc<Block>>>,
    pub statements: RefCell<Vec<Statement>>,

    pub result: Rc<semantic::execution::Block>,
}

impl Block {
    pub fn new_empty(name: Option<Rc<str>>) -> Self {
        Block {
            //name,
            next: RefCell::new(None),
            statements: RefCell::new(vec![]),
            result: Rc::new(semantic::execution::Block::new_empty(name)),
        }
    }

    pub fn solve<T>(&self, solved: &mut T) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        self.statements
            .borrow_mut()
            .iter_mut()
            .map(|statements| statements.solve(solved))
            .collect::<Result<(), _>>()
    }
    pub fn convert(&self) -> Rc<semantic::execution::Block> {
        let next = self.next.borrow().as_ref().map(|x| Rc::clone(x));
        if let Some(next_block) = next {
            *self.result.next.borrow_mut() =
                Some(Rc::clone(&next_block.result));
        }

        let statements: Vec<_> =
            std::mem::take(self.statements.borrow_mut().as_mut());
        let statements = statements
            .into_iter()
            .map(|statement| statement.convert())
            .collect();
        *self.result.statements.borrow_mut() = statements;
        Rc::clone(&self.result)
    }
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: Rc<str>,
    //pub scope: RefCell<VariableScope>,
    pub size: Cell<FieldSize>,
    //location of the variable declaration
    pub src: InputSource,
    me: Weak<Self>,

    pub result: RefCell<Option<Rc<semantic::execution::Variable>>>,
}

impl Variable {
    pub fn new(name: Rc<str>, src: InputSource) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name,
            //scope,
            size: Cell::default(),
            src,
            me: Weak::clone(me),
            result: RefCell::default(),
        })
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    fn len(&self) -> &Cell<FieldSize> {
        &self.size
    }
    fn solve<T: SolverStatus>(
        &self,
        solved: &mut T,
    ) -> Result<(), ExecutionError> {
        if self.size.get().is_undefined() {
            solved.iam_not_finished_location(&self.src, file!(), line!())
        }
        Ok(())
    }
    //fn eval_time(&self) -> EvaluationTime {
    //    todo!();
    //}
    fn convert(&self) -> Rc<semantic::execution::Variable> {
        if self.result.borrow().is_none() {
            let mut result = self.result.borrow_mut();
            *result = Some(Rc::new(semantic::execution::Variable::new(
                Rc::clone(&self.name),
            )));
        }
        self.result.borrow().as_ref().map(Rc::clone).unwrap()
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ExecutionExport {
    //don't return
    #[default]
    None,
    //type can't be defined yet.
    //Undefined(FieldSize),
    //value that is known at Dissassembly time
    Const(FieldSize),
    //value that can be know at execution time
    Value(FieldSize),
    //References/registers and other mem locations, all with the same size
    Reference(FieldSize),
    //multiple source, can by any kind of return, value or address,
    //but all with the same size
    Multiple(FieldSize),
}

impl ExecutionExport {
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
    pub fn combine(self, other: Self) -> Option<Self> {
        match (self, other) {
            //if both return nothing, the result is to return nothing
            (Self::None, Self::None) => Some(Self::None),
            //two const values, keep the type
            (Self::Const(one), Self::Const(other)) => {
                one.intersection(other).map(Self::Const)
            }
            //const and value, become value
            (Self::Const(con), Self::Value(value))
            | (Self::Value(value), Self::Const(con)) => {
                con.intersection(value).map(Self::Value)
            }
            //const can't be combined with anything else
            (Self::Const(_), _) | (_, Self::Const(_)) => None,
            //one return nothing and the other return something is invalid
            (Self::None, _) | (_, Self::None) => None,
            //if one is multiple, it consumes any other value type
            (Self::Multiple(one), other) | (other, Self::Multiple(one)) => {
                one.intersection(*other.size().unwrap()).map(Self::Multiple)
            }
            //two values, keep the same type
            (Self::Value(one), Self::Value(other)) => {
                one.intersection(other).map(Self::Value)
            }
            //two references from the same address space, keep the same type
            (Self::Reference(one), Self::Reference(other)) => {
                one.intersection(other).map(Self::Reference)
            }
            //any other combination is is just multiple
            (one, other) => one
                .size()
                .unwrap()
                .intersection(*other.size().unwrap())
                .map(Self::Multiple),
        }
    }
    //TODO from Option into a Result
    pub fn convert(&self) -> Option<crate::semantic::table::ExecutionExport> {
        use crate::semantic::table;
        let result = match self {
            Self::None => table::ExecutionExport::None,
            Self::Const(len) if !len.is_undefined() => {
                table::ExecutionExport::Const(len.possible_value().unwrap())
            }
            Self::Value(len) if !len.is_undefined() => {
                table::ExecutionExport::Value(len.possible_value().unwrap())
            }
            Self::Reference(len) if !len.is_undefined() => {
                table::ExecutionExport::Reference(len.possible_value().unwrap())
            }
            Self::Multiple(len) if !len.is_undefined() => {
                table::ExecutionExport::Multiple(len.possible_value().unwrap())
            }
            _ => return None,
        };
        Some(result)
    }
}

#[derive(Clone, Debug)]
pub struct Execution {
    src: InputSource,
    pub blocks: IndexMap<Rc<str>, Rc<Block>>,
    pub vars: IndexMap<Rc<str>, Rc<Variable>>,

    pub return_value: ExecutionExport,

    //entry_block have no name and is not on self.labels
    pub entry_block: Rc<Block>,
}

pub type FinalExecution = semantic::execution::Execution;
impl Drop for Execution {
    fn drop(&mut self) {
        for block in self.blocks() {
            //avoid circle references in case Block point to one another
            let mut next = block.next.borrow_mut();
            //clean statements to delete Rc<Block> from branchs
            let mut statements = block.statements.borrow_mut();
            *next = None;
            *statements = vec![];
        }
    }
}

impl Execution {
    pub fn src(&self) -> &InputSource {
        &self.src
    }
    pub fn new_empty(src: &InputSource) -> Self {
        let entry_block = Rc::new(Block::new_empty(None));
        Execution {
            src: src.clone(),
            blocks: IndexMap::default(),
            vars: IndexMap::default(),
            return_value: ExecutionExport::default(),
            entry_block,
        }
    }
    pub fn solve<T>(&mut self, solved: &mut T) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        self.blocks()
            .map(|blocks| blocks.solve(solved))
            .collect::<Result<(), _>>()?;

        //get the export sizes, otherwise we are finished
        let mut return_size = if let Some(size) = self.return_size().cloned() {
            size
        } else {
            return Ok(());
        };
        //find and combine all the output sizes
        let mut modified = self
            .blocks()
            .filter(|block| block.next.borrow().is_none())
            .filter_map(|block| match block.statements.borrow().last()? {
                Statement::Export(exp) => Some(exp.output_size()),
                _ => None,
            })
            .try_fold(false, |acc, out_size| {
                return_size
                    .update_action(|size| size.intersection(out_size))
                    .map(|modified| acc | modified)
                    .ok_or_else(|| ExecutionError::InvalidExport)
            })?;
        //update all the export output sizes
        self.blocks()
            .filter(|block| block.next.borrow().is_none())
            .for_each(|block| {
                let mut statements = block.statements.borrow_mut();
                match statements.last_mut() {
                    Some(Statement::Export(export)) => {
                        modified |= export
                            .output_size_mut()
                            .update_action(|size| {
                                size.intersection(return_size)
                            })
                            .unwrap();
                    }
                    _ => (),
                }
            });
        modified |= self
            .return_size_mut()
            .unwrap()
            .update_action(|size| size.intersection(return_size))
            .unwrap();
        if modified {
            solved.i_did_a_thing();
        }
        if return_size.is_undefined() {
            solved.iam_not_finished_location(&self.src, file!(), line!());
        }
        Ok(())
    }
    pub fn convert(mut self) -> FinalExecution {
        let blocks = std::mem::take(&mut self.blocks);
        let vars = std::mem::take(&mut self.vars);
        let entry_block = self.entry_block.convert();
        let blocks = blocks
            .into_iter()
            .map(|(name, block)| (name, block.convert()))
            .collect();
        let vars = vars
            .into_iter()
            .map(|(name, var)| (name, var.convert()))
            .collect();
        FinalExecution {
            blocks,
            vars,
            entry_block,
        }
    }
    pub fn return_size(&self) -> Option<&FieldSize> {
        self.return_value.size()
    }
    pub fn return_size_mut(&mut self) -> Option<&mut FieldSize> {
        self.return_value.size_mut()
    }
    pub fn return_type(&self) -> &ExecutionExport {
        &self.return_value
    }
    pub fn return_type_mut(&mut self) -> &mut ExecutionExport {
        &mut self.return_value
    }
    pub fn blocks(&self) -> impl Iterator<Item = &Block> {
        self.blocks
            .values()
            .map(Rc::as_ref)
            .chain([Rc::as_ref(&self.entry_block)])
    }
    pub fn block(&self, name: &str) -> Option<Rc<Block>> {
        self.blocks.get(name).map(|x| Rc::clone(x))
    }
    pub fn new_block(&mut self, name: &str) -> Option<Rc<Block>> {
        let name = Rc::from(name);
        let block = Rc::new(Block::new_empty(Some(Rc::clone(&name))));
        self.blocks
            .insert(name, Rc::clone(&block))
            .map_or(Some(block), |_| None)
    }
    pub fn entry_block(&self) -> &Block {
        &self.entry_block
    }
    pub fn variable(&self, name: &str) -> Option<&Rc<Variable>> {
        self.vars.get(name)
    }
    pub fn create_variable(
        &mut self,
        name_ori: &str,
        src: InputSource,
        //scope: VariableScope,
    ) -> Result<Rc<Variable>, ExecutionError> {
        let name = Rc::from(name_ori);
        //TODO src
        let var = Variable::new(name, src.clone());
        self.vars
            .insert(Rc::clone(&var.name), Rc::clone(&var))
            .map_or(Ok(var), |_| Err(ExecutionError::InvalidRef(src)))
    }
}
