use core::cell::Cell;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::base::NonZeroTypeU;
use crate::semantic::pcode_macro::{
    PcodeMacroError, ToPcodeMacroError,
};
use crate::semantic::table::ExecutionError;
use crate::semantic::varnode::VarnodeType;
use crate::syntax::block;
use crate::{semantic, ParamNumber};
use crate::InputSource;

use super::execution::{
    Block, Build, EvaluationTime, Execution, ExecutionBuilder, Expr,
    ExprElement, ExprValue, Statement, WriteValue,
};
use super::{FieldSize, GlobalScope, Sleigh, SolverStatus};

pub type FinalPcodeMacroInstance = semantic::pcode_macro::PcodeMacroInstance;
#[derive(Debug, Clone)]
pub struct PcodeMacroInstance {
    signature: Vec<NonZeroTypeU>,
    pub params: Vec<Rc<Parameter>>,
    pub execution: RefCell<Execution>,
    me: Weak<Self>,
    parent: Weak<PcodeMacro>,
    result: RefCell<Option<Rc<FinalPcodeMacroInstance>>>,
}

impl PcodeMacroInstance {
    fn new_root(
        params: Vec<Rc<Parameter>>,
        execution: RefCell<Execution>,
        parent: Weak<PcodeMacro>,
    ) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            signature: vec![],
            params,
            execution,
            me: Weak::clone(me),
            parent,
            result: RefCell::default(),
        })
    }
    pub fn new(
        params: Vec<Rc<Parameter>>,
        execution: RefCell<Execution>,
        parent: Weak<PcodeMacro>,
    ) -> Rc<Self> {
        let signature = params
            .iter()
            .map(|param| param.size().final_value().unwrap())
            .collect();
        Rc::new_cyclic(|me| Self {
            signature,
            params,
            execution,
            me: Weak::clone(me),
            parent,
            result: RefCell::default(),
        })
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    pub fn parent(&self) -> Rc<PcodeMacro> {
        self.parent.upgrade().unwrap()
    }
    pub fn solve<T>(&self, solved: &mut T) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        self.execution.borrow_mut().solve(solved)
    }
    pub fn specialize_me(&self, params: &[NonZeroTypeU]) -> Rc<Self> {
        let params: Vec<Rc<Parameter>> = self
            .params
            .iter()
            .zip(params)
            .map(|(param, size)| {
                let param = param.clone_me();
                param
                    .update_size(|param| param.set_final_value(*size))
                    .unwrap();
                param
            })
            .collect();
        update_all_statements(&self.execution.borrow(), &self.params, &params);
        Self::new(params, self.execution.clone(), Weak::clone(&self.parent))
    }
    pub fn params(&self) -> &[Rc<Parameter>] {
        &self.params
    }
    pub fn signature(&self) -> &[NonZeroTypeU] {
        &self.signature
    }
    fn convert_from_parent(
        &self,
        parent: &Weak<FinalPcodeMacro>,
    ) -> Rc<FinalPcodeMacroInstance> {
        if let Some(result) = self.result.borrow().as_ref() {
            return Rc::clone(result);
        }
        let exec_src = self.execution.borrow().src().clone();
        let execution = self.execution.replace(Execution::new_empty(&exec_src));
        let final_instance = FinalPcodeMacroInstance {
            //    params,
            execution: execution.convert(),
            parent: Weak::clone(parent),
        };
        *self.result.borrow_mut() = Some(Rc::new(final_instance));
        Rc::clone(&self.result.borrow().as_ref().unwrap())
    }
    pub fn convert(&self) -> Rc<FinalPcodeMacroInstance> {
        let parent = self.parent.upgrade().unwrap().convert();
        self.convert_from_parent(&Rc::downgrade(&parent))
    }
}

pub type FinalPcodeMacro = semantic::pcode_macro::PcodeMacro;
#[derive(Clone, Debug)]
pub struct PcodeMacro {
    pub name: Rc<str>,
    og_instance: Rc<PcodeMacroInstance>,
    instances: RefCell<Vec<Rc<PcodeMacroInstance>>>,
    pub src: InputSource,
    solved: RefCell<bool>,
    me: Weak<Self>,

    pub result: RefCell<Option<Rc<semantic::pcode_macro::PcodeMacro>>>,
    //TODO: export macro is a thing?
}

impl PcodeMacro {
    pub fn new(
        name: &str,
        src: InputSource,
        params: Vec<Rc<Parameter>>,
        execution: RefCell<Execution>,
    ) -> Rc<Self> {
        let name = Rc::from(name);
        Rc::new_cyclic(|me| Self {
            name: Rc::clone(&name),
            instances: RefCell::default(),
            og_instance: PcodeMacroInstance::new_root(
                params,
                execution,
                Weak::clone(me),
            ),
            src,
            solved: RefCell::new(false),
            me: Weak::clone(me),
            result: RefCell::default(),
        })
    }
    pub fn params(&self) -> &[Rc<Parameter>] {
        &self.og_instance.params
    }
    pub fn export_size(&self) -> Option<FieldSize> {
        todo!("capture PcodeMacro return size")
    }
    pub fn range_params(&self) -> ParamNumber {
        let len = self.og_instance.params().len();
        ParamNumber::new(len, Some(len))
    }
    pub fn is_solved(&self) -> bool {
        *self.solved.borrow()
    }
    pub fn og_signature<'a>(&'a self) -> impl Iterator<Item = FieldSize> + 'a {
        self.og_instance.params.iter().map(|param| param.size())
    }
    pub fn try_specialize(
        &self,
        params: &[NonZeroTypeU],
    ) -> Option<Rc<PcodeMacroInstance>> {
        if params.len() != self.og_instance.params().len() {
            panic!("invalid number of param")
        }

        //check if this instance was already created, if so, return it
        if let Some(inst) = self
            .instances
            .borrow()
            .iter()
            .find(|inst| inst.signature() == params)
        {
            return Some(Rc::clone(inst));
        }

        //if not already exists, create a new instance with this signature
        let new_og = self.og_instance.specialize_me(params);
        self.instances.borrow_mut().push(Rc::clone(&new_og));

        //if a new instance was created, then, we are not finished yet, more
        //solving need to happen
        *self.solved.borrow_mut() = false;

        Some(new_og)
    }
    pub fn solve<T: SolverStatus>(
        &self,
        solved: &mut T,
    ) -> Result<(), PcodeMacroError>
    where
        T: SolverStatus + Default,
    {
        //NOTE this makes sure we never call this macro recursivally
        //and stack overflow it
        let mut self_solved_lock = match self.solved.try_borrow_mut() {
            //if unable to get the lock, this is a recursive, so just return
            Err(_) => return Ok(()),
            //this macro is already fully solved, nothing to do
            Ok(x) if *x => return Ok(()),
            //hold the lock to avoid the recursive, and solve the stuff
            Ok(x) => x,
        };
        let mut self_solved = T::default();
        self.instances
            .borrow()
            .iter()
            .chain([&self.og_instance])
            .map(|inst| inst.execution.borrow_mut().solve(&mut self_solved))
            .collect::<Result<_, _>>()
            .to_pcode_macro(self.src.clone())?;

        if self_solved.we_finished() {
            *self_solved_lock = true;
        }
        solved.combine(&self_solved);
        Ok(())
    }
    pub fn convert(&self) -> Rc<FinalPcodeMacro> {
        if let Some(result) = self.result.borrow().as_ref() {
            return Rc::clone(result);
        }
        let final_macro =
            Rc::new(FinalPcodeMacro::new_empty(Rc::clone(&self.name)));
        let weak_final_macro = Rc::downgrade(&final_macro);
        for instance in self.instances.borrow_mut().drain(..) {
            final_macro
                .instances
                .borrow_mut()
                .push(instance.convert_from_parent(&weak_final_macro));
        }
        *self.result.borrow_mut() = Some(final_macro);
        Rc::clone(&self.result.borrow().as_ref().unwrap())
    }
}

pub type FinalParameter = semantic::execution::Variable;
#[derive(Debug)]
pub struct Parameter {
    pub name: Rc<str>,
    src: InputSource,
    me: Weak<Self>,

    size: Cell<FieldSize>,
    time: EvaluationTime,
    result: RefCell<Option<Rc<FinalParameter>>>,
}
impl Parameter {
    pub fn new(name: &str, src: InputSource) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            src,
            me: Weak::clone(me),
            size: Cell::default(),
            time: EvaluationTime::default(),
            result: RefCell::default(),
        })
    }
    pub fn clone_me(&self) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::clone(&self.name),
            src: self.src.clone(),
            me: Weak::clone(me),
            size: self.size.clone(),
            time: self.time,
            result: RefCell::default(),
        })
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    pub fn size_capable(&self) -> &Cell<FieldSize> {
        &self.size
    }
    pub fn size(&self) -> FieldSize {
        self.size.get()
    }
    pub fn update_size<F>(&self, mut action: F) -> Option<bool>
    where
        F: FnMut(FieldSize) -> Option<FieldSize>,
    {
        let old = self.size.get();
        self.size.set(action(old)?);
        Some(self.size.get() != old)
    }
    pub fn convert(&self) -> Rc<FinalParameter> {
        if self.result.borrow().is_none() {
            //create the result here
            let mut result = self.result.borrow_mut();
            *result = Some(Rc::new(FinalParameter::new(Rc::clone(&self.name))));
        }
        let result = self.result.borrow();
        let result = result.as_ref().unwrap();
        Rc::clone(result)
    }
}

#[derive(Clone, Debug)]
pub struct Builder<'a, 'b, 'c> {
    execution: Execution,
    current_block: Rc<Block>,

    sleigh: &'b Sleigh<'a>,
    params: &'c Vec<Rc<Parameter>>,
}

impl<'a, 'b, 'c> Builder<'a, 'b, 'c> {
    fn new(
        sleigh: &'b Sleigh<'a>,
        params: &'c Vec<Rc<Parameter>>,
        src: &InputSource,
    ) -> Self {
        let execution = Execution::new_empty(src);
        let current_block = Rc::clone(&execution.entry_block);
        Self {
            execution,
            current_block,
            sleigh,
            params,
        }
    }
}

impl<'a, 'b, 'c> From<Builder<'a, 'b, 'c>> for Execution {
    fn from(input: Builder<'a, 'b, 'c>) -> Self {
        input.execution
    }
}

impl<'a, 'b, 'c> ExecutionBuilder<'a> for Builder<'a, 'b, 'c> {
    fn sleigh(&self) -> &Sleigh<'a> {
        self.sleigh
    }
    fn execution(&self) -> &Execution {
        &self.execution
    }
    fn execution_mut(&mut self) -> &mut Execution {
        &mut self.execution
    }
    fn read_scope(
        &mut self,
        name: &'a str,
    ) -> Result<ExprValue, ExecutionError> {
        //check local variable
        let src = || self.sleigh.input_src(name);
        self.variable(name)
            .map(|var| ExprValue::ExeVar(src(), Rc::clone(var)))
            .or_else(|| {
                self.params
                    .iter()
                    .find(|param| param.name.as_ref() == name)
                    .map(|x| ExprValue::Param(src(), Rc::clone(x)))
            })
            .map(Result::Ok)
            .unwrap_or_else(|| {
                //at last check the global scope
                use super::GlobalScope::*;
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(ExecutionError::MissingRef(src()))?
                {
                    Assembly(x) => match x.assembly_type {
                        semantic::assembly::AssemblyType::Next(_)
                        | semantic::assembly::AssemblyType::Start(_) => {
                            Ok(ExprValue::Assembly(
                                src(),
                                x.value_size(),
                                Rc::clone(x),
                            ))
                        }
                        _ => Err(ExecutionError::InvalidRef(src())),
                    },
                    Varnode(x) => Ok(ExprValue::Varnode(src(), Rc::clone(x))),
                    _ => Err(ExecutionError::InvalidRef(src())),
                }
            })
    }

    fn write_scope(
        &mut self,
        name: &'a str,
    ) -> Result<WriteValue, ExecutionError> {
        let src = || self.sleigh.input_src(name);
        self.variable(name)
            .map(|var| WriteValue::ExeVar(src(), Rc::clone(var)))
            .or_else(|| {
                self.params
                    .iter()
                    .find(|param| param.name.as_ref() == name)
                    .map(|x| WriteValue::Param(src(), Rc::clone(x)))
            })
            .map(Result::Ok)
            .unwrap_or_else(|| {
                //at last check the global scope
                use super::GlobalScope::*;
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(ExecutionError::MissingRef(src()))?
                {
                    Varnode(varnode)
                        if matches!(
                            varnode.varnode_type,
                            VarnodeType::Memory(_) | VarnodeType::BitRange(_)
                        ) =>
                    {
                        Ok(WriteValue::Varnode(src(), Rc::clone(varnode)))
                    }
                    _ => Err(ExecutionError::InvalidRef(src())),
                }
            })
    }

    fn current_block(&self) -> Rc<Block> {
        Rc::clone(&self.current_block)
    }

    fn set_current_block(&mut self, block: Rc<Block>) {
        self.current_block = block
    }

    fn insert_statement(&mut self, statement: Statement) {
        let mut statements = self.current_block.statements.borrow_mut();
        statements.push(statement);
    }
    //macro have no build statement, so if try to parse it, is always error
    fn new_build(
        &mut self,
        _input: block::execution::Build<'a>,
    ) -> Result<Build, ExecutionError> {
        Err(ExecutionError::MacroBuildInvalid)
    }

    fn current_block_mut(&mut self) -> &mut Rc<Block> {
        &mut self.current_block
    }
}

impl<'a> Sleigh<'a> {
    pub fn create_pcode_macro(
        &mut self,
        pcode: block::pcode_macro::PcodeMacro<'a>,
    ) -> Result<(), PcodeMacroError> {
        let src = self.input_src(pcode.src);
        let params = pcode
            .params
            .iter()
            .map(|name| Parameter::new(name, self.input_src(name)))
            .collect();
        let mut execution = Builder::new(self, &params, &src);
        execution.extend(pcode.body).to_pcode_macro(src.clone())?;
        let execution = RefCell::new(execution.into());

        let pcode_macro = PcodeMacro::new(&pcode.name, src, params, execution);
        self.idents.insert(
            Rc::clone(&pcode_macro.name),
            GlobalScope::PcodeMacro(pcode_macro),
        );
        Ok(())
    }
}

//TODO: make it better then this HACK
fn update_all_statements(
    exec: &Execution,
    old: &[Rc<Parameter>],
    new: &[Rc<Parameter>],
) {
    for block in exec.blocks() {
        let mut block = block.statements.borrow_mut();
        for stat in block.iter_mut() {
            match stat {
                Statement::Delayslot(_)
                | Statement::Build(_)
                | Statement::Declare(_) => (),
                Statement::Export(_) => unreachable!("TODO macro export?"),
                Statement::CpuBranch(super::execution::CpuBranch {
                    cond,
                    dst,
                    ..
                }) => {
                    if let Some(cond) = cond {
                        update_expr(cond, old, new);
                    }
                    update_expr(dst, old, new);
                }
                Statement::LocalGoto(super::execution::LocalGoto {
                    cond,
                    ..
                }) => {
                    if let Some(cond) = cond {
                        update_expr(cond, old, new);
                    }
                }
                Statement::UserCall(super::execution::UserCall {
                    params,
                    ..
                })
                | Statement::MacroCall(super::execution::MacroCall {
                    params,
                    ..
                }) => {
                    for param in params.iter_mut() {
                        update_expr(param, old, new);
                    }
                }
                Statement::Assignment(super::execution::Assignment {
                    var,
                    right,
                    ..
                }) => {
                    update_write_value(var, old, new);
                    update_expr(right, old, new);
                }
                Statement::MemWrite(super::execution::MemWrite {
                    addr,
                    right,
                    ..
                }) => {
                    update_expr(addr, old, new);
                    update_expr(right, old, new);
                }
            }
        }
    }
}
fn update_write_value(
    write: &mut WriteValue,
    old: &[Rc<Parameter>],
    new: &[Rc<Parameter>],
) {
    match write {
        WriteValue::Varnode(_, _)
        | WriteValue::Table(_, _)
        | WriteValue::ExeVar(_, _)
        | WriteValue::Assembly(_, _) => (),
        WriteValue::Param(_, param) => {
            if let Some(new_param) =
                old.iter().enumerate().find_map(|(i, x)| {
                    (Rc::as_ptr(x) == Rc::as_ptr(param)).then(|| i)
                })
            {
                *param = Rc::clone(&new[new_param]);
            }
        }
    }
}
fn update_expr(expr: &mut Expr, old: &[Rc<Parameter>], new: &[Rc<Parameter>]) {
    match expr {
        super::execution::Expr::Value(expr_element) => {
            update_expr_element(expr_element, old, new)
        }
        super::execution::Expr::Op(_, _, _, left, right) => {
            update_expr(left, old, new);
            update_expr(right, old, new);
        }
    }
}
fn update_expr_element(
    expr: &mut ExprElement,
    old: &[Rc<Parameter>],
    new: &[Rc<Parameter>],
) {
    match expr {
        ExprElement::Value(expr) => update_expr_value(expr, old, new),
        ExprElement::CPool(_, params)
        | ExprElement::UserCall(_, super::execution::UserCall { params, .. }) => {
            for param in params.iter_mut() {
                update_expr(param, old, new);
            }
        }
        ExprElement::Reference(_, _, _) => todo!(),
        ExprElement::Op(_, _, _, expr) => update_expr(expr, old, new),
        ExprElement::New(_, param0, param1) => {
            update_expr(param0, old, new);
            if let Some(param1) = param1 {
                update_expr(param1, old, new);
            }
        }
    }
}
fn update_expr_value(
    expr: &mut ExprValue,
    old: &[Rc<Parameter>],
    new: &[Rc<Parameter>],
) {
    match expr {
        ExprValue::Int(_, _, _)
        | ExprValue::DisVar(_, _, _)
        | ExprValue::Assembly(_, _, _)
        | ExprValue::Varnode(_, _)
        | ExprValue::Table(_, _)
        | ExprValue::ExeVar(_, _) => (),
        ExprValue::Param(_, param) => {
            if let Some(new_param) =
                old.iter().enumerate().find_map(|(i, x)| {
                    (Rc::as_ptr(x) == Rc::as_ptr(param)).then(|| i)
                })
            {
                *param = Rc::clone(&new[new_param]);
            }
        }
    }
}
