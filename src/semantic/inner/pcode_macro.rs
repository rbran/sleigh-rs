use crate::semantic::{GlobalElement, GlobalReference};
use core::cell::Cell;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::semantic::pcode_macro::{PcodeMacroError, ToPcodeMacroError};
use crate::semantic::table::ExecutionError;
use crate::syntax::block;
use crate::{semantic, ParamNumber};
use crate::{NumberNonZeroUnsigned, Span};

use super::execution::{
    Block, Build, Execution, ExecutionBuilder, Expr, ExprElement, ExprValue,
    Statement, WriteValue,
};
use super::{FieldSize, GlobalConvert, GlobalScope, Sleigh, SolverStatus};

pub type FinalPcodeMacroInstance = semantic::pcode_macro::PcodeMacroInstance;
#[derive(Debug, Clone)]
pub struct PcodeMacroInstance {
    signature: Vec<NumberNonZeroUnsigned>,
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
            .map(|param| param.size().get().final_value().unwrap())
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
    pub fn specialize_me(&self, params: &[NumberNonZeroUnsigned]) -> Rc<Self> {
        let params: Vec<Rc<Parameter>> = self
            .params
            .iter()
            .zip(params)
            .map(|(param, size)| {
                let param = param.clone_me();
                param
                    .size()
                    .set(param.size().get().set_final_value(*size).unwrap());
                param
            })
            .collect();
        update_all_statements(&self.execution.borrow(), &self.params, &params);
        Self::new(params, self.execution.clone(), Weak::clone(&self.parent))
    }
    pub fn params(&self) -> &[Rc<Parameter>] {
        &self.params
    }
    pub fn signature(&self) -> &[NumberNonZeroUnsigned] {
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
        let final_instance = FinalPcodeMacroInstance::new(
            execution.convert(),
            Weak::clone(parent),
        );
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
    og_instance: Rc<PcodeMacroInstance>,
    instances: RefCell<Vec<Rc<PcodeMacroInstance>>>,
    pub src: Span,
    solved: RefCell<bool>,
    _me: Weak<Self>,

    pub result: RefCell<Option<Rc<semantic::pcode_macro::PcodeMacro>>>,
    //TODO: export macro is a thing?
}

impl PcodeMacro {
    pub fn new(
        src: Span,
        params: Vec<Rc<Parameter>>,
        execution: RefCell<Execution>,
    ) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            instances: RefCell::default(),
            og_instance: PcodeMacroInstance::new_root(
                params,
                execution,
                Weak::clone(me),
            ),
            src,
            solved: RefCell::new(false),
            _me: Weak::clone(me),
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
        self.og_instance
            .params
            .iter()
            .map(|param| param.size().get())
    }
    pub fn specialize(
        &self,
        params: &[NumberNonZeroUnsigned],
    ) -> Rc<PcodeMacroInstance> {
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
            return Rc::clone(inst);
        }

        //if not already exists, create a new instance with this signature
        let new_og = self.og_instance.specialize_me(params);
        self.instances.borrow_mut().push(Rc::clone(&new_og));

        //if a new instance was created, then, we are not finished yet, more
        //solving need to happen
        *self.solved.borrow_mut() = false;

        new_og
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
}

impl GlobalConvert for PcodeMacro {
    type FinalType = FinalPcodeMacro;

    fn convert(&self) -> Rc<Self::FinalType> {
        //TODO set a flat to converted
        if let Some(result) = self.result.borrow().as_ref() {
            return Rc::clone(result);
        }
        //let final_macro = Rc::new(UnsafeCell::new(MaybeUninit::zeroed()));
        let final_macro = Rc::new_cyclic(|weak_final_macro| {
            let instances: Vec<_> =
                std::mem::take(self.instances.borrow_mut().as_mut());
            let instances = instances
                .into_iter()
                .map(|instance| instance.convert_from_parent(&weak_final_macro))
                .collect();
            FinalPcodeMacro::new(instances)
        });
        *self.result.borrow_mut() = Some(Rc::clone(&final_macro));
        final_macro
    }
}

pub type FinalParameter = semantic::execution::Variable;
#[derive(Debug)]
pub struct Parameter {
    pub name: Rc<str>,
    src: Span,
    me: Weak<Self>,

    size: Cell<FieldSize>,
    result: RefCell<Option<Rc<FinalParameter>>>,
}
impl Parameter {
    pub fn new(name: &str, src: Span) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            src,
            me: Weak::clone(me),
            size: Cell::default(),
            result: RefCell::default(),
        })
    }
    pub fn clone_me(&self) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::clone(&self.name),
            src: self.src.clone(),
            me: Weak::clone(me),
            size: self.size.clone(),
            result: RefCell::default(),
        })
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    pub fn size(&self) -> &Cell<FieldSize> {
        &self.size
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
pub struct Builder<'a, 'b> {
    execution: Execution,
    current_block: Rc<Block>,

    sleigh: &'a Sleigh,
    params: &'b Vec<Rc<Parameter>>,
}

impl<'a, 'b> Builder<'a, 'b> {
    fn new(
        sleigh: &'a Sleigh,
        params: &'b Vec<Rc<Parameter>>,
        src: &Span,
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

impl<'a, 'b> From<Builder<'a, 'b>> for Execution {
    fn from(input: Builder<'a, 'b>) -> Self {
        input.execution
    }
}

impl<'a, 'b> ExecutionBuilder for Builder<'a, 'b> {
    fn sleigh(&self) -> &Sleigh {
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
        name: &str,
        src: &Span,
    ) -> Result<ExprValue, ExecutionError> {
        //check local variable
        self.variable(name)
            .map(|var| ExprValue::ExeVar(src.clone(), Rc::clone(var)))
            .or_else(|| {
                self.params
                    .iter()
                    .find(|param| param.name.as_ref() == name)
                    .map(|x| ExprValue::Param(src.clone(), Rc::clone(x)))
            })
            .map(Result::Ok)
            .unwrap_or_else(|| {
                //at last check the global scope
                use super::GlobalScope::*;
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(ExecutionError::MissingRef(src.clone()))?
                {
                    InstStart(x) => {
                        //TODO error
                        let len = self.sleigh().exec_addr_size().unwrap();
                        Ok(ExprValue::new_inst_start(src.clone(), *len, x))
                    }
                    InstNext(x) => {
                        //TODO error
                        let len = self.sleigh().exec_addr_size().unwrap();
                        Ok(ExprValue::new_inst_next(src.clone(), *len, x))
                    }
                    //TODO is this even legal? How the token_field is accesed?
                    TokenField(x) => {
                        Ok(ExprValue::new_token_field(src.clone(), x))
                    }
                    Varnode(x) => Ok(ExprValue::new_varnode(src.clone(), x)),
                    Context(x) => Ok(ExprValue::new_context(src.clone(), x)),
                    Bitrange(x) => Ok(ExprValue::new_bitrange(src.clone(), x)),
                    _ => Err(ExecutionError::InvalidRef(src.clone())),
                }
            })
    }

    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteValue, ExecutionError> {
        self.variable(name)
            .map(|var| WriteValue::ExeVar(src.clone(), Rc::clone(var)))
            .or_else(|| {
                self.params
                    .iter()
                    .find(|param| param.name.as_ref() == name)
                    .map(|x| WriteValue::Param(src.clone(), Rc::clone(x)))
            })
            .map(Result::Ok)
            .unwrap_or_else(|| {
                //at last check the global scope
                use super::GlobalScope::*;
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(ExecutionError::MissingRef(src.clone()))?
                {
                    Varnode(x) => Ok(WriteValue::Varnode(
                        GlobalReference::from_element(x, src.clone()),
                    )),
                    //Context(x) => Ok(WriteValue::Context(
                    //    GlobalReference::from_element(x, src.clone()),
                    //)),
                    Bitrange(x) => Ok(WriteValue::Bitrange(
                        GlobalReference::from_element(x, src.clone()),
                    )),
                    _ => Err(ExecutionError::InvalidRef(src.clone())),
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
        _input: block::execution::Build,
    ) -> Result<Build, ExecutionError> {
        Err(ExecutionError::MacroBuildInvalid)
    }

    fn current_block_mut(&mut self) -> &mut Rc<Block> {
        &mut self.current_block
    }
}

impl Sleigh {
    pub fn create_pcode_macro(
        &mut self,
        pcode: block::pcode_macro::PcodeMacro,
    ) -> Result<(), PcodeMacroError> {
        let params = pcode
            .params
            .iter()
            .map(|(name, src)| Parameter::new(&name, src.clone()))
            .collect();
        let mut execution = Builder::new(self, &params, &pcode.src);
        execution
            .extend(pcode.body)
            .to_pcode_macro(pcode.src.clone())?;
        let execution = RefCell::new(execution.into());

        let name = Rc::from(pcode.name);
        let pcode_macro = GlobalElement::new(
            name,
            PcodeMacro::new(pcode.src, params, execution),
        );
        //TODO: Error here
        self.insert_global(GlobalScope::PcodeMacro(pcode_macro))
            .unwrap();
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
        WriteValue::Varnode(_)
        | WriteValue::Bitrange(_)
        | WriteValue::Table(_)
        | WriteValue::ExeVar(_, _)
        | WriteValue::TokenField(_) => (),
        WriteValue::Param(_, param) => {
            if let Some(new_param) =
                old.iter().enumerate().find_map(|(i, x)| {
                    (Rc::as_ptr(x) == Rc::as_ptr(param)).then_some(i)
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
        ExprElement::DeReference(_, _, expr)
        | ExprElement::Truncate(_, _, expr)
        | ExprElement::Op(_, _, _, expr) => update_expr(expr, old, new),
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
        | ExprValue::TokenField(_, _)
        | ExprValue::InstStart(_, _)
        | ExprValue::InstNext(_, _)
        | ExprValue::Varnode(_)
        | ExprValue::Context(_, _)
        | ExprValue::Bitrange(_, _)
        | ExprValue::Table(_)
        | ExprValue::ExeVar(_, _) => (),
        ExprValue::Param(_, param) => {
            if let Some(new_param) =
                old.iter().enumerate().find_map(|(i, x)| {
                    (Rc::as_ptr(x) == Rc::as_ptr(param)).then_some(i)
                })
            {
                *param = Rc::clone(&new[new_param]);
            }
        }
    }
}
