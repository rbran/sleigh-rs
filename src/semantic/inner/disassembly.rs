use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use block::disassembly::{Op, OpUnary};

use crate::base::{IntTypeU, Value};
use crate::semantic::varnode::Varnode;
use crate::semantic::{self, DisassemblyError};
use crate::syntax::block;
use crate::InputSource;

use super::assembly::Assembly;
use super::Table;

pub trait ExprBuilder<'a> {
    fn read_scope(
        &mut self,
        name: &'a str,
    ) -> Result<Rc<dyn ReadScope>, DisassemblyError>;
    fn new_expr(
        &mut self,
        mut input: block::disassembly::Expr<'a>,
    ) -> Result<Expr, DisassemblyError> {
        let rpn = input
            .rpn
            .drain(..)
            .map(|x| self.new_expr_element(x))
            .collect::<Result<_, _>>()?;
        Ok(Expr { rpn })
    }
    fn new_expr_element(
        &mut self,
        input: block::disassembly::ExprElement<'a>,
    ) -> Result<ExprElement, DisassemblyError> {
        let ele = match input {
            block::disassembly::ExprElement::Value(Value::Number(_, int)) => {
                ExprElement::Value(int.as_read())
            }
            block::disassembly::ExprElement::Value(Value::Ident(ident)) => {
                self.read_scope(ident).map(ExprElement::Value)?
            }
            block::disassembly::ExprElement::Op(x) => ExprElement::Op(x),
            block::disassembly::ExprElement::OpUnary(x) => {
                ExprElement::OpUnary(x)
            }
        };
        Ok(ele)
    }
}

pub trait DisassemblyBuilder<'a>: ExprBuilder<'a> {
    fn insert_assertation(&mut self, ass: Assertation);
    fn addr_scope(
        &mut self,
        name: &'a str,
    ) -> Result<AddrScope, DisassemblyError>;
    //TODO Write Scope shold never fail, leave the Result just in case
    fn write_scope(
        &mut self,
        name: &'a str,
    ) -> Result<Rc<dyn WriteScope>, DisassemblyError>;
    fn context(&mut self, name: &'a str) -> Result<Rc<Varnode>, DisassemblyError>;

    fn new_globalset(
        &mut self,
        input: block::disassembly::GlobalSet<'a>,
    ) -> Result<GlobalSet, DisassemblyError> {
        let address = match input.address {
            Value::Number(_, int) => AddrScope::Int(int),
            Value::Ident(ident) => self.addr_scope(ident)?,
        };
        let context = self.context(input.context)?;
        Ok(GlobalSet { address, context })
    }
    fn new_assignment(
        &mut self,
        input: block::disassembly::Assignment<'a>,
    ) -> Result<Assignment, DisassemblyError> {
        let left = self.write_scope(input.left)?;
        let right = self.new_expr(input.right)?;
        Ok(Assignment { left, right })
    }
    fn new_assertation(
        &mut self,
        input: block::disassembly::Assertation<'a>,
    ) -> Result<Assertation, DisassemblyError> {
        match input {
            block::disassembly::Assertation::GlobalSet(globalset) => {
                self.new_globalset(globalset).map(Assertation::GlobalSet)
            }
            block::disassembly::Assertation::Assignment(assignment) => {
                self.new_assignment(assignment).map(Assertation::Assignment)
            }
        }
    }
    fn extend(
        &mut self,
        mut input: block::disassembly::Disassembly<'a>,
    ) -> Result<(), DisassemblyError> {
        input
            .assertations
            .drain(..)
            .map(|input| {
                self.new_assertation(input)
                    .map(|ass| self.insert_assertation(ass))
            })
            .collect::<Result<_, _>>()
    }
}

pub type FinalReadScope = semantic::disassembly::ReadScope;
pub trait ReadScope: std::fmt::Debug {
    fn as_read(&self) -> Rc<dyn ReadScope>;
    fn convert(&self) -> FinalReadScope;
    fn value(&self) -> Option<IntTypeU> {
        None
    }
}
impl ReadScope for IntTypeU {
    fn as_read(&self) -> Rc<dyn ReadScope> {
        Rc::new(*self)
    }
    fn convert(&self) -> FinalReadScope {
        FinalReadScope::Integer(*self)
    }
    fn value(&self) -> Option<IntTypeU> {
        Some(*self)
    }
}
impl ReadScope for Varnode {
    fn as_read(&self) -> Rc<dyn ReadScope> {
        self.me()
    }
    fn convert(&self) -> FinalReadScope {
        FinalReadScope::Varnode(self.me())
    }
}
impl ReadScope for Assembly {
    fn as_read(&self) -> Rc<dyn ReadScope> {
        self.me()
    }
    fn convert(&self) -> FinalReadScope {
        FinalReadScope::Assembly(self.me())
    }
}
impl ReadScope for Variable {
    fn as_read(&self) -> Rc<dyn ReadScope> {
        self.me()
    }
    fn convert(&self) -> FinalReadScope {
        FinalReadScope::Local(self.convert())
    }
}

pub type FinalWriteScope = semantic::disassembly::WriteScope;
pub trait WriteScope: std::fmt::Debug {
    fn as_write(&self) -> Rc<dyn WriteScope>;
    fn convert(&self) -> FinalWriteScope;
}
impl WriteScope for Varnode {
    fn as_write(&self) -> Rc<dyn WriteScope> {
        self.me()
    }
    fn convert(&self) -> FinalWriteScope {
        FinalWriteScope::Varnode(self.me())
    }
}
impl WriteScope for Variable {
    fn as_write(&self) -> Rc<dyn WriteScope> {
        self.me()
    }
    fn convert(&self) -> FinalWriteScope {
        FinalWriteScope::Local(self.convert())
    }
}

#[derive(Clone, Debug)]
pub enum AddrScope {
    Int(IntTypeU),
    Table(Rc<Table>),
    Varnode(Rc<Varnode>),
    Assembly(Rc<Assembly>),
    Local(Rc<Variable>),
}
pub type FinalAddrScope = semantic::disassembly::AddrScope;
impl AddrScope {
    fn convert(&self) -> FinalAddrScope {
        match self {
            AddrScope::Int(x) => FinalAddrScope::Int(*x),
            AddrScope::Table(x) => FinalAddrScope::Table(x.reference()),
            AddrScope::Varnode(x) => FinalAddrScope::Varnode(Rc::clone(x)),
            AddrScope::Assembly(x) => FinalAddrScope::Assembly(Rc::clone(x)),
            AddrScope::Local(x) => FinalAddrScope::Local(x.convert()),
        }
    }
}

pub type FinalVariable = Rc<semantic::disassembly::Variable>;
#[derive(Clone, Debug)]
pub struct Variable {
    name: Rc<str>,
    src: InputSource,
    result: RefCell<Option<FinalVariable>>,
    me: Weak<Self>,
}
impl Variable {
    pub fn new(name: &str, src: InputSource) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            src,
            result: RefCell::default(),
            me: Weak::clone(me),
        })
    }
    pub fn src(&self) -> &InputSource {
        &self.src
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    pub fn name(&self) -> &Rc<str> {
        &self.name
    }
    pub fn reference(&self) -> FinalVariable {
        Rc::clone(self.result.borrow().as_ref().unwrap())
    }
    pub fn convert(&self) -> FinalVariable {
        let create = self.result.borrow().is_none();
        if create {
            let mut result = self.result.borrow_mut();
            *result = Some(Rc::new(semantic::disassembly::Variable {
                name: Rc::clone(&self.name),
            }));
        }
        self.reference()
    }
}

pub type FinalExpr = semantic::disassembly::Expr;
#[derive(Clone, Debug)]
pub struct Expr {
    pub rpn: Vec<ExprElement>,
}

impl Expr {
    pub fn convert(mut self) -> FinalExpr {
        FinalExpr {
            rpn: self.rpn.drain(..).map(|x| x.convert()).collect(),
        }
    }
}

pub type FinalExprElement = semantic::disassembly::ExprElement;
#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(Rc<dyn ReadScope>),
    Op(Op),
    OpUnary(OpUnary),
}
impl ExprElement {
    pub fn convert(self) -> FinalExprElement {
        match self {
            ExprElement::Value(x) => FinalExprElement::Value(x.convert()),
            ExprElement::Op(x) => FinalExprElement::Op(x),
            ExprElement::OpUnary(x) => FinalExprElement::OpUnary(x),
        }
    }
}

pub type FinalGlobalSet = semantic::disassembly::GlobalSet;
#[derive(Clone, Debug)]
pub struct GlobalSet {
    pub address: AddrScope,
    pub context: Rc<Varnode>, //context only
}
impl GlobalSet {
    pub fn convert(self) -> FinalGlobalSet {
        let address = self.address.convert();
        let context = Rc::clone(&self.context);
        FinalGlobalSet { address, context }
    }
}

pub type FinalAssignment = semantic::disassembly::Assignment;
#[derive(Clone, Debug)]
pub struct Assignment {
    pub left: Rc<dyn WriteScope>,
    pub right: Expr,
}
impl Assignment {
    pub fn convert(self) -> FinalAssignment {
        let left = self.left.convert();
        let right = self.right.convert();
        FinalAssignment { left, right }
    }
}

pub type FinalAssertation = semantic::disassembly::Assertation;
#[derive(Clone, Debug)]
pub enum Assertation {
    GlobalSet(GlobalSet),
    Assignment(Assignment),
}

impl Assertation {
    pub fn convert(self) -> FinalAssertation {
        match self {
            Assertation::GlobalSet(x) => {
                FinalAssertation::GlobalSet(x.convert())
            }
            Assertation::Assignment(x) => {
                FinalAssertation::Assignment(x.convert())
            }
        }
    }
}

pub type FinalDisassembly = semantic::disassembly::Disassembly;
#[derive(Clone, Debug, Default)]
pub struct Disassembly {
    pub vars: HashMap<Rc<str>, Rc<Variable>>,
    pub assertations: Vec<Assertation>,
}

impl Disassembly {
    pub fn variable(&self, name: &str) -> Option<&Rc<Variable>> {
        self.vars.get(name)
    }
    pub fn convert(mut self) -> FinalDisassembly {
        let vars = self
            .vars
            .iter()
            .map(|(name, var)| (Rc::clone(name), var.convert()))
            .collect();
        let assertations: Vec<_> =
            self.assertations.drain(..).map(|x| x.convert()).collect();
        //TODO disassembly need to be separated between before and after pattern
        //match but for now it is just a flag that make everything before or
        //after if there is an inst_next on the execution.
        let pos_match = assertations.iter().any(|asser| match asser {
            semantic::disassembly::Assertation::Assignment(
                semantic::disassembly::Assignment { left: _, right },
            ) => right.rpn.iter().any(|ele| match ele {
                semantic::disassembly::ExprElement::Value(
                    semantic::disassembly::ReadScope::Assembly(ass),
                ) => matches!(
                    &ass.assembly_type,
                    semantic::assembly::AssemblyType::Next(_)
                ),
                _ => false,
            }),
            semantic::disassembly::Assertation::GlobalSet(
                semantic::disassembly::GlobalSet {
                    address,
                    context: _,
                },
            ) => match address {
                semantic::disassembly::AddrScope::Assembly(ass) => matches!(
                    &ass.assembly_type,
                    semantic::assembly::AssemblyType::Next(_)
                ),
                _ => false,
            },
        });
        FinalDisassembly {
            pos: pos_match,
            vars,
            assertations,
        }
    }
}
