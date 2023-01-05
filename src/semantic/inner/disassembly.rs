use indexmap::IndexMap;
use std::cell::RefCell;
use std::ops::ControlFlow;
use std::rc::{Rc, Weak};

use block::disassembly::{Op, OpUnary};

use crate::base::{IntTypeU, Value};
use crate::semantic::varnode::Varnode;
use crate::semantic::{
    self, DisassemblyError, GlobalReference, InstNext, InstStart,
};
use crate::syntax::block;
use crate::InputSource;

use super::token::TokenField;
use super::varnode::Context;
use super::{Pattern, Sleigh, Table};

pub trait ExprBuilder<'a> {
    fn read_scope(
        &mut self,
        name: &'a str,
    ) -> Result<ReadScope, DisassemblyError>;
    fn new_expr(
        &mut self,
        input: block::disassembly::Expr<'a>,
    ) -> Result<Expr, DisassemblyError> {
        let rpn = input
            .rpn
            .into_iter()
            .map(|x| self.new_expr_element(x))
            .collect::<Result<_, _>>()?;
        Ok(Expr { rpn })
    }
    fn new_expr_element(
        &mut self,
        input: block::disassembly::ExprElement<'a>,
    ) -> Result<ExprElement, DisassemblyError> {
        match input {
            block::disassembly::ExprElement::Value(Value::Number(_, int)) => {
                Ok(ExprElement::Value(ReadScope::Integer(int)))
            }
            block::disassembly::ExprElement::Value(Value::Ident(ident)) => {
                self.read_scope(ident).map(ExprElement::Value)
            }
            block::disassembly::ExprElement::Op(x) => Ok(ExprElement::Op(x)),
            block::disassembly::ExprElement::OpUnary(x) => {
                Ok(ExprElement::OpUnary(x))
            }
        }
    }
}
//block number, but the last bit is if it pre/pos, None means after the
//whole pattern is matched
#[derive(Debug)]
struct BlockCounter(Option<usize>);
impl BlockCounter {
    fn disassembly_at(&mut self, pos: bool, num: usize) {
        let num = num << 1 | usize::from(pos);
        match &mut self.0 {
            None => (),
            Some(current) => *current = num.max(*current),
        }
    }
    fn pre_disassembly_at(&mut self, num: usize) {
        self.disassembly_at(false, num)
    }
    fn pos_disassembly_at(&mut self, num: usize) {
        self.disassembly_at(true, num)
    }
    fn post_match(&mut self) {
        self.0 = None;
    }
}
impl Default for BlockCounter {
    fn default() -> Self {
        //first block on pre disassembly
        Self(Some(0))
    }
}

#[derive(Debug)]
pub struct Builder<'a, 'b> {
    sleigh: &'b Sleigh<'a>,
    pattern: &'a mut Pattern,
    block_counter: BlockCounter,
}

impl<'a, 'b> ExprBuilder<'a> for Builder<'a, 'b> {
    fn read_scope(
        &mut self,
        name: &'b str,
    ) -> Result<ReadScope, DisassemblyError> {
        use super::GlobalScope::*;
        let src = self.sleigh.input_src(name);
        self.pattern
            .disassembly_vars
            .get(name)
            .map(|local| Ok(ReadScope::Local(Rc::clone(local))))
            .unwrap_or_else(|| {
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(DisassemblyError::MissingRef(src.clone()))?
                {
                    InstNext(x) => {
                        //inst_next can only be known after the pattern is
                        //completly match
                        self.block_counter.post_match();
                        Ok(ReadScope::InstNext(
                            GlobalReference::from_element(x, src),
                        ))
                    }
                    InstStart(x) => Ok(ReadScope::InstStart(
                        GlobalReference::from_element(x, src),
                    )),
                    TokenField(x) => {
                        //check the pattern will produce this field
                        let Ok(Some(block_num)) = self.pattern.produce_token_field(
                            &GlobalReference::from_element(x, src.clone()),
                        ) else {
                            return Err(DisassemblyError::InvalidRef(src))
                        };
                        self.block_counter.pre_disassembly_at(block_num);
                        Ok(ReadScope::TokenField(
                            GlobalReference::from_element(x, src),
                        ))
                    }
                    Context(x) => Ok(ReadScope::Context(
                        GlobalReference::from_element(x, src),
                    )),
                    _ => Err(DisassemblyError::InvalidRef(src.clone())),
                }
            })
    }
}

impl<'a, 'b> Builder<'a, 'b> {
    pub fn new(sleigh: &'b Sleigh<'a>, pattern: &'a mut Pattern) -> Self {
        Self {
            sleigh,
            pattern,
            block_counter: BlockCounter::default(),
        }
    }
    fn insert_assertation(&mut self, ass: Assertation) {
        let ass_pos = match self.block_counter.0 {
            None => &mut self.pattern.pos,
            Some(block_counter) => {
                let block_num = block_counter >> 1;
                let block_pre = block_counter & 1 == 0;
                let block = &mut self.pattern.blocks[block_num];
                if block_pre {
                    &mut block.pre
                } else {
                    &mut block.pos
                }
            }
        };
        ass_pos.push(ass);
    }
    fn addr_scope(
        &mut self,
        name: &'a str,
    ) -> Result<AddrScope, DisassemblyError> {
        use super::GlobalScope::*;
        //get from local, otherwise get from global
        let src = self.sleigh.input_src(name);
        self.pattern
            .disassembly_vars
            .get(name)
            .map(|local| Ok(AddrScope::Local(Rc::clone(local))))
            .unwrap_or_else(|| {
                match self
                    .sleigh
                    .get_global(name)
                    .ok_or(DisassemblyError::MissingRef(src.clone()))?
                {
                    //TODO make sure the pattern will produce this table
                    Table(x) => {
                        let table_ref = GlobalReference::from_element(x, src);
                        //TODO error
                        let block_num =
                            self.pattern.is_table_produced(&table_ref).unwrap();
                        self.block_counter.pos_disassembly_at(block_num);
                        Ok(AddrScope::Table(table_ref))
                    }
                    InstStart(x) => Ok(AddrScope::InstStart(
                        GlobalReference::from_element(x, src),
                    )),
                    InstNext(x) => {
                        //inst_next can only be known after the pattern is
                        //completly match
                        self.block_counter.post_match();
                        Ok(AddrScope::InstNext(GlobalReference::from_element(
                            x, src,
                        )))
                    }
                    //TokenField(x) => Ok(AddrScope::TokenField(
                    //    GlobalReference::from_element(x, src),
                    //)),
                    Varnode(x) => Ok(AddrScope::Varnode(
                        GlobalReference::from_element(x, src),
                    )),
                    _ => Err(DisassemblyError::InvalidRef(src)),
                }
            })
    }
    //TODO Write Scope shold never fail, leave the Result just in case
    fn write_scope(
        &mut self,
        name: &'a str,
    ) -> Result<WriteScope, DisassemblyError> {
        //if variable exists, return it
        let var = self
            .pattern
            .disassembly_vars
            .get(name)
            .map(|var| WriteScope::Local(Rc::clone(var)))
            .or_else(|| {
                //check the global context, if context return it
                //NOTE if other thing with the same name, but is not context,
                //create the variable to shadow global context
                let src = self.sleigh.input_src(name);
                let context = self.sleigh.get_global(name)?.unwrap_context()?;
                Some(WriteScope::Context(GlobalReference::from_element(
                    context, src,
                )))
            })
            .unwrap_or_else(|| {
                //otherwise create the variable
                let src = self.sleigh.input_src(name);
                let var = Variable::new(name, src);
                self.pattern
                    .disassembly_vars
                    .insert(Rc::clone(var.name()), Rc::clone(&var));
                WriteScope::Local(var)
            });
        Ok(var)
    }
    fn context(
        &mut self,
        name: &'a str,
    ) -> Result<GlobalReference<Context>, DisassemblyError> {
        let src = self.sleigh.input_src(name);
        let context = self
            .sleigh
            .get_global(name)
            .ok_or(DisassemblyError::MissingRef(src.clone()))?
            .context_or(DisassemblyError::InvalidRef(src.clone()))?;
        Ok(GlobalReference::from_element(context, src))
    }

    fn new_globalset(
        &mut self,
        input: block::disassembly::GlobalSet<'a>,
    ) -> Result<GlobalSet, DisassemblyError> {
        let address = match input.address {
            Value::Number(_, int) => AddrScope::Interger(int),
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
    pub fn build(
        mut self,
        input: block::disassembly::Disassembly<'a>,
    ) -> Result<(), DisassemblyError> {
        input
            .assertations
            .into_iter()
            .map(|input| {
                self.new_assertation(input)
                    .map(|ass| self.insert_assertation(ass))
            })
            .collect::<Result<_, _>>()?;
        Ok(())
    }
}

pub type FinalReadScope = semantic::disassembly::ReadScope;
#[derive(Clone, Debug)]
pub enum ReadScope {
    //TODO: table??? Handle tables that the execution is just export Disassembly
    //Table(Reference<GlobalElement<Table>>),
    Integer(IntTypeU),
    Context(GlobalReference<Context>),
    TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Local(Rc<Variable>),
}

impl From<ReadScope> for FinalReadScope {
    fn from(input: ReadScope) -> Self {
        match input {
            ReadScope::Integer(x) => Self::Integer(x),
            ReadScope::Context(x) => Self::Context(x.convert_reference()),
            ReadScope::TokenField(x) => Self::TokenField(x.convert_reference()),
            ReadScope::InstStart(x) => Self::InstStart(x),
            ReadScope::InstNext(x) => Self::InstNext(x),
            ReadScope::Local(x) => Self::Local(x.convert()),
        }
    }
}

pub type FinalWriteScope = semantic::disassembly::WriteScope;
#[derive(Clone, Debug)]
pub enum WriteScope {
    Context(GlobalReference<Context>),
    Local(Rc<Variable>),
}
impl From<WriteScope> for FinalWriteScope {
    fn from(input: WriteScope) -> Self {
        match input {
            WriteScope::Context(x) => Self::Context(x.convert_reference()),
            WriteScope::Local(x) => Self::Local(x.convert()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AddrScope {
    Interger(IntTypeU),
    Table(GlobalReference<Table>),
    Varnode(GlobalReference<Varnode>),
    //TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Local(Rc<Variable>),
}
pub type FinalAddrScope = semantic::disassembly::AddrScope;
impl From<AddrScope> for FinalAddrScope {
    fn from(input: AddrScope) -> Self {
        match input {
            AddrScope::Interger(x) => Self::Integer(x),
            AddrScope::Varnode(x) => Self::Varnode(x.clone()),
            //AddrScope::TokenField(x) => Self::TokenField(x.convert_reference()),
            AddrScope::InstStart(x) => Self::InstStart(x),
            AddrScope::InstNext(x) => Self::InstNext(x),
            AddrScope::Table(x) => Self::Table(x.convert_reference()),
            AddrScope::Local(x) => Self::Local(x.convert()),
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
            *result = Some(Rc::new(semantic::disassembly::Variable::new(
                Rc::clone(&self.name),
            )));
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
    pub fn convert(self) -> FinalExpr {
        FinalExpr::new(self.rpn.into_iter().map(|x| x.convert()).collect())
    }
}

pub type FinalExprElement = semantic::disassembly::ExprElement;
#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ReadScope),
    Op(Op),
    OpUnary(OpUnary),
}
impl ExprElement {
    pub fn convert(self) -> FinalExprElement {
        match self {
            Self::Value(x) => FinalExprElement::Value(x.into()),
            Self::Op(x) => FinalExprElement::Op(x),
            Self::OpUnary(x) => FinalExprElement::OpUnary(x),
        }
    }
}

pub type FinalGlobalSet = semantic::disassembly::GlobalSet;
#[derive(Clone, Debug)]
pub struct GlobalSet {
    //pub src: InputSource,
    pub address: AddrScope,
    pub context: GlobalReference<Context>,
}
impl GlobalSet {
    pub fn convert(self) -> FinalGlobalSet {
        //let src = self.src;
        let address = self.address.into();
        let context = self.context.convert_reference();
        FinalGlobalSet::new(address, context)
    }
}

pub type FinalAssignment = semantic::disassembly::Assignment;
#[derive(Clone, Debug)]
pub struct Assignment {
    pub left: WriteScope,
    pub right: Expr,
}
impl Assignment {
    pub fn convert(self) -> FinalAssignment {
        let left = self.left.into();
        let right = self.right.convert();
        FinalAssignment::new(left, right)
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

#[derive(Clone, Debug, Default)]
pub struct Disassembly {
    pub vars: IndexMap<Rc<str>, Rc<Variable>>,
    pub assertations: Vec<Assertation>,
}

pub trait WalkerDisassembly<Break = ()> {
    fn disassembly(
        &mut self,
        disassembly: &Disassembly,
    ) -> ControlFlow<Break, ()> {
        disassembly
            .vars
            .values()
            .try_for_each(|var| self.declare_variable(var))?;
        disassembly
            .assertations
            .iter()
            .try_for_each(|ass| self.assertation(ass))
    }
    fn declare_variable(
        &mut self,
        _var: &Rc<Variable>,
    ) -> ControlFlow<Break, ()> {
        ControlFlow::Continue(())
    }
    fn assertation(&mut self, ass: &Assertation) -> ControlFlow<Break, ()> {
        match ass {
            Assertation::GlobalSet(gs) => self.global_set(gs),
            Assertation::Assignment(ass) => self.assignment(ass),
        }
    }
    fn global_set(&mut self, global_set: &GlobalSet) -> ControlFlow<Break, ()> {
        self.global_set_address(&global_set.address)?;
        self.global_set_context(&global_set.context)
    }
    fn assignment(
        &mut self,
        assignment: &Assignment,
    ) -> ControlFlow<Break, ()> {
        self.write(&assignment.left)?;
        self.expr(&assignment.right)
    }
    fn write(&mut self, _var: &WriteScope) -> ControlFlow<Break, ()> {
        ControlFlow::Continue(())
    }
    fn global_set_address(
        &mut self,
        _address: &AddrScope,
    ) -> ControlFlow<Break, ()> {
        ControlFlow::Continue(())
    }
    fn global_set_context(
        &mut self,
        _context: &GlobalReference<Context>,
    ) -> ControlFlow<Break, ()> {
        ControlFlow::Continue(())
    }
    fn expr(&mut self, expr: &Expr) -> ControlFlow<Break, ()> {
        expr.rpn.iter().try_for_each(|ele| self.element(ele))
    }
    fn element(&mut self, element: &ExprElement) -> ControlFlow<Break, ()> {
        match element {
            ExprElement::Value(value) => self.read(value),
            ExprElement::Op(_) | ExprElement::OpUnary(_) => {
                ControlFlow::Continue(())
            }
        }
    }
    fn read(&mut self, _value: &ReadScope) -> ControlFlow<Break, ()> {
        ControlFlow::Continue(())
    }
}
