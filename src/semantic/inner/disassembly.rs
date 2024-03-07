use std::cell::Cell;
use std::collections::HashMap;

use crate::disassembly::VariableType;
use crate::semantic::disassembly::{
    AddrScope, Assertation, Expr, ExprElement, GlobalSet, ReadScope, WriteScope,
};
use crate::semantic::disassembly::{Assignment, Variable, VariableId};
use crate::semantic::{ContextId, Span};
use crate::{syntax, DisassemblyError};

use super::pattern::Pattern;
use super::Sleigh;

pub trait ExprBuilder {
    fn read_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ReadScope, Box<DisassemblyError>>;
    fn new_expr(
        &mut self,
        input: syntax::block::disassembly::Expr,
    ) -> Result<Expr, Box<DisassemblyError>> {
        let rpn = input
            .rpn
            .into_iter()
            .map(|x| self.new_expr_element(x))
            .collect::<Result<_, _>>()?;
        Ok(Expr { rpn })
    }
    fn new_expr_element(
        &mut self,
        input: syntax::block::disassembly::ExprElement,
    ) -> Result<ExprElement, Box<DisassemblyError>> {
        match input {
            syntax::block::disassembly::ExprElement::Value(
                syntax::Value::Number(src, int),
            ) => Ok(ExprElement::Value {
                value: ReadScope::Integer(int),
                location: src,
            }),

            syntax::block::disassembly::ExprElement::Value(
                syntax::Value::Ident(src, ident),
            ) => {
                self.read_scope(&ident, &src)
                    .map(|value| ExprElement::Value {
                        value,
                        location: src,
                    })
            }

            syntax::block::disassembly::ExprElement::Op(x) => {
                Ok(ExprElement::Op(x))
            }

            syntax::block::disassembly::ExprElement::OpUnary(x) => {
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
    sleigh: &'b Sleigh,
    pattern: &'a mut Pattern,
    block_counter: BlockCounter,
}

impl<'a, 'b> ExprBuilder for Builder<'a, 'b> {
    fn read_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ReadScope, Box<DisassemblyError>> {
        use super::GlobalScope;
        self.pattern
            .disassembly_variable_names
            .get(name)
            .map(|local| Ok(ReadScope::Local(*local)))
            .unwrap_or_else(|| {
                match self.sleigh.get_global(name).ok_or_else(|| {
                    Box::new(DisassemblyError::MissingRef(src.clone()))
                })? {
                    GlobalScope::InstNext(x) => {
                        //inst_next can only be known after the pattern is
                        //completly match
                        self.block_counter.post_match();
                        Ok(ReadScope::InstNext(x))
                    }
                    GlobalScope::InstStart(x) => Ok(ReadScope::InstStart(x)),
                    GlobalScope::TokenField(x) => {
                        //check the pattern will produce this field
                        let Ok(Some(block_num)) =
                            self.pattern.produce_token_field(self.sleigh, x)
                        else {
                            return Err(Box::new(
                                DisassemblyError::InvalidRef(src.clone()),
                            ));
                        };
                        self.block_counter.pre_disassembly_at(block_num);
                        Ok(ReadScope::TokenField(x))
                    }
                    GlobalScope::Context(x) => Ok(ReadScope::Context(x)),
                    _ => {
                        Err(Box::new(DisassemblyError::InvalidRef(src.clone())))
                    }
                }
            })
    }
}

impl<'a, 'b> Builder<'a, 'b> {
    pub fn new(sleigh: &'b Sleigh, pattern: &'a mut Pattern) -> Self {
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
                    &mut block.base.pre
                } else {
                    &mut block.base.pos
                }
            }
        };
        ass_pos.push(ass);
    }
    fn addr_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<AddrScope, Box<DisassemblyError>> {
        use super::GlobalScope::*;
        //get from local, otherwise get from global
        self.pattern
            .disassembly_variable_names
            .get(name)
            .map(|local| Ok(AddrScope::Local(*local)))
            .unwrap_or_else(|| {
                match self.sleigh.get_global(name).ok_or_else(|| {
                    Box::new(DisassemblyError::MissingRef(src.clone()))
                })? {
                    //TODO make sure the pattern will produce this table
                    Table(x) => {
                        //TODO error
                        let block_num =
                            self.pattern.is_table_produced(x).unwrap();
                        self.block_counter.pos_disassembly_at(block_num);
                        Ok(AddrScope::Table(x))
                    }
                    InstStart(x) => Ok(AddrScope::InstStart(x)),
                    InstNext(x) => {
                        //inst_next can only be known after the pattern is
                        //completly match
                        self.block_counter.post_match();
                        Ok(AddrScope::InstNext(x))
                    }
                    _ => {
                        Err(Box::new(DisassemblyError::InvalidRef(src.clone())))
                    }
                }
            })
    }
    //TODO Write Scope shold never fail, leave the Result just in case
    fn write_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<WriteScope, Box<DisassemblyError>> {
        //if variable exists, return it
        if let Some(var) = self.pattern.disassembly_variable_names.get(name) {
            return Ok(WriteScope::Local(*var));
        }
        //check the global context, if context return it
        //NOTE if other thing with the same name, but is not context,
        //create the variable to shadow global context
        if let Some(context) = self
            .sleigh
            .get_global(name)
            .and_then(|global| global.context())
        {
            return Ok(WriteScope::Context(context));
        }
        //otherwise create the variable
        let var = Variable {
            location: src.clone(),
            value_type: Cell::new(VariableType::Value(None)),
            name: name.to_owned().into(),
        };
        self.pattern.disassembly_variables.push(var);
        let var_id = VariableId(self.pattern.disassembly_variables.len() - 1);
        self.pattern
            .disassembly_variable_names
            .insert(name.to_owned(), var_id);
        Ok(WriteScope::Local(var_id))
    }
    fn context(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ContextId, Box<DisassemblyError>> {
        let context = self
            .sleigh
            .get_global(name)
            .ok_or_else(|| Box::new(DisassemblyError::MissingRef(src.clone())))?
            .context()
            .ok_or_else(|| {
                Box::new(DisassemblyError::InvalidRef(src.clone()))
            })?;
        Ok(context)
    }

    fn new_globalset(
        &mut self,
        input: syntax::block::disassembly::GlobalSet,
    ) -> Result<GlobalSet, Box<DisassemblyError>> {
        let (_src, address) = match input.address {
            syntax::Value::Number(src, int) => {
                let addr = AddrScope::Integer(int.unsigned().unwrap());
                (src, addr)
            }
            syntax::Value::Ident(src, ident) => {
                let addr = self.addr_scope(&ident, &src)?;
                (src, addr)
            }
        };
        Ok(GlobalSet {
            address,
            context: self.context(&input.context, &input.src)?,
            location: input.src,
        })
    }
    fn new_assignment(
        &mut self,
        input: syntax::block::disassembly::Assignment,
    ) -> Result<Assignment, Box<DisassemblyError>> {
        let left = self.write_scope(&input.left, &input.left_span)?;
        let right = self.new_expr(input.right)?;
        Ok(Assignment { left, right })
    }
    fn new_assertation(
        &mut self,
        input: syntax::block::disassembly::Assertation,
    ) -> Result<Assertation, Box<DisassemblyError>> {
        match input {
            syntax::block::disassembly::Assertation::GlobalSet(globalset) => {
                self.new_globalset(globalset).map(Assertation::GlobalSet)
            }
            syntax::block::disassembly::Assertation::Assignment(assignment) => {
                self.new_assignment(assignment).map(Assertation::Assignment)
            }
        }
    }
    pub fn build(
        mut self,
        input: syntax::block::disassembly::Disassembly,
    ) -> Result<(), Box<DisassemblyError>> {
        input.assertations.into_iter().try_for_each(|input| {
            self.new_assertation(input)
                .map(|ass| self.insert_assertation(ass))
        })
    }
}

#[derive(Clone, Debug, Default)]
pub struct Disassembly {
    pub variable_names: HashMap<String, VariableId>,
    pub variables: Vec<Variable>,
    pub assertations: Vec<Assertation>,
}
