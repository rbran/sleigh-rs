use std::collections::HashMap;
use std::ops::ControlFlow;

use crate::semantic::disassembly::{
    Assertation, Expr, ReadScope, Variable, VariableId,
};
use crate::semantic::pattern::{
    ConstraintValue, Pattern as FinalPattern, PatternLen, ProducedTable,
    ProducedTokenField,
};
use crate::semantic::{ContextId, TableId, TokenFieldId, TokenId};
use crate::{syntax, DisassemblyError, PatternError, Span};

use super::disassembly::ExprBuilder;
use super::{Sleigh, SolverStatus};

mod block;
pub use block::*;
mod verification;
pub use verification::*;
mod constraint;
pub use constraint::*;
mod pattern_len;
pub use pattern_len::*;

/// create directly from the syntax tree.
#[derive(Clone, Debug)]
pub struct Pattern {
    //NOTE point after the `is` in the constructor, pattern itself don't have a
    //start, thats because it could be mixed with the `with_block` pattern
    pub src: Span,
    /// all the blocks
    pub blocks: Vec<Block>,
    /// tables produced by all the blocks
    pub tables: HashMap<TableId, ProducedTable>,
    /// token fields produced, both implicit and explicit, local and in
    /// sub_patterns
    pub token_fields: HashMap<TokenFieldId, ProducedTokenField>,
    /// number of times each token is produced, used to find implicit fields
    pub tokens: HashMap<TokenId, usize>,
    //products: FieldProducts,
    /// disassasembly variables that will be created by the disassembler
    pub disassembly_variable_names: HashMap<String, VariableId>,
    pub disassembly_variables: Vec<Variable>,
    /// disassembly assertations that executed after all the blocks, because it
    /// requires the `inst_next` to be calculated
    pub pos: Vec<Assertation>,
    ////token fields required by value checking, not produced locally, maybe
    //they could be produced in outer patterns (in case this is sub_pattern)
    //pub unresolved_token_fields: Vec<GlobalReference<TokenField>>,
    pub len: Option<ConstructorPatternLen>,
}

struct FindValues(HashMap<TokenFieldId, Span>);

impl FindValues {
    fn search<'a>(
        verifications: impl Iterator<Item = &'a Verification>,
    ) -> HashMap<TokenFieldId, Span> {
        let mut find = Self(HashMap::new());
        for ver in verifications {
            find.verification(ver);
        }
        find.0
    }
}

impl PatternWalker for FindValues {
    //don't search recursivally, only on this pattern, each sub-pattern
    //will be responsible to produce it's own fields
    fn pattern(&mut self, _pattern: &Pattern) -> ControlFlow<(), ()> {
        ControlFlow::Continue(())
    }
    fn value(&mut self, value: &ConstraintValue) -> ControlFlow<(), ()> {
        use crate::semantic::disassembly::ExprElement::*;
        use crate::semantic::disassembly::ReadScope::*;
        for expr_ele in value.expr().elements().iter() {
            match expr_ele {
                Value {
                    value: TokenField(ass),
                    location,
                } => {
                    self.0.insert(*ass, location.clone());
                }
                Value {
                    value: Integer(_) | Context(_) | InstStart(_),
                    location: _,
                }
                | OpUnary(_)
                | Op(_) => (),
                Value {
                    value: InstNext(_) | Local(_),
                    location: _,
                } => unreachable!(),
            }
        }
        ControlFlow::Continue(())
    }
}

#[derive(Clone, Debug)]
pub struct DisassemblyBuilder<'a> {
    sleigh: &'a Sleigh,
}

impl<'a> DisassemblyBuilder<'a> {
    pub fn parse_expr(
        sleigh: &'a Sleigh,
        expr: syntax::block::disassembly::Expr,
    ) -> Result<Expr, Box<DisassemblyError>> {
        let mut builder = Self { sleigh };
        builder.new_expr(expr)
    }
}

impl<'a> ExprBuilder for DisassemblyBuilder<'a> {
    fn read_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ReadScope, Box<DisassemblyError>> {
        use super::GlobalScope::*;
        match self.sleigh.get_global(name).ok_or_else(|| {
            Box::new(DisassemblyError::MissingRef(src.clone()))
        })? {
            TokenField(x) => Ok(ReadScope::TokenField(x)),
            Context(x) => Ok(ReadScope::Context(x)),
            _ => Err(Box::new(DisassemblyError::InvalidRef(src.clone()))),
        }
    }
}

pub trait PatternWalker<B = ()> {
    fn pattern(&mut self, pattern: &Pattern) -> ControlFlow<B, ()> {
        pattern
            .blocks
            .iter()
            .try_for_each(|block| self.block(block))
    }
    fn block(&mut self, block: &Block) -> ControlFlow<B, ()> {
        block
            .base
            .tokens
            .keys()
            .try_for_each(|token| self.token(*token))?;
        block
            .base
            .token_fields
            .values()
            .try_for_each(|field| self.token_field(field.field))?;
        block
            .base
            .verifications
            .iter()
            .try_for_each(|x| self.verification(x))
    }
    fn token(&mut self, _token: TokenId) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn token_field(&mut self, _field: TokenFieldId) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn table(&mut self, _table: TableId) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn context(&mut self, _table: ContextId) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn value(&mut self, _value: &ConstraintValue) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn verification(
        &mut self,
        verification: &Verification,
    ) -> ControlFlow<B, ()> {
        match verification {
            Verification::ContextCheck {
                context,
                op: _,
                value,
            } => {
                self.context(*context)?;
                self.value(value)
            }
            Verification::TableBuild {
                produced_table,
                verification,
            } => {
                if let Some((_op, value)) = verification {
                    self.value(value)?;
                }
                self.table(produced_table.table)
            }
            Verification::SubPattern {
                location: _,
                pattern,
            } => self.pattern(pattern),
            Verification::TokenFieldCheck {
                field,
                op: _,
                value,
            } => {
                self.value(value)?;
                self.token_field(*field)
            }
        }
    }
}

impl Pattern {
    pub fn new(
        sleigh: &Sleigh,
        input: syntax::block::pattern::Pattern,
        this_table: TableId,
    ) -> Result<Self, Box<PatternError>> {
        let blocks = input
            .blocks
            .into_iter()
            .map(|block| Block::new(sleigh, block, this_table))
            .collect::<Result<Vec<Block>, _>>()?;

        //make sure the fields are not duplicated
        let mut token_fields: HashMap<TokenFieldId, ProducedTokenField> =
            HashMap::new();
        use std::collections::hash_map::Entry::*;
        for (k, produced_field) in blocks
            .iter()
            .flat_map(|block| block.base.token_fields.iter())
        {
            match token_fields.entry(*k) {
                Occupied(entry) => {
                    // implicit fields where not found yet, so unwrap here
                    // dont panic
                    return Err(Box::new(PatternError::MultipleProduction(
                        entry.get().source.as_ref().unwrap().clone(),
                        produced_field.source.as_ref().unwrap().clone(),
                    )));
                }
                Vacant(entry) => {
                    entry.insert(produced_field.clone());
                }
            }
        }
        //make sure the tables are not duplicated
        let mut tables: HashMap<TableId, ProducedTable> = HashMap::new();
        for (k, produced_table) in
            blocks.iter().flat_map(|block| block.base.tables.iter())
        {
            match tables.entry(*k) {
                Occupied(entry) => {
                    return Err(Box::new(PatternError::MultipleProduction(
                        entry.get().location.clone(),
                        produced_table.location.clone(),
                    )));
                }
                Vacant(entry) => {
                    entry.insert(produced_table.clone());
                }
            }
        }
        let mut tokens = HashMap::new();
        let tokens_iter =
            blocks.iter().flat_map(|block| block.base.tokens.iter());
        for (token, num) in tokens_iter {
            tokens
                .entry(*token)
                .and_modify(|entry_num| *entry_num += *num)
                .or_insert_with(|| *num);
        }
        //let products = FieldProducts::combine_blocks(blocks.iter())?;
        Ok(Self {
            blocks,
            len: None,
            //products,
            tokens,
            token_fields,
            tables,
            disassembly_variable_names: HashMap::new(),
            disassembly_variables: vec![],
            pos: vec![],
            src: input.src,
        })
    }

    pub fn variable(&self, id: VariableId) -> &Variable {
        &self.disassembly_variables[id.0]
    }

    pub fn variable_mut(&mut self, id: VariableId) -> &mut Variable {
        &mut self.disassembly_variables[id.0]
    }
    //token fields required by value comparisons that this pattern can't produce
    //itself
    pub fn unresolved_token_fields(
        &self,
        sleigh: &Sleigh,
    ) -> impl Iterator<Item = (TokenFieldId, Span)> {
        let mut all_unresolved = HashMap::new();
        for (index, block) in self.blocks.iter().enumerate() {
            let mut block_unresolved =
                block.base.unresolved_token_fields(sleigh);
            //remove unresolveds already produced by previous blocks
            block_unresolved.retain(|unresolved, _location| {
                self.blocks[..index].iter().any(|block| {
                    block.base.token_fields.contains_key(unresolved)
                })
            });
            all_unresolved.extend(block_unresolved);
        }
        all_unresolved.into_iter()
    }
    pub fn is_table_produced(&self, table: TableId) -> Option<usize> {
        self.blocks
            .iter()
            .position(|block| block.base.is_table_produced(table))
    }
    pub fn produce_token_field(
        &mut self,
        sleigh: &Sleigh,
        token_field_id: TokenFieldId,
    ) -> Result<Option<usize>, Box<PatternError>> {
        //check if we already produces it, if so do nothing
        if self.token_fields.contains_key(&token_field_id) {
            let block_num = self.blocks.iter().position(|block| {
                block.base.is_token_field_produced(token_field_id)
            });
            return Ok(block_num);
        }
        //try all the blocks, find one that is able to produce it, but only one!
        let mut found = None;
        for (block_num, block) in self.blocks.iter_mut().enumerate() {
            match (
                found,
                block.base.produce_token_field(sleigh, token_field_id),
            ) {
                //this block can't add this token_field
                (_, false) => (),
                //found the first block that is able to add the token_field
                (None, true) => found = Some(block_num),
                //found the second block that is able to add the token_field
                (Some(_), true) => {
                    return Err(Box::new(PatternError::AmbiguousProduction(
                        sleigh.token_field(token_field_id).location.clone(),
                    )))
                }
            }
        }
        self.token_fields.insert(
            token_field_id,
            ProducedTokenField {
                local: true,
                source: None,
                field: token_field_id,
            },
        );
        Ok(found)
    }
    pub fn src(&self) -> &Span {
        &self.src
    }
    pub fn root_len(&self, sleigh: &Sleigh) -> usize {
        self.blocks
            .iter()
            .map(|block| block.base.root_len(sleigh))
            .sum()
    }
    pub fn calculate_len(
        &mut self,
        sleigh: &Sleigh,
        solved: &mut impl SolverStatus,
    ) -> Result<bool, Box<PatternError>> {
        //if fully solved, do nothing
        if let Some(ConstructorPatternLen::Basic(_)) = self.len {
            return Ok(true);
        }
        let finished = self
            .blocks
            .iter_mut()
            .map(|block| block.solve(sleigh, solved))
            .try_fold(
                true,
                |acc, finished| -> Result<_, Box<PatternError>> {
                    Ok(acc & finished?)
                },
            )?;
        //FUTURE replace with try_reduce
        let mut lens = self.blocks.iter().map(|block| block.len());
        let first = ConstructorPatternLen::Basic(PatternLen::Defined(0));
        let final_len = lens.try_fold(first, |acc, len| acc.add(len?));

        let finished_len =
            matches!(final_len, Some(ConstructorPatternLen::Basic(_)));
        //is not possible to have a final len, but not finished
        assert_eq!(finished_len, finished);

        if !finished {
            //if not fully finished yet, request another run
            solved.iam_not_finished_location(self.src(), file!(), line!());
        }
        //update self.len if found a more restricted value
        match final_len {
            Some(final_len) if self.len != Some(final_len) => {
                solved.i_did_a_thing();
                self.len = Some(final_len);
            }
            _ => (),
        }
        Ok(finished)
    }
    pub fn calculate_bits(&mut self, variants_before: usize) {
        //variants_before 0 means this is a root pattern
        let mut variants_before_counter = variants_before.max(1);
        //we are ready to convert all blocks into phase2
        for block in self.blocks.iter_mut() {
            //create the block and update the variants_counter
            let block = block.build_phase2(variants_before_counter);
            variants_before_counter *= block.variants_number;

            //reset the before counter if a disjointed pattern is found
            if block.len.single_len().is_none() {
                variants_before_counter = 1;
            }
        }
    }
    pub fn variants_num(&self) -> usize {
        self.blocks
            .iter()
            .map(|block| block.phase2().unwrap())
            .fold(1, |acc, block| acc * block.variants_number)
    }
    pub fn convert(self) -> FinalPattern {
        let blocks = self.blocks.into_iter().map(|b| b.convert()).collect();
        FinalPattern {
            disassembly_vars: self.disassembly_variables.into(),
            blocks,
            pos: self.pos.into(),
            len: self.len.unwrap().basic().unwrap(),
        }
    }
}
