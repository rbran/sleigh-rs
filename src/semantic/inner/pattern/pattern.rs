use std::rc::Rc;

use indexmap::IndexMap;

use crate::semantic::inner::disassembly::{Assertation, Variable};
use crate::semantic::inner::table::Table;
use crate::semantic::inner::token::TokenField;
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::semantic::pattern::{PatternError, PatternLen};
use crate::semantic::token::Token;
use crate::semantic::{GlobalAnonReference, GlobalReference};
use crate::syntax::block;
use crate::Span;

use super::constraint::{BitConstraint, BlockConstraint, PatternConstraint};
use super::{
    is_len_finished, Block, ConstructorPatternLen, ProducedTable,
    ProducedTokenField,
};

pub type FinalPattern = crate::semantic::pattern::Pattern;
#[derive(Clone, Debug)]
pub struct Pattern {
    //NOTE point after the `is` in the constructor, pattern itself don't have a
    //start, thats because it could be mixed with the `with_block` pattern
    pub src: Span,
    pub len: Option<ConstructorPatternLen>,
    pub tables: IndexMap<*const Table, ProducedTable>,
    pub tokens: IndexMap<*const Token, (usize, GlobalAnonReference<Token>)>,
    pub token_fields: IndexMap<*const TokenField, ProducedTokenField>,
    //products: FieldProducts,
    pub disassembly_vars: IndexMap<Rc<str>, Rc<Variable>>,
    pub blocks: Vec<Block>,
    pub pos: Vec<Assertation>,
    ////token fields required by value checking, not produced locally, maybe
    //they could be produced in outer patterns (in case this is sub_pattern)
    //pub unresolved_token_fields: Vec<GlobalReference<TokenField>>,
}

impl Pattern {
    pub fn new(
        sleigh: &Sleigh,
        input: block::pattern::Pattern,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        let blocks = input
            .blocks
            .into_iter()
            .map(|block| Block::new(sleigh, block, this_table))
            .collect::<Result<Vec<Block>, _>>()?;
        let len = blocks.is_empty().then(|| PatternLen::Defined(0).into());

        //make sure the fields are not duplicated
        let mut token_fields: IndexMap<*const TokenField, ProducedTokenField> =
            IndexMap::new();
        use indexmap::map::Entry::*;
        for (k, produced_field) in
            blocks.iter().map(|block| &block.token_fields).flatten()
        {
            match token_fields.entry(*k) {
                Occupied(entry) => {
                    return Err(PatternError::MultipleProduction(
                        entry.get().field.location().clone(),
                        produced_field.field.location().clone(),
                    ));
                }
                Vacant(entry) => {
                    entry.insert(produced_field.clone());
                }
            }
        }
        //make sure the tables are not duplicated
        let mut tables: IndexMap<*const Table, ProducedTable> = IndexMap::new();
        for (k, produced_table) in
            blocks.iter().map(|block| &block.tables).flatten()
        {
            match tables.entry(*k) {
                Occupied(entry) => {
                    return Err(PatternError::MultipleProduction(
                        entry.get().table.location().clone(),
                        produced_table.table.location().clone(),
                    ));
                }
                Vacant(entry) => {
                    entry.insert(produced_table.clone());
                }
            }
        }
        let mut tokens = IndexMap::new();
        let tokens_iter =
            blocks.iter().map(|block| block.tokens.values()).flatten();
        for (num, token) in tokens_iter {
            tokens
                .entry(token.element_ptr())
                .and_modify(|(entry_num, _token)| *entry_num += *num)
                .or_insert_with(|| (*num, token.clone()));
        }
        //let products = FieldProducts::combine_blocks(blocks.iter())?;
        Ok(Self {
            blocks,
            //products,
            len,
            tokens,
            token_fields,
            tables,
            disassembly_vars: IndexMap::new(),
            pos: vec![],
            src: input.src,
        })
    }
    //token fields required by value comparisons that this pattern can't produce
    //itself
    pub fn unresolved_token_fields(
        &self,
    ) -> IndexMap<*const TokenField, GlobalReference<TokenField>> {
        let mut all_unresolved = IndexMap::new();
        for (index, block) in self.blocks.iter().enumerate() {
            let mut block_unresolved = block.unresolved_token_fields();
            //remove unresolveds already produced by previous blocks
            block_unresolved.retain(|_key, unresolved| {
                self.blocks[..index].iter().any(|block| {
                    block.token_fields.contains_key(&unresolved.element_ptr())
                })
            });
            all_unresolved.extend(block_unresolved);
        }
        all_unresolved
    }
    pub fn is_table_produced(
        &self,
        table: &GlobalReference<Table>,
    ) -> Option<usize> {
        self.blocks
            .iter()
            .enumerate()
            .find(|(_, block)| block.is_table_produced(table))
            .map(|(i, _)| i)
    }
    pub fn produce_token_field(
        &mut self,
        token_field: &GlobalReference<TokenField>,
    ) -> Result<Option<usize>, PatternError> {
        let ptr = token_field.element_ptr();
        //check if we already produces it, if so do nothing
        if self.token_fields.contains_key(&ptr) {
            let block_num = self
                .blocks
                .iter()
                .enumerate()
                .find(|(_, block)| block.is_token_field_produced(token_field))
                .map(|(i, _)| i);
            return Ok(block_num);
        }
        //try all the blocks, find one that is able to produce it, but only one!
        let mut found = None;
        for (block_num, block) in self.blocks.iter_mut().enumerate() {
            match (found, block.produce_token_field(token_field)) {
                //this block can't add this token_field
                (_, false) => (),
                //found the first block that is able to add the token_field
                (None, true) => found = Some(block_num),
                //found the second block that is able to add the token_field
                (Some(_), true) => {
                    return Err(PatternError::AmbiguousProduction(
                        token_field.src.clone(),
                    ))
                }
            }
        }
        self.token_fields.insert(
            ptr,
            ProducedTokenField {
                local: true,
                explicit: false,
                field: token_field.clone(),
            },
        );
        Ok(found)
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), PatternError> {
        let is_finished =
            |len: &Option<ConstructorPatternLen>| match len.as_ref() {
                Some(len) => len.is_basic(),
                None => false,
            };
        //if fully solved, do nothing
        if is_finished(&self.len) {
            return Ok(());
        }
        self.blocks
            .iter_mut()
            .try_for_each(|block| block.solve(solved))?;
        //FUTURE replace with try_reduce
        let mut lens = self.blocks.iter().map(|block| block.len.clone());
        let first = ConstructorPatternLen::Basic(PatternLen::Defined(0).into());
        let final_len = lens.try_fold(first, |acc, len| acc.add(len?));

        if !is_len_finished(&final_len) {
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
        Ok(())
    }
    pub fn src(&self) -> &Span {
        &self.src
    }
    pub fn root_len(&self) -> usize {
        self.blocks.iter().map(Block::root_len).sum()
    }
    pub fn len(&self) -> Option<ConstructorPatternLen> {
        self.len
    }
    pub fn len_mut(&mut self) -> &mut Option<ConstructorPatternLen> {
        &mut self.len
    }
    pub fn sub_pattern_constraint(
        &self,
        constraint: &mut BlockConstraint,
        offset: usize,
    ) {
        let mut current = offset;
        for block in self.blocks.iter() {
            block.constraint(constraint, current);
            let Some(next_offset) = block.len.unwrap().basic().unwrap().single_len() else {
                break;
            };
            //the block have a len, but there is nothing to constraint, usually
            //is just table(s)
            current += next_offset as usize * 8;
            if current >= constraint.base.len() {
                break;
            }
        }
    }
    pub fn sub_pattern_constraint_variant(
        &self,
        constraint: &mut [BitConstraint],
    ) {
        let mut current = constraint;
        for block in self.blocks.iter() {
            block.constraint_variant(current);
            let Some(next_offset) = block.len.unwrap().basic().unwrap().single_len() else {
                break;
            };
            //the block have a len, but there is nothing to constraint, usually
            //is just table(s)
            if current.len() < next_offset as usize {
                break;
            }
            current = &mut current[next_offset as usize..];
        }
    }
    pub fn constraint(&self) -> PatternConstraint {
        PatternConstraint::new(self)
    }
    pub fn convert(self) -> FinalPattern {
        let Pattern {
            len,
            blocks,
            disassembly_vars,
            tokens: _,
            tables: _,
            token_fields: _,
            pos,
            src: _,
        } = self;
        let len = len.unwrap().basic().unwrap();
        let disassembly_vars = disassembly_vars
            .into_iter()
            .map(|(_, v)| v.convert())
            .collect();
        let blocks =
            blocks.into_iter().map(|b| b.try_into().unwrap()).collect();
        let pos = pos.into_iter().map(|b| b.convert()).collect();
        FinalPattern::new(disassembly_vars, blocks, pos, len)
    }
}
