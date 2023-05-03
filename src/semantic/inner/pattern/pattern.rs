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

use super::{Block, ConstructorPatternLen, ProducedTable, ProducedTokenField};
pub type FinalPattern = crate::semantic::pattern::Pattern;
/// create directly from the syntax tree.
#[derive(Clone, Debug)]
pub struct Pattern {
    //NOTE point after the `is` in the constructor, pattern itself don't have a
    //start, thats because it could be mixed with the `with_block` pattern
    pub src: Span,
    /// all the blocks
    pub blocks: Vec<Block>,
    /// tables produced by all the blocks
    pub tables: IndexMap<*const Table, ProducedTable>,
    /// token fields produced, both implicit and explicit, local and in
    /// sub_patterns
    pub token_fields: IndexMap<*const TokenField, ProducedTokenField>,
    /// number of times each token is produced, used to find implicit fields
    pub tokens: IndexMap<*const Token, (usize, GlobalAnonReference<Token>)>,
    //products: FieldProducts,
    /// disassasembly variables that will be created by the disassembler
    pub disassembly_vars: IndexMap<Rc<str>, Rc<Variable>>,
    /// disassembly assertations that executed after all the blocks, because it
    /// requires the `inst_next` to be calculated
    pub pos: Vec<Assertation>,
    ////token fields required by value checking, not produced locally, maybe
    //they could be produced in outer patterns (in case this is sub_pattern)
    //pub unresolved_token_fields: Vec<GlobalReference<TokenField>>,
    pub len: Option<ConstructorPatternLen>,
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

        //make sure the fields are not duplicated
        let mut token_fields: IndexMap<*const TokenField, ProducedTokenField> =
            IndexMap::new();
        use indexmap::map::Entry::*;
        for (k, produced_field) in blocks
            .iter()
            .map(|block| &block.base.token_fields)
            .flatten()
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
            blocks.iter().map(|block| &block.base.tables).flatten()
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
        let tokens_iter = blocks
            .iter()
            .map(|block| block.base.tokens.values())
            .flatten();
        for (num, token) in tokens_iter {
            tokens
                .entry(token.element_ptr())
                .and_modify(|(entry_num, _token)| *entry_num += *num)
                .or_insert_with(|| (*num, token.clone()));
        }
        //let products = FieldProducts::combine_blocks(blocks.iter())?;
        Ok(Self {
            blocks,
            len: None,
            //products,
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
            let mut block_unresolved = block.base.unresolved_token_fields();
            //remove unresolveds already produced by previous blocks
            block_unresolved.retain(|_key, unresolved| {
                self.blocks[..index].iter().any(|block| {
                    block
                        .base
                        .token_fields
                        .contains_key(&unresolved.element_ptr())
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
            .find(|(_, block)| block.base.is_table_produced(table))
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
                .find(|(_, block)| {
                    block.base.is_token_field_produced(token_field)
                })
                .map(|(i, _)| i);
            return Ok(block_num);
        }
        //try all the blocks, find one that is able to produce it, but only one!
        let mut found = None;
        for (block_num, block) in self.blocks.iter_mut().enumerate() {
            match (found, block.base.produce_token_field(token_field)) {
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
    pub fn src(&self) -> &Span {
        &self.src
    }
    pub fn root_len(&self) -> usize {
        self.blocks.iter().map(|block| block.base.root_len()).sum()
    }
    pub fn calculate_len(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<bool, PatternError> {
        //if fully solved, do nothing
        if let Some(ConstructorPatternLen::Basic(_)) = self.len {
            return Ok(true);
        }
        let finished = self
            .blocks
            .iter_mut()
            .map(|block| block.solve(solved))
            .try_fold(true, |acc, finished| -> Result<_, PatternError> {
                Ok(acc & finished?)
            })?;
        //FUTURE replace with try_reduce
        let mut lens = self.blocks.iter().map(|block| block.len());
        let first = ConstructorPatternLen::Basic(PatternLen::Defined(0).into());
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
            let block = block.into_phase2(variants_before_counter);
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
        let Pattern {
            blocks,
            src: _,
            len,
            tables: _,
            token_fields: _,
            tokens: _,
            disassembly_vars,
            pos,
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
