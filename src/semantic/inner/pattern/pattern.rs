use std::rc::Rc;

use indexmap::IndexMap;

use crate::semantic::inner::disassembly::{Assertation, Variable};
use crate::semantic::inner::pattern::SinglePatternOrdering;
use crate::semantic::inner::table::Table;
use crate::semantic::inner::token::TokenField;
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::semantic::pattern::{PatternError, PatternLen};
use crate::semantic::token::Token;
use crate::semantic::{GlobalAnonReference, GlobalReference};
use crate::syntax::block;
use crate::Span;

use super::{
    BitConstraint, Block, ConstructorPatternLen, MultiplePatternOrdering,
    ProducedTable, ProducedTokenField,
};

pub type FinalPattern = crate::semantic::pattern::Pattern;
#[derive(Clone, Debug)]
pub enum Pattern {
    Phase1(PatternPhase1),
    Phase2(PatternPhase2),
}
/// create directly from the syntax tree.
#[derive(Clone, Debug)]
pub struct PatternBase {
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
}
#[derive(Clone, Debug)]
pub struct PatternPhase1 {
    pub base: PatternBase,
    ///len that will be gradually be calculated
    pub len: Option<ConstructorPatternLen>,
}
/// after all the blocks/sub_patterns/tables sizes have being calculated
#[derive(Clone, Debug)]
pub struct PatternPhase2 {
    pub base: PatternBase,
    /// len that was calculated previously
    pub len: PatternLen,
    /// the flat pattern len
    pub flat_len: usize,
    /// number of flat patterns before, `only != 0` if sub_pattern
    pub variants_before: usize,
    /// number of flat patterns resulted from the combination of all blocks
    pub variants_number: usize,
}
impl Pattern {
    pub fn new(
        sleigh: &Sleigh,
        input: block::pattern::Pattern,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        //phase is created directly from the syntax
        let phase1 = PatternBase::new(sleigh, input, this_table)?;
        //and we go into the phase2 and start calculating the len
        Ok(Self::Phase1(PatternPhase1::new(phase1)))
    }
    pub fn base(&self) -> &PatternBase {
        match self {
            Pattern::Phase1(ph1) => &ph1.base,
            Pattern::Phase2(ph2) => &ph2.base,
        }
    }
    pub fn base_mut(&mut self) -> &mut PatternBase {
        match self {
            Pattern::Phase1(ph1) => &mut ph1.base,
            Pattern::Phase2(ph2) => &mut ph2.base,
        }
    }
    pub fn phase2(&self) -> Option<&PatternPhase2> {
        match self {
            Pattern::Phase1(_) => None,
            Pattern::Phase2(ph2) => Some(ph2),
        }
    }
    pub fn len(&self) -> Option<ConstructorPatternLen> {
        match self {
            Pattern::Phase1(ph1) => ph1.len,
            Pattern::Phase2(ph2) => Some(ConstructorPatternLen::Basic(ph2.len)),
        }
    }
    /// return true if is fully solved
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<bool, PatternError> {
        match self {
            Pattern::Phase1(ph1) => ph1.solve(solved),
            Pattern::Phase2(_) => unreachable!(),
        }
    }
    pub fn into_phase2(&mut self, variants_prior: usize) -> &mut PatternPhase2 {
        //convert all blocks into phase2
        if let Pattern::Phase2(ph2) = self {
            return ph2;
        }
        // don't worry, the dummy value is always safe overwritten
        // by the *self assignemnt bellow
        unsafe {
            use std::mem::{swap, ManuallyDrop, MaybeUninit};
            //extract the value of self, and puth nothing there
            let mut old: MaybeUninit<Self> = MaybeUninit::uninit();
            swap(self, old.assume_init_mut());
            //consume the old self
            let Self::Phase1(ph1) = old.assume_init() else {
                unreachable!()
            };
            let base = ph1.base;
            let len = ph1.len.unwrap().basic().unwrap();

            //create the new Self (phase2)
            let mut new: ManuallyDrop<Self> = ManuallyDrop::new(Self::Phase2(
                PatternPhase2::new(base, len, variants_prior),
            ));
            //overwrite self with the new phase2
            swap(self, &mut new);
            //NOTE new is not dropped, because at this point it have nothing
        }
        let Pattern::Phase2(ph2) = self else {
            unreachable!()
        };
        ph2
    }
    pub fn convert(self) -> FinalPattern {
        let (base, len) = match self {
            Pattern::Phase1(ph1) => {
                (ph1.base, ph1.len.unwrap().basic().unwrap())
            }
            Pattern::Phase2(ph2) => (ph2.base, ph2.len),
        };
        let PatternBase {
            blocks,
            src: _,
            tables: _,
            token_fields: _,
            tokens: _,
            disassembly_vars,
            pos,
        } = base;
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

impl PatternBase {
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
            .map(|block| &block.base().token_fields)
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
            blocks.iter().map(|block| &block.base().tables).flatten()
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
            .map(|block| block.base().tokens.values())
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
            let mut block_unresolved = block.base().unresolved_token_fields();
            //remove unresolveds already produced by previous blocks
            block_unresolved.retain(|_key, unresolved| {
                self.blocks[..index].iter().any(|block| {
                    block
                        .base()
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
            .find(|(_, block)| block.base().is_table_produced(table))
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
                    block.base().is_token_field_produced(token_field)
                })
                .map(|(i, _)| i);
            return Ok(block_num);
        }
        //try all the blocks, find one that is able to produce it, but only one!
        let mut found = None;
        for (block_num, block) in self.blocks.iter_mut().enumerate() {
            match (found, block.base_mut().produce_token_field(token_field)) {
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
        self.blocks
            .iter()
            .map(|block| block.base().root_len())
            .sum()
    }
}
impl PatternPhase1 {
    pub fn new(phase1: PatternBase) -> Self {
        // if the pattern is empty, the final len, is known
        let len = phase1
            .blocks
            .is_empty()
            .then(|| PatternLen::Defined(0).into());
        Self { base: phase1, len }
    }
    pub fn len_mut(&mut self) -> &mut Option<ConstructorPatternLen> {
        &mut self.len
    }
    pub fn len(&self) -> Option<ConstructorPatternLen> {
        self.len
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<bool, PatternError> {
        //if fully solved, do nothing
        if let Some(ConstructorPatternLen::Basic(_)) = self.len {
            return Ok(true);
        }
        let finished = self
            .base
            .blocks
            .iter_mut()
            .map(|block| block.solve(solved))
            .try_fold(true, |acc, finished| -> Result<_, PatternError> {
                Ok(acc & finished?)
            })?;
        //FUTURE replace with try_reduce
        let mut lens = self.base.blocks.iter().map(|block| block.len());
        let first = ConstructorPatternLen::Basic(PatternLen::Defined(0).into());
        let final_len = lens.try_fold(first, |acc, len| acc.add(len?));

        let finished_len =
            matches!(final_len, Some(ConstructorPatternLen::Basic(_)));
        //is not possible to have a final len, but not finished
        assert_eq!(finished_len, finished);

        if !finished {
            //if not fully finished yet, request another run
            solved.iam_not_finished_location(self.base.src(), file!(), line!());
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
}
impl PatternPhase2 {
    pub fn new(
        mut base: PatternBase,
        len: PatternLen,
        variants_before: usize,
    ) -> Self {
        let mut variants_number = 1;
        //variants_before 0 means this is a root pattern
        let mut variants_before_counter = variants_before.max(1);
        let mut flat_len = 0;
        //we are ready to convert all blocks into phase2
        for block in base.blocks.iter_mut() {
            //create the block and update the variants_counter
            let block = block.into_phase2(variants_before_counter);
            variants_number *= block.variants_number;
            variants_before_counter *= block.variants_number;

            //stop parsing if the len is not defined
            if let Some(len) = block.len.single_len() {
                flat_len += usize::try_from(len).unwrap() * 8;
            } else {
                flat_len += usize::try_from(block.len.min()).unwrap() * 8;
                break;
            }
        }
        Self {
            base,
            len,
            flat_len,
            variants_before,
            variants_number,
        }
    }
    pub fn bits_produced(&self) -> usize {
        let len = self.len.single_len().unwrap_or_else(|| self.len.min());
        usize::try_from(len).unwrap() * 8
    }
    pub fn constraint(
        &self,
        variant_id: usize,
        constraint: &mut [BitConstraint],
    ) {
        let mut current = constraint;
        //NOTE map_while, if a block didn't get into phase2, it is unable to
        //be constrainted
        for block in self.base.blocks.iter().map_while(|b| b.phase2()) {
            block.constraint(variant_id, current);
            let next_offset = block.bits_produced();
            current = &mut current[next_offset..];
        }
    }
    pub fn flat_variant(
        &self,
        variant_id: usize,
        len_bits: usize,
    ) -> impl Iterator<Item = BitConstraint> + Clone {
        let mut flat = vec![BitConstraint::Unrestrained; self.flat_len];
        self.constraint(variant_id, &mut flat);
        let extra_len = len_bits.max(self.bits_produced()) - self.flat_len;
        flat.into_iter().chain(
            (0..extra_len)
                .into_iter()
                .map(|_| BitConstraint::Unrestrained),
        )
    }
    pub fn flat_variants<'a>(
        &'a self,
        len_bits: usize,
    ) -> impl Iterator<Item = impl Iterator<Item = BitConstraint> + Clone + 'a>
           + 'a
           + Clone {
        (0..self.variants_number)
            .into_iter()
            .map(move |variant_id| self.flat_variant(variant_id, len_bits))
    }
    ////7.8.1. Matching
    ////one pattern contains the other if all the cases that match the contained,
    ////also match the pattern.
    ////eg: `a` contains `b` if all cases that match `b` also match `a`. In other
    ////words `a` is a special case of `b`.
    ////NOTE the opose don't need to be true.
    pub fn ordering(&self, other: &Self) -> MultiplePatternOrdering {
        let len_bits = self.bits_produced().max(other.bits_produced());

        use BitConstraint::*;
        use SinglePatternOrdering::*;
        let mut variant_ordering = MultiplePatternOrdering::default();
        for self_variant in self.flat_variants(len_bits) {
            if self_variant.clone().any(|bit| bit.is_impossible()) {
                continue;
            }
            for other_variant in other.flat_variants(len_bits) {
                if other_variant.clone().any(|bit| bit.is_impossible()) {
                    continue;
                }
                let cmp: SinglePatternOrdering = self_variant
                    .clone()
                    .zip(other_variant)
                    .map(|(self_bit, other_bit)| match (self_bit, other_bit) {
                        (Impossible, _) | (_, Impossible) => unreachable!(),
                        (Defined(_) | Restrained, Defined(_) | Restrained)
                        | (Unrestrained, Unrestrained) => Eq,
                        (Unrestrained, _) => Contained,
                        (_, Unrestrained) => Contains,
                    })
                    .collect();
                variant_ordering.add(cmp);
            }
        }
        variant_ordering
    }
}