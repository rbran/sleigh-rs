use indexmap::IndexMap;
use std::convert::TryFrom;
use std::ops::ControlFlow;
use std::rc::Rc;

use crate::base::IntTypeU;
use crate::semantic::inner::table::Table;
use crate::semantic::inner::GlobalScope;
use crate::semantic::pattern::PatternError;
use crate::semantic::table::DisassemblyError;
use crate::semantic::{GlobalAnonReference, GlobalElement, GlobalReference};
use crate::syntax::block;
use crate::syntax::block::pattern::Op;
use crate::{semantic, InputSource, PatternLen, Token};

use super::disassembly::{Assertation, Expr, ExprBuilder, ReadScope, Variable};
use super::token::TokenField;
use super::varnode::Context;
use super::{Sleigh, SolverStatus};
use semantic::pattern::{CmpOp, Ellipsis};

use bitvec::prelude::*;

//TODO instead of a bit-vec where 1 is constrained and 0 not, should we
//use a BitVec of Free/Set(1)/Set(0)? This way we can detect conflicts in
//the constrain, such `c0102=3 & c0203=0`
#[derive(Clone, Debug, Default)]
pub struct PatternConstraint(BitVec);

impl PatternConstraint {
    //7.8.1. Matching
    //one pattern contains the other if all the cases that match the contained,
    //also match the pattern.
    //eg: `a` contains `b` if all cases that match `b` also match `a`. In other
    //words `a` is a special case of `b`.
    //NOTE the opose don't need to be true.
    pub fn contains(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        self.0
            .iter()
            .zip(other.0.iter())
            //None means `b` restrain something `a` don't, so a don't contains b
            //Some(false) means `a`/`b` are identical.
            //Some(true) means `b` is fully contained in `a`
            .try_fold(false, |acc, (a, b)| match (*a, *b) {
                (false, false) | (true, true) => Some(acc),
                (true, false) => Some(true),
                (false, true) => None,
            })
            .unwrap_or(false)
    }
}
impl PatternLen {
    pub fn new_range(min: IntTypeU, max: IntTypeU) -> Self {
        match min.cmp(&max) {
            std::cmp::Ordering::Greater => {
                unreachable!("PatternLen min({}) > max({})", min, max)
            }
            std::cmp::Ordering::Equal => Self::Defined(min),
            std::cmp::Ordering::Less => Self::Range { min, max },
        }
    }
    pub fn single_len(&self) -> Option<IntTypeU> {
        match self {
            Self::Defined(value) => Some(*value),
            Self::Min(_) | Self::Range { .. } => None,
        }
    }
    pub fn add(self, other: Self) -> Self {
        match (self, other) {
            (Self::Defined(x), Self::Defined(y)) => Self::Defined(x + y),
            (
                Self::Defined(ix @ ax) | Self::Range { min: ix, max: ax },
                Self::Defined(iy @ ay) | Self::Range { min: iy, max: ay },
            ) => {
                let min = ix + iy;
                let max = ax + ay;
                Self::Range { min, max }
            }
            (
                Self::Min(x) | Self::Defined(x) | Self::Range { min: x, .. },
                Self::Min(y) | Self::Defined(y) | Self::Range { min: y, .. },
            ) => Self::Min(x + y),
        }
    }
    ///Used to solve recursives in Growing/NonGrowing patterns
    pub fn greater(self, other: Self) -> Self {
        match (self, other) {
            (Self::Defined(x), Self::Defined(y)) => Self::Defined(x.max(y)),
            (
                Self::Defined(ix @ ax) | Self::Range { min: ix, max: ax },
                Self::Defined(iy @ ay) | Self::Range { min: iy, max: ay },
            ) => {
                let min = ix.max(iy);
                let max = ax.max(ay);
                Self::new_range(min, max)
            }
            (
                Self::Min(x) | Self::Defined(x) | Self::Range { min: x, .. },
                Self::Min(y) | Self::Defined(y) | Self::Range { min: y, .. },
            ) => Self::Min(x.max(y)),
        }
    }
    pub fn intersection(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::Defined(ix @ ax) | Self::Range { min: ix, max: ax },
                Self::Defined(iy @ ay) | Self::Range { min: iy, max: ay },
            ) => {
                let min = ix.min(iy);
                let max = ax.max(ay);
                Self::new_range(min, max)
            }
            (
                Self::Min(x) | Self::Defined(x) | Self::Range { min: x, .. },
                Self::Min(y) | Self::Defined(y) | Self::Range { min: y, .. },
            ) => Self::Min(x.min(y)),
        }
    }
}

//Describe a Block/Pattern possible len
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstructorPatternLen {
    //Cases with the pattern call it own table, NOTE: the table can only call
    //itself once.
    //self-Recursive, non-growing, basically unrestricted, but indicating
    //to tables that this don't change the table.pattern_len directly.
    //Value is the len that this constructor will generate, not including
    //the recursive itself
    NonGrowingRecursive(PatternLen),
    //self-Recrusive, growing, similat to NonGrowing, but is possible that this
    //keep calling itself, in a infinite growing patter. It is the context job
    //to limit the size of it.
    //grow, is the len of the size that will be added to the len, and non_grow,
    //is the value that was taken from NonGrowing Recursive
    GrowingRecursive {
        grow: PatternLen,
        non_grow: PatternLen,
    },

    Basic(PatternLen),
}
impl ConstructorPatternLen {
    pub fn single_len(&self) -> Option<IntTypeU> {
        match self {
            Self::Basic(basic) => basic.single_len(),
            Self::NonGrowingRecursive(_) | Self::GrowingRecursive { .. } => {
                None
            }
        }
    }
    pub fn is_basic(&self) -> bool {
        matches!(self, Self::Basic(_))
    }
    pub fn basic(&self) -> Option<PatternLen> {
        match self {
            Self::Basic(basic) => Some(*basic),
            Self::NonGrowingRecursive(_) | Self::GrowingRecursive { .. } => {
                None
            }
        }
    }
    ///if is some kind of recursive
    pub fn is_recursive(&self) -> bool {
        match self {
            Self::Basic(basic) => basic.is_recursive(),
            Self::NonGrowingRecursive(_) | Self::GrowingRecursive { .. } => {
                true
            }
        }
    }
    ///the min possible pattern len size, None means the min can't be calculated
    ///because this is a recursive and the len depends on the other constructors
    pub fn min(&self) -> Option<IntTypeU> {
        self.basic().map(|len| len.min())
    }
    ///the max possible pattern len size, None is infinite maximum possible len
    pub fn max(&self) -> Option<IntTypeU> {
        self.basic().map(|len| len.max()).flatten()
    }
    //TODO replace Option with Result?
    pub fn add(self, other: Self) -> Option<Self> {
        let new_self = match (self, other) {
            (Self::Basic(x), Self::Basic(y)) => Self::Basic(x.add(y)),
            //NonGrowingRecursize concat with a basic block, result in a
            //GrowingRecursive
            (Self::NonGrowingRecursive(non_grow), Self::Basic(basic))
            | (Self::Basic(basic), Self::NonGrowingRecursive(non_grow)) => {
                Self::GrowingRecursive {
                    grow: basic,
                    non_grow,
                }
            }
            //Growing Recursive concat with a basic, just grows
            (Self::GrowingRecursive { grow, non_grow }, Self::Basic(basic))
            | (Self::Basic(basic), Self::GrowingRecursive { grow, non_grow }) => {
                Self::GrowingRecursive {
                    grow: grow.add(basic),
                    non_grow,
                }
            }
            //a pattern can only have one SelfRecursive, so this is invalid
            (
                Self::GrowingRecursive { .. } | Self::NonGrowingRecursive(_),
                Self::GrowingRecursive { .. } | Self::NonGrowingRecursive(_),
            ) => return None,
        };
        Some(new_self)
    }
    pub fn greater(self, other: Self) -> Option<Self> {
        match (self, other) {
            (
                Self::GrowingRecursive { .. } | Self::NonGrowingRecursive(_),
                Self::GrowingRecursive { .. } | Self::NonGrowingRecursive(_),
            ) => return None,
            (Self::Basic(x), Self::Basic(y)) => Some(Self::Basic(x.greater(y))),
            (
                Self::Basic(x) | Self::NonGrowingRecursive(x),
                Self::Basic(y) | Self::NonGrowingRecursive(y),
            ) => Some(Self::NonGrowingRecursive(x.greater(y))),
            (Self::Basic(_), Self::GrowingRecursive { .. })
            | (Self::GrowingRecursive { .. }, Self::Basic(_)) => {
                //This only happen if recursive block is in a sub_pattern
                //what i think is not allowed
                unimplemented!()
            }
        }
    }
}
impl From<PatternLen> for ConstructorPatternLen {
    fn from(value: PatternLen) -> Self {
        Self::Basic(value)
    }
}
fn is_len_finished(len: &Option<ConstructorPatternLen>) -> bool {
    match len.as_ref() {
        Some(len) => len.is_basic(),
        None => false,
    }
}
struct FindValues(IndexMap<*const TokenField, GlobalReference<TokenField>>);
impl FindValues {
    fn search<'a>(
        verifications: impl Iterator<Item = &'a Verification>,
    ) -> IndexMap<*const TokenField, GlobalReference<TokenField>> {
        let mut find = Self(IndexMap::new());
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
        use semantic::inner::disassembly::ExprElement::*;
        use semantic::inner::disassembly::ReadScope::*;
        for expr_ele in value.expr.rpn.iter() {
            match expr_ele {
                Value(TokenField(ass)) => {
                    self.0
                        .entry(ass.element_ptr())
                        .or_insert_with(|| ass.clone());
                }
                Value(Integer(_) | Context(_) | InstStart(_))
                | OpUnary(_)
                | Op(_) => (),
                Value(InstNext(_) | Local(_)) => unreachable!(),
            }
        }
        ControlFlow::Continue(())
    }
}

pub type FinalProducedTable = semantic::pattern::ProducedTable;
#[derive(Clone, Debug)]
pub struct ProducedTable {
    pub table: GlobalReference<Table>,
    //pub local: bool,
    pub always: bool,
    pub recursive: bool,
}

impl From<ProducedTable> for FinalProducedTable {
    fn from(value: ProducedTable) -> Self {
        Self::new(
            value.table.convert_reference(),
            value.always,
            value.recursive,
        )
    }
}

pub type FinalProducedTokenField = semantic::pattern::ProducedTokenField;
#[derive(Clone, Debug)]
pub struct ProducedTokenField {
    //if this field is produced explicitly (on pattern) or implicitly deduced
    //the existence of by the use of it in a desassembly/execution
    pub explicit: bool,
    pub local: bool,
    pub field: GlobalReference<TokenField>,
}

impl From<ProducedTokenField> for FinalProducedTokenField {
    fn from(value: ProducedTokenField) -> Self {
        Self::new(value.field.convert_reference(), value.local, value.explicit)
    }
}

pub type FinalPattern = semantic::pattern::Pattern;
#[derive(Clone, Debug)]
pub struct Pattern {
    //NOTE point after the `is` in the constructor, pattern itself don't have a
    //start, thats because it could be mixed with the `with_block` pattern
    pub src: InputSource,
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
        let src = sleigh.input_src(&input.src);
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
            src,
            blocks,
            //products,
            len,
            tokens,
            token_fields,
            tables,
            disassembly_vars: IndexMap::new(),
            pos: vec![],
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
    pub fn src(&self) -> &InputSource {
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
    pub fn constrain(&self, constraint: &mut BitSlice) {
        let mut current = constraint;
        for block in self.blocks.iter() {
            block.constrain(current);
            let block_len = block.root_len();
            current = &mut current[block_len.try_into().unwrap()..];
        }
    }
    pub fn pattern_constrait(&self) -> PatternConstraint {
        //TODO unwrap into error
        let size = self.root_len();
        let mut value = bitvec![0; size.try_into().unwrap()];
        self.constrain(&mut value);
        PatternConstraint(value)
    }
    pub fn convert(self) -> FinalPattern {
        let Pattern {
            len,
            blocks,
            disassembly_vars,
            src: _,
            tokens: _,
            tables: _,
            token_fields: _,
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

pub type FinalBlock = crate::semantic::pattern::Block;
#[derive(Clone, Debug)]
pub struct Block {
    pub op: Op,
    pub location: InputSource,
    pub len: Option<ConstructorPatternLen>,
    //root_len: usize,

    //block produces this token this number of times
    pub tokens: IndexMap<*const Token, (usize, GlobalAnonReference<Token>)>,
    //map to make sure token_fields are produced only once
    //fields extracted, implicitly or explicity, localy or in sub_pattern
    pub token_fields: IndexMap<*const TokenField, ProducedTokenField>,
    //produced tables
    pub tables: IndexMap<*const Table, ProducedTable>,

    //verification in the order they are defined
    pub verifications: Vec<Verification>,
    pub pre: Vec<Assertation>,
    pub pos: Vec<Assertation>,
}

impl Block {
    fn new_or<'a>(
        sleigh: &Sleigh,
        location: InputSource,
        elements: impl Iterator<Item = block::pattern::Element<'a>>,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        //convert the verifications and generate the tokens/token_fields that
        //all branches produces.
        let mut tokens: Option<IndexMap<_, (usize, _)>> = None;
        let mut token_fields: Option<IndexMap<_, _>> = None;
        let branches = elements
            //convert into Verifications
            .map(|element| {
                let block::pattern::Element { field, ellipsis } = element;
                if ellipsis.is_some() {
                    //TODO error here
                    todo!("Ellipsis on OR pattern?");
                }
                match field {
                    block::pattern::Field::Field {
                        field,
                        constraint: None,
                    } => {
                        let src = sleigh.input_src(field);
                        let field = sleigh
                            .get_global(field)
                            .ok_or(PatternError::MissingRef(src.clone()))?;
                        match field {
                            GlobalScope::Table(table) => {
                                //this branch only produces a table, so no
                                //token/token_fields, so clean/empty it
                                match &mut tokens {
                                    Some(tokens) => tokens.clear(),
                                    None => tokens = Some(IndexMap::new()),
                                }
                                match &mut token_fields {
                                    Some(token_fields) => token_fields.clear(),
                                    None => {
                                        token_fields = Some(IndexMap::new())
                                    }
                                }
                                Ok(Verification::new_table(
                                    this_table, table, src, None,
                                ))
                            }
                            _ => Err(PatternError::UnrestrictedOr(src)),
                        }
                    }
                    block::pattern::Field::Field {
                        field,
                        constraint: Some(constraint),
                    } => {
                        let verification = Verification::from_constraint(
                            sleigh, field, constraint, this_table,
                        )?;
                        match &verification {
                            Verification::ContextCheck { .. }
                            | Verification::TableBuild { .. } => {
                                //this branch only produces_table/check_context,
                                //so no token/token_fields, so clean/empty it
                                match &mut tokens {
                                    Some(tokens) => tokens.clear(),
                                    None => tokens = Some(IndexMap::new()),
                                }
                                match &mut token_fields {
                                    Some(token_fields) => token_fields.clear(),
                                    None => {
                                        token_fields = Some(IndexMap::new())
                                    }
                                }
                            }
                            Verification::TokenFieldCheck { field, .. } => {
                                let field_ele = field.element();
                                let token = &field_ele.token;
                                //this branch checks a token_field, remove all
                                //tokens except for this one
                                match &mut tokens {
                                    Some(tokens) => {
                                        tokens.retain(|this_token, _| {
                                            *this_token == token.element_ptr()
                                        })
                                    }
                                    None => {
                                        tokens = Some(IndexMap::from([(
                                            token.element_ptr(),
                                            (1, token.reference()),
                                        )]))
                                    }
                                }
                                //to token_fields are produced, clean/empty it
                                match &mut token_fields {
                                    Some(token_fields) => token_fields.clear(),
                                    None => {
                                        token_fields = Some(IndexMap::new())
                                    }
                                }
                            }
                            Verification::SubPattern { .. } => {
                                unreachable!()
                            }
                        }
                        Ok(verification)
                    }
                    block::pattern::Field::SubPattern(sub) => {
                        let location = sleigh.input_src(&sub.src);
                        let pattern = Pattern::new(sleigh, sub, this_table)?;
                        //remote all the tokens that this sub_pattern don't
                        //produces or produces more then once
                        match &mut tokens {
                            Some(tokens) => {
                                tokens.retain(|this_token, (num, _token)| {
                                    let found = pattern.tokens.get(this_token);
                                    if let Some((found_num, _token)) = found {
                                        *num = (*num).max(*found_num);
                                    }
                                    found.is_some()
                                })
                            }
                            None => tokens = Some(pattern.tokens.clone()),
                        }
                        //remove all token this sub_pattern, don't produces
                        match &mut token_fields {
                            Some(fields) => fields.retain(|this_field, _| {
                                pattern.token_fields.contains_key(this_field)
                            }),
                            None => {
                                token_fields = Some(
                                    pattern
                                        .token_fields
                                        .iter()
                                        .map(|(k, v)| {
                                            let mut v = v.clone();
                                            v.local = false;
                                            (*k, v)
                                        })
                                        .collect(),
                                )
                            }
                        }
                        Ok(Verification::SubPattern { location, pattern })
                    }
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        //all tables produced should be included on the final tables list,
        //but mark always=true only tables produced in all branches
        let mut tables_iter = branches.iter();
        let mut tables: IndexMap<*const Table, ProducedTable> =
            match tables_iter.next().expect("LOGIC_ERROR: empty OR pattern") {
                //no tables in branch/no_branch
                Verification::ContextCheck { .. }
                | Verification::TokenFieldCheck { .. } => IndexMap::new(),
                //branch produces only one table, add this table
                Verification::TableBuild {
                    produced_table,
                    verification: _,
                } => IndexMap::from([(
                    produced_table.table.element_ptr(),
                    produced_table.clone(),
                )]),
                Verification::SubPattern {
                    location: _,
                    pattern,
                } => pattern.tables.clone(),
            };
        tables_iter.for_each(|verification| {
            match verification {
                //no table in this branch, mark all existing tables as
                //not always producing.
                Verification::ContextCheck { .. }
                | Verification::TokenFieldCheck { .. } => {
                    tables.values_mut().for_each(|table| table.always = false)
                }

                //branch produces only one table, add this table
                Verification::TableBuild {
                    produced_table,
                    verification: _,
                } => {
                    let ptr = produced_table.table.element_ptr();
                    //if this table don't exists, add it and mark it
                    //not_always produce
                    tables.entry(ptr).or_insert_with(|| {
                        let mut produced_table = produced_table.clone();
                        produced_table.always = false;
                        produced_table
                    });
                    //mark all other tables as not always produce
                    tables
                        .iter_mut()
                        .filter(|(k, _v)| **k != ptr)
                        .for_each(|(_k, v)| v.always = false);
                }
                //this branch can produce 0..n tables
                Verification::SubPattern {
                    location: _,
                    pattern,
                } => {
                    pattern.tables.iter().for_each(|(ptr, produced_table)| {
                        //if this table don't exists, add it and mark it
                        //not_always produce
                        tables.entry(*ptr).or_insert_with(|| {
                            let mut produced_table = produced_table.clone();
                            produced_table.always = false;
                            produced_table
                        });
                    });
                    //mark all other tables as not always produce
                    tables
                        .iter_mut()
                        .filter(|(k, _v)| !pattern.tables.contains_key(*k))
                        .for_each(|(_k, v)| v.always = false);
                }
            }
        });

        //tokens that are present in all branches of the or can produce
        //fields,
        let mut tokens_iter = branches.iter();
        let mut tokens: IndexMap<
            *const Token,
            (usize, GlobalAnonReference<Token>),
        > = match tokens_iter.next().expect("LOGIC_ERROR: empty OR pattern") {
            //no fields exported
            Verification::ContextCheck { .. }
            | Verification::TableBuild { .. } => IndexMap::new(),
            //TODO: sub_pattern export tokens?
            Verification::SubPattern { .. } => IndexMap::new(),
            //one fields exported
            Verification::TokenFieldCheck {
                field,
                op: _,
                value: _,
            } => {
                let token_field = field.element();
                let token = &token_field.token;
                IndexMap::from([(token.element_ptr(), (1, token.reference()))])
            }
        };
        for verification in tokens_iter {
            match verification {
                //no fields, just clean it
                Verification::ContextCheck { .. }
                | Verification::TableBuild { .. } => {
                    tokens.clear();
                }
                //TODO: sub_pattern export tokens?
                Verification::SubPattern { .. } => tokens.clear(),
                //one field, remove all the others
                Verification::TokenFieldCheck {
                    field,
                    op: _,
                    value: _,
                } => {
                    let token_field = field.element();
                    tokens.retain(|ptr, _token| {
                        *ptr == token_field.token.element_ptr()
                    })
                }
            }
            if tokens.is_empty() {
                //don't need to search any further, because is already empty
                break;
            }
        }
        //fields explicitly produced by all branches, are produced by the or
        //pattern
        let mut fields_iter = branches.iter();
        let mut fields: IndexMap<*const TokenField, ProducedTokenField> =
            match fields_iter.next().expect("LOGIC_ERROR: empty OR pattern") {
                //no fields exported
                Verification::ContextCheck { .. }
                | Verification::TokenFieldCheck { .. }
                | Verification::TableBuild { .. } => IndexMap::new(),
                //TODO: sub_pattern export tokens?
                Verification::SubPattern { .. } => IndexMap::new(),
                //one fields exported
            };
        for verification in fields_iter {
            match verification {
                //no fields, just clean it
                Verification::ContextCheck { .. }
                | Verification::TokenFieldCheck { .. }
                | Verification::TableBuild { .. } => {
                    fields.clear();
                }
                Verification::SubPattern {
                    location: _,
                    pattern,
                } => fields.retain(|ptr, _token| {
                    pattern.token_fields.keys().any(|sub_ptr| sub_ptr == ptr)
                }),
            }
            if fields.is_empty() {
                //don't need to search any further, because is already empty
                break;
            }
        }
        fields.values_mut().for_each(|prod| prod.local = false);

        Ok(Self {
            op: Op::Or,
            location,
            len: None,
            tokens,
            token_fields: token_fields.unwrap_or_default(),
            tables,
            verifications: branches,
            pre: vec![],
            pos: vec![],
        })
    }
    fn new_and<'a>(
        sleigh: &Sleigh,
        location: InputSource,
        elements: impl Iterator<Item = block::pattern::Element<'a>>,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        //convert into Verifications, also capturing the explicit fields
        let mut token_fields = IndexMap::new();
        use indexmap::map::Entry::*;
        //closure for identation sake
        let mut add_explicit_token_field =
            |token_field: ProducedTokenField| match token_fields
                .entry(token_field.field.element_ptr())
            {
                Occupied(entry) => {
                    let field_old: &ProducedTokenField = entry.get();
                    Err(PatternError::MultipleProduction(
                        field_old.field.location().clone(),
                        token_field.field.location().clone(),
                    ))
                }
                Vacant(entry) => {
                    entry.insert(token_field);
                    Ok(())
                }
            };
        let mut verifications = vec![];
        for element in elements {
            let block::pattern::Element { field, ellipsis } = element;
            if matches!(ellipsis, Some(Ellipsis::Left)) {
                todo!("Ellispsis on the Left")
            }
            match field {
                block::pattern::Field::Field {
                    field,
                    constraint: Some(constraint),
                } => {
                    let src = sleigh.input_src(field);
                    let block::pattern::Constraint { op: cmp_op, value } =
                        constraint;
                    let value = ConstraintValue::new(sleigh, value)?;
                    let field = sleigh
                        .get_global(field)
                        .ok_or(PatternError::MissingRef(src.clone()))?;
                    match field {
                        GlobalScope::TokenField(x) => {
                            verifications.push(Verification::new_token_field(
                                x, src, cmp_op, value,
                            ))
                        }
                        //TODO create InstStart? Does start_start exists?
                        GlobalScope::Context(x) => verifications.push(
                            Verification::new_context(x, src, cmp_op, value),
                        ),
                        GlobalScope::Table(x) => {
                            verifications.push(Verification::new_table(
                                this_table,
                                x,
                                src,
                                Some((cmp_op, value)),
                            ))
                        }
                        _ => return Err(PatternError::InvalidRef(src)),
                    };
                }

                block::pattern::Field::Field {
                    field,
                    constraint: None,
                } => {
                    let src = sleigh.input_src(field);
                    let field = sleigh
                        .get_global(field)
                        .ok_or(PatternError::MissingRef(src.clone()))?;
                    match field {
                        //could be explicitly defined, usually for display,
                        //but is not required
                        GlobalScope::Epsilon(_)
                        | GlobalScope::Varnode(_)
                        | GlobalScope::Context(_)
                        | GlobalScope::Bitrange(_) => (),
                        //token_field exported
                        GlobalScope::TokenField(token_field) => {
                            add_explicit_token_field(ProducedTokenField {
                                explicit: true,
                                local: true,
                                field: token_field.reference_from(src),
                            })?
                        }
                        GlobalScope::Table(table) => {
                            verifications.push(Verification::new_table(
                                this_table, table, src, None,
                            ))
                        }
                        _ => return Err(PatternError::InvalidRef(src)),
                    }
                }

                block::pattern::Field::SubPattern(sub) => {
                    let location = sleigh.input_src(&sub.src);
                    let pattern = Pattern::new(sleigh, sub, this_table)?;
                    //add/verify all token fields
                    pattern
                        .token_fields
                        .values()
                        .map(|prod| {
                            let mut prod = prod.clone();
                            prod.local = false;
                            prod
                        })
                        .try_for_each(&mut add_explicit_token_field)?;
                    verifications
                        .push(Verification::SubPattern { location, pattern })
                }
            }
        }

        //tokens (that can produce implicit fields) are only taken from local,
        //sub_patterns can't produce implicit token_fields
        let fields_from_verifications =
            verifications
                .iter()
                .filter_map(
                    |verification: &Verification| match &verification {
                        Verification::TokenFieldCheck { field, .. } => {
                            Some(field.element())
                        }
                        Verification::ContextCheck { .. }
                        | Verification::TableBuild { .. }
                        | Verification::SubPattern { .. } => None,
                    },
                );
        let fields_from_explicit_fields = token_fields
            .values()
            .map(|token_fields| token_fields.field.element());
        let token_from_pattern = fields_from_explicit_fields
            .chain(fields_from_verifications)
            .map(|field| field.token.reference());
        let mut tokens = IndexMap::new();
        for token in token_from_pattern {
            tokens
                .entry(token.element_ptr())
                .and_modify(|(num, _token)| {
                    *num += 1;
                })
                .or_insert_with(|| (1, token));
        }

        let tables = verifications
            .iter()
            .filter_map(Verification::tables)
            .flatten()
            .map(|produced_table| {
                (produced_table.table.element_ptr(), produced_table.clone())
            })
            .collect();

        Ok(Self {
            op: Op::And,
            location,
            len: None,
            token_fields,
            tokens,
            verifications,
            tables,
            pre: vec![],
            pos: vec![],
        })
    }
    fn new(
        sleigh: &Sleigh,
        input: block::pattern::Block,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        let block::pattern::Block {
            src,
            first,
            elements,
        } = input;
        let src = sleigh.input_src(src);

        //NOTE I'll not allow to mix `&` and `|` in the same level
        let op = match &elements[..] {
            //a single element don't care for operations, just default to and
            [] => block::pattern::Op::And,
            [(first_op, _), rest @ ..] => {
                if rest.iter().all(|(op, _)| first_op == op) {
                    *first_op
                } else {
                    return Err(PatternError::MixOperations(src.clone()));
                }
            }
        };

        let all_elements = [first]
            .into_iter()
            .chain(elements.into_iter().map(|(_op, element)| element));

        match op {
            block::pattern::Op::Or => {
                Self::new_or(sleigh, src, all_elements, this_table)
            }
            block::pattern::Op::And => {
                Self::new_and(sleigh, src, all_elements, this_table)
            }
        }
    }
    //token fields required by value comparisons that this block can't produce
    //itself
    pub fn unresolved_token_fields(
        &self,
    ) -> IndexMap<*const TokenField, GlobalReference<TokenField>> {
        let mut this_unresolved = FindValues::search(self.verifications.iter());
        //remove all token_fields that can be produced locally
        this_unresolved.retain(|_key, token_field| {
            let token_field = token_field.element();
            let token = &token_field.token;
            !self.tokens.contains_key(&token.element_ptr())
        });
        //for each sub_pattern, find unresolved token_fields
        this_unresolved.extend(
            self.verifications
                .iter()
                .map(Verification::sub_pattern)
                .flatten()
                .map(Pattern::unresolved_token_fields)
                .flatten(),
        );
        ////remove all token_fields that is produced locally
        //this_unresolved.retain(|_key, token_field| {
        //    !self.token_fields.contains_key(&token_field.element_ptr())
        //});
        this_unresolved
    }
    pub fn is_token_field_produced(
        &self,
        search_field: &GlobalReference<TokenField>,
    ) -> bool {
        self.token_fields.get(&search_field.element_ptr()).is_some()
    }
    pub fn is_table_produced(
        &self,
        token_field: &GlobalReference<Table>,
    ) -> bool {
        self.tables.get(&token_field.element_ptr()).is_some()
    }
    pub fn produce_token_field(
        &mut self,
        token_field: &GlobalReference<TokenField>,
    ) -> bool {
        if self.is_token_field_produced(token_field) {
            return true;
        }
        let token_field_element = token_field.element();
        let token = &token_field_element.token;
        let token_num_used = self
            .tokens
            .get(&token.element_ptr())
            .map(|(num, _)| *num)
            .unwrap_or(0);
        if token_num_used == 1 {
            self.token_fields.insert(
                token_field.element_ptr(),
                ProducedTokenField {
                    explicit: false,
                    field: token_field.clone(),
                    local: true,
                },
            );
            true
        } else {
            false
        }
    }
    pub fn solve_or<T: SolverStatus>(
        &mut self,
        solved: &mut T,
    ) -> Result<(), PatternError> {
        //each branch of the or is represented by each verification
        enum OrLenPossible {
            Recursive,    //recursive in `OR` is not allowed
            Unknown,      //at least one branch len is not known
            DiferenceLen, //all branchs need to have the same len
        }
        let mut branch_len_iter = self
            .verifications
            .iter()
            .map(Verification::pattern_len)
            .map(|len| match len {
                Some(ConstructorPatternLen::Basic(len)) => Ok(len),
                Some(
                    ConstructorPatternLen::NonGrowingRecursive(_)
                    | ConstructorPatternLen::GrowingRecursive { .. },
                ) => Err(OrLenPossible::Recursive),
                None => Err(OrLenPossible::Unknown),
            });
        //unwrap never happen because empty pattern default to `Op::And`
        let first = branch_len_iter.next().unwrap();
        let new_len = first.and_then(|first| {
            branch_len_iter.try_fold(first, |acc, x| {
                if x? == first {
                    Ok(acc)
                } else {
                    Err(OrLenPossible::DiferenceLen)
                }
            })
        });
        match new_len {
            //all the lens are valid, and equal
            Ok(new_len) => {
                solved.i_did_a_thing();
                self.len = Some(ConstructorPatternLen::Basic(new_len));
            }
            //at least one len is not known yet, request another
            //run to solve it
            Err(OrLenPossible::Unknown) => {
                solved.iam_not_finished_location(
                    &self.location,
                    file!(),
                    line!(),
                );
            }
            //at least one len is diferent from the others
            Err(OrLenPossible::DiferenceLen) => {
                return Err(PatternError::InvalidOrLen(self.location.clone()))
            }
            //at least one len is recursive
            Err(OrLenPossible::Recursive) => {
                return Err(PatternError::InvalidOrLen(self.location.clone()))
            }
        }
        Ok(())
    }
    pub fn solve_and<T: SolverStatus>(
        &mut self,
        solved: &mut T,
    ) -> Result<(), PatternError> {
        //the pattern len is the biggest of all tokens, all sub_patterns
        //and all tables in the pattern
        let tokens_len = self
            .tokens
            .values()
            .map(|(_num, token)| token.element().len_bytes())
            .max()
            .map(|token_len| {
                ConstructorPatternLen::Basic(PatternLen::Defined(
                    token_len.get(),
                ))
            });
        let mut verifications_len =
            self.verifications.iter().map(Verification::pattern_len);
        let first = tokens_len.unwrap_or(PatternLen::Defined(0).into());
        let final_len =
            verifications_len.try_fold(first, |acc, x| acc.greater(x?));

        if let Some(final_len) = final_len {
            //basic means fully solved
            if !final_len.is_basic() {
                //need to solve the recursive len of the pattern
                solved.iam_not_finished_location(
                    &self.location,
                    file!(),
                    line!(),
                );
            }
            if self.len != Some(final_len) {
                solved.i_did_a_thing();
                self.len = Some(final_len);
            }
        } else {
            //if not fully finished yet, request another run
            solved.iam_not_finished_location(&self.location, file!(), line!());
        }
        Ok(())
    }
    pub fn solve<T: SolverStatus>(
        &mut self,
        solved: &mut T,
    ) -> Result<(), PatternError> {
        //if len is already solved and no unresolved_token_field, there is
        //nothing todo
        if is_len_finished(&self.len) {
            return Ok(());
        }
        //call solve in all sub_patterns
        self.verifications
            .iter_mut()
            .filter_map(Verification::sub_pattern_mut)
            .map(|sub| sub.solve(solved))
            .collect::<Result<_, _>>()?;
        //TODO check all table value verifications export a const value
        match self.op {
            Op::And => self.solve_and(solved)?,
            Op::Or => self.solve_or(solved)?,
        }
        if !is_len_finished(&self.len) {
            solved.iam_not_finished_location(&self.location, file!(), line!());
        }
        Ok(())
    }
    pub fn root_len(&self) -> usize {
        match self.op {
            Op::And => {
                //get the biggest token, from all token_field verifications and
                //explicit/implicit token_fields
                let biggest_token_bytes = self
                    .tokens
                    .values()
                    .map(|(_num, token)| token.element().len_bytes().get())
                    .max()
                    .unwrap_or(0);
                let biggest_token =
                    usize::try_from(biggest_token_bytes).unwrap() * 8usize;
                //calc the len of the biggest sub_pattern
                let biggest_sub_pattern = self
                    .verifications
                    .iter()
                    .filter_map(Verification::sub_pattern)
                    .map(Pattern::root_len)
                    .max()
                    .unwrap_or(0);
                //return the biggest one
                biggest_token.max(biggest_sub_pattern)
            }
            Op::Or => {
                //get the biggest of all branches
                self.verifications
                    .iter()
                    .map(Verification::root_len)
                    .max()
                    .unwrap_or(0)
            }
        }
    }
    pub fn constrain(&self, constraint: &mut BitSlice) {
        match self.op {
            Op::Or => {
                let mut verifications_iter = self.verifications.iter();
                let first = verifications_iter
                    .next()
                    .expect("LOGIC_ERROR: Block with Or operator but no elements is invalid");
                let mut out = bitvec![0; constraint.len()];
                first.constrain(&mut out);
                let mut ele_out = bitvec![0; constraint.len()];
                for ele in verifications_iter {
                    ele_out.set_elements(0);
                    ele.constrain(&mut ele_out);
                    for (mut out, ele) in out.iter_mut().zip(ele_out.iter()) {
                        *out = *out & *ele;
                    }
                }
                for (mut out, ele) in constraint.iter_mut().zip(out.iter()) {
                    *out = *out | *ele;
                }
            }
            Op::And => self
                .verifications
                .iter()
                .for_each(|ele| ele.constrain(constraint)),
        }
    }
}
impl From<Block> for FinalBlock {
    fn from(value: Block) -> Self {
        let exported_token_fields =
            value.token_fields.values().map(|prod| &prod.field);
        let verified_token_fields =
            value.verifications.iter().filter_map(|ver| match ver {
                Verification::ContextCheck { .. }
                | Verification::TableBuild { .. }
                | Verification::SubPattern { .. } => None,
                Verification::TokenFieldCheck {
                    field,
                    op: _,
                    value: _,
                } => Some(field),
            });
        let token_len = exported_token_fields
            .chain(verified_token_fields)
            .map(|token_field| token_field.element().token.len_bytes().get())
            .max();
        let token_fields = value
            .token_fields
            .into_iter()
            .map(|(_, k)| k.into())
            .collect();
        let tables = value.tables.into_iter().map(|(_, k)| k.into()).collect();
        let verifications = value
            .verifications
            .into_iter()
            .map(|k| k.try_into().unwrap())
            .collect();
        match value.op {
            Op::And => FinalBlock::And {
                len: value.len.unwrap().basic().unwrap(),
                token_len: token_len.unwrap_or(0),
                token_fields,
                tables,
                verifications,
                pre: value.pre.into_iter().map(|x| x.convert()).collect(),
                pos: value.pos.into_iter().map(|x| x.convert()).collect(),
            },
            Op::Or => FinalBlock::Or {
                len: value.len.unwrap().basic().unwrap(),
                token_fields,
                tables,
                branches: verifications,
                pos: value
                    .pre
                    .into_iter()
                    .chain(value.pos.into_iter())
                    .map(|x| x.convert())
                    .collect(),
            },
        }
    }
}
#[derive(Default)]
struct TokenFinder(Option<GlobalElement<Token>>);
impl PatternWalker for TokenFinder {
    fn token_field(
        &mut self,
        field: &GlobalReference<TokenField>,
    ) -> ControlFlow<(), ()> {
        let this_token = &field.element().token;
        match &mut self.0 {
            None => {
                self.0 = Some(this_token.clone());
                ControlFlow::Continue(())
            }
            Some(token)
                if token.element_ptr() == this_token.element_ptr() =>
            {
                ControlFlow::Continue(())
            }
            Some(_token)
                /*if Rc::as_ptr(&_token) != Rc::as_ptr(_this_token)*/ =>
            {
                ControlFlow::Break(())
            }
        }
    }
}

pub type FinalVerification = semantic::pattern::Verification;
#[derive(Clone, Debug)]
pub enum Verification {
    ContextCheck {
        context: GlobalReference<Context>,
        op: CmpOp,
        value: ConstraintValue,
    },
    TableBuild {
        produced_table: ProducedTable,
        verification: Option<(CmpOp, ConstraintValue)>,
    },
    TokenFieldCheck {
        field: GlobalReference<TokenField>,
        op: CmpOp,
        value: ConstraintValue,
    },
    SubPattern {
        location: InputSource,
        pattern: Pattern,
    },
}
impl Verification {
    pub fn from_constraint<'a>(
        sleigh: &Sleigh,
        field: &'a str,
        constraint: block::pattern::Constraint,
        this_table: *const Table,
    ) -> Result<Self, PatternError> {
        let block::pattern::Constraint { op: cmp_op, value } = constraint;
        let value = ConstraintValue::new(sleigh, value)?;
        let src = sleigh.input_src(field);
        let field = sleigh
            .get_global(field)
            .ok_or(PatternError::MissingRef(src.clone()))?;
        match field {
            GlobalScope::TokenField(x) => Ok(Self::TokenFieldCheck {
                field: x.reference_from(src),
                op: cmp_op,
                value,
            }),
            //TODO create InstStart? Does start_start exists?
            GlobalScope::Context(x) => Ok(Self::ContextCheck {
                context: x.reference_from(src),
                op: cmp_op,
                value,
            }),
            GlobalScope::Table(x) => Ok({
                let verification = Some((cmp_op, value));
                let recursive = x.element_ptr() == this_table;
                let table = x.reference_from(src);
                Self::TableBuild {
                    produced_table: ProducedTable {
                        table,
                        always: true,
                        recursive,
                    },
                    verification,
                }
            }),
            _ => return Err(PatternError::InvalidRef(src)),
        }
    }
    pub fn new_context(
        context: &GlobalElement<Context>,
        src: InputSource,
        op: CmpOp,
        value: ConstraintValue,
    ) -> Self {
        Self::ContextCheck {
            context: context.reference_from(src),
            op,
            value,
        }
    }
    pub fn new_table(
        this_table: *const Table,
        table: &GlobalElement<Table>,
        src: InputSource,
        verification: Option<(CmpOp, ConstraintValue)>,
    ) -> Self {
        let recursive = table.element_ptr() == this_table;
        let table = table.reference_from(src);
        Self::TableBuild {
            produced_table: ProducedTable {
                table,
                always: true,
                recursive,
            },
            verification,
        }
    }
    pub fn new_token_field(
        field: &GlobalElement<TokenField>,
        src: InputSource,
        op: CmpOp,
        value: ConstraintValue,
    ) -> Self {
        Self::TokenFieldCheck {
            field: field.reference_from(src),
            op,
            value,
        }
    }
    pub fn root_len(&self) -> usize {
        match self {
            Verification::ContextCheck { .. }
            | Verification::TableBuild { .. } => 0,
            Verification::TokenFieldCheck {
                field,
                op: _,
                value: _,
            } => {
                let bytes: usize =
                    field.element().token.len_bytes().get().try_into().unwrap();
                bytes * 8usize
            }
            Verification::SubPattern {
                location: _,
                pattern,
            } => pattern.root_len(),
        }
    }
    fn tables<'a>(
        &'a self,
    ) -> Option<impl Iterator<Item = &'a ProducedTable> + 'a> {
        match self {
            Self::TokenFieldCheck { .. } | Self::ContextCheck { .. } => None,
            Self::TableBuild {
                produced_table,
                verification: _,
            } => {
                let iter: Box<dyn Iterator<Item = &'a _>> =
                    Box::new([produced_table].into_iter());
                Some(iter)
            }
            Self::SubPattern {
                location: _,
                pattern,
            } => {
                let iter: Box<dyn Iterator<Item = &'a _>> = Box::new(
                    pattern
                        .blocks
                        .iter()
                        .map(|block| block.tables.values())
                        .flatten(),
                );
                Some(iter)
            }
        }
    }
    fn sub_pattern(&self) -> Option<&Pattern> {
        match self {
            Self::TokenFieldCheck { .. }
            | Self::ContextCheck { .. }
            | Self::TableBuild { .. } => None,
            Self::SubPattern {
                location: _,
                pattern,
            } => Some(pattern),
        }
    }
    fn sub_pattern_mut(&mut self) -> Option<&mut Pattern> {
        match self {
            Self::TokenFieldCheck { .. }
            | Self::ContextCheck { .. }
            | Self::TableBuild { .. } => None,
            Self::SubPattern {
                location: _,
                pattern,
            } => Some(pattern),
        }
    }
    fn pattern_len(&self) -> Option<ConstructorPatternLen> {
        match self {
            Self::ContextCheck { .. } => {
                Some(ConstructorPatternLen::Basic(PatternLen::Defined(0)))
            }
            Self::TableBuild {
                produced_table:
                    ProducedTable {
                        table,
                        always: _,
                        recursive: true,
                    },
                verification: _,
            } => match table.element().pattern_len() {
                //if the table len is known, return it
                Some(table_len) => Some(table_len.into()),
                //otherwise the indication that this is a recursive
                None => Some(ConstructorPatternLen::NonGrowingRecursive(
                    PatternLen::Defined(0),
                )),
            },
            Self::TableBuild {
                produced_table:
                    ProducedTable {
                        table,
                        always: _,
                        recursive: false,
                    },
                verification: _,
            } => table
                .element()
                .pattern_len()
                .map(ConstructorPatternLen::Basic),
            Self::TokenFieldCheck {
                field,
                op: _,
                value: _,
            } => Some(ConstructorPatternLen::Basic(PatternLen::Defined(
                field.element().token.len_bytes().get(),
            ))),
            Self::SubPattern {
                location: _,
                pattern,
            } => pattern.len(),
        }
    }
    pub fn constrain(&self, constraint: &mut BitSlice) {
        match self {
            Self::SubPattern {
                location: _,
                pattern,
            } => pattern.constrain(constraint),
            Self::TokenFieldCheck {
                field,
                op: CmpOp::Eq | CmpOp::Ne,
                value: _,
            } => {
                let field = field.element();
                field.range().into_iter().for_each(|bit| {
                    constraint.set(bit.try_into().unwrap(), true)
                });
            }
            //TODO: in some cases, in the `CmpOp::L*` is possible to restrict
            //some bits. Do it?
            Self::TokenFieldCheck { .. }
            | Self::ContextCheck { .. }
            | Self::TableBuild { .. } => (),
        }
    }
}

impl From<Verification> for FinalVerification {
    fn from(value: Verification) -> Self {
        match value {
            Verification::ContextCheck { context, op, value } => {
                Self::ContextCheck {
                    context: context.convert_reference(),
                    op,
                    value: value.into(),
                }
            }
            Verification::TableBuild {
                produced_table,
                verification,
            } => Self::TableBuild {
                produced_table: produced_table.into(),
                verification: verification
                    .map(|(op, value)| (op, value.into())),
            },
            Verification::TokenFieldCheck { field, op, value } => {
                Self::TokenFieldCheck {
                    field: field.convert_reference(),
                    op,
                    value: value.into(),
                }
            }
            Verification::SubPattern { location, pattern } => {
                Self::SubPattern {
                    location,
                    pattern: pattern.convert(),
                }
            }
        }
    }
}

pub type FinalConstraintValue = semantic::pattern::ConstraintValue;
#[derive(Clone, Debug)]
pub struct ConstraintValue {
    pub expr: Expr,
}

impl ConstraintValue {
    fn new<'a>(
        sleigh: &Sleigh<'a>,
        input: block::pattern::ConstraintValue<'a>,
    ) -> Result<Self, PatternError> {
        let block::pattern::ConstraintValue { expr } = input;
        let mut builder = DisassemblyBuilder { sleigh };
        let expr = builder.new_expr(expr)?;
        Ok(Self { expr })
    }
}

impl From<ConstraintValue> for FinalConstraintValue {
    fn from(value: ConstraintValue) -> Self {
        Self::new(value.expr.convert())
    }
}

#[derive(Clone, Debug)]
pub struct DisassemblyBuilder<'a, 'b> {
    sleigh: &'b Sleigh<'a>,
}

impl<'a, 'b> ExprBuilder<'a> for DisassemblyBuilder<'a, 'b> {
    fn read_scope(
        &mut self,
        name: &'a str,
    ) -> Result<ReadScope, DisassemblyError> {
        use super::GlobalScope::*;
        let src = self.sleigh.input_src(name);
        match self
            .sleigh
            .get_global(name)
            .ok_or(DisassemblyError::MissingRef(src.clone()))?
        {
            TokenField(x) => {
                Ok(ReadScope::TokenField(GlobalReference::from_element(x, src)))
            }
            Context(x) => {
                Ok(ReadScope::Context(GlobalReference::from_element(x, src)))
            }
            _ => Err(DisassemblyError::InvalidRef(src.clone())),
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
            .tokens
            .values()
            .try_for_each(|(_num, token)| self.token(token))?;
        block
            .token_fields
            .values()
            .try_for_each(|field| self.token_field(&field.field))?;
        block
            .verifications
            .iter()
            .try_for_each(|x| self.verification(x))
    }
    fn token(
        &mut self,
        _token: &GlobalAnonReference<Token>,
    ) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn token_field(
        &mut self,
        _field: &GlobalReference<TokenField>,
    ) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn table(&mut self, _table: &GlobalReference<Table>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn context(
        &mut self,
        _table: &GlobalReference<Context>,
    ) -> ControlFlow<B, ()> {
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
                self.context(context)?;
                self.value(value)
            }
            Verification::TableBuild {
                produced_table,
                verification,
            } => {
                if let Some((_op, value)) = verification {
                    self.value(value)?;
                }
                self.table(&produced_table.table)
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
                self.token_field(field)
            }
        }
    }
}
