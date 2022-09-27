use std::collections::HashMap;
use std::ops::ControlFlow;
use std::rc::Rc;

use crate::base::IntTypeU;
use crate::semantic::assembly::Assembly;
use crate::semantic::inner::table::Table;
use crate::semantic::inner::GlobalScope;
use crate::semantic::pattern::{ConstraintField, PatternError};
use crate::semantic::table::DisassemblyError;
use crate::semantic::varnode::{Varnode, VarnodeType};
use crate::syntax::block;
use crate::{semantic, InputSource, PatternLen, Token};

use super::disassembly::{ExprBuilder, ReadScope};
use super::{Sleigh, SolverStatus};
use semantic::pattern::CmpOp;

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
    //words `b` is a special case of `a`.
    //NOTE the opose don't need to be true.
    pub fn contains(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        self.0
            .iter()
            .zip(other.0.iter())
            .all(|(a, b)| match (*a, *b) {
                (true, true) => true,
                (true, false) => false,
                (false, true) => true,
                (false, false) => true,
            })
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
    /////Used to find the smallest constructor in a table, and solve the
    /////Growing/NonGrowing recrusive patterns
    //pub fn smaller(self, other: Self) -> Self {
    //    match (self, other) {
    //        (Self::Defined(x), Self::Defined(y)) => Self::Defined(x.max(y)),
    //        (
    //            Self::Defined(ix @ ax) | Self::Range { min: ix, max: ax },
    //            Self::Defined(iy @ ay) | Self::Range { min: iy, max: ay },
    //        ) => {
    //            let min = ix.min(iy);
    //            let max = ax.min(ay);
    //            if min == max {
    //                Self::Defined(min)
    //            } else {
    //                Self::Range { min, max }
    //            }
    //        }
    //        (Self::Min(x), other) => Self::Min(x.max(y)),
    //    }
    //}
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
    /////Used to find the smallest constructor in a table, and solve the
    /////table final pattern len
    //pub fn smaller(self, other: Self) -> Option<Self> {
    //    match (self, other) {
    //        //recrusives can't be the smaller
    //        (
    //            Self::NonGrowingRecursive(_) | Self::GrowingRecursive { .. },
    //            Self::NonGrowingRecursive(_) | Self::GrowingRecursive { .. },
    //        ) => None,
    //        (Self::Basic(x), Self::Basic(y)) => match (x, y) {},
    //        _ => todo!(),
    //    }
    //}
    //pub fn add_value(self, add: IntTypeU) -> Self {
    //    match self {
    //        Self::Unrestricted => self,
    //        //recursive block can't be concat at yet
    //        Self::NonGrowingRecursive(value) => {
    //            Self::GrowingRecursive(value.add_value(add))
    //        }
    //        Self::GrowingRecursive(_) => {
    //            unimplemented!("Incremental Self-Recursive")
    //        }
    //        Self::Restricted(value) => Self::Restricted(value + add),
    //        Self::Range { min, max } => Self::Range {
    //            min: min + add,
    //            max: max + add,
    //        },
    //    }
    //}
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
                //This only happen if recursive block is in a sub-pattern
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
fn is_finished(len: &Option<ConstructorPatternLen>) -> bool {
    match len.as_ref() {
        Some(len) => len.is_basic(),
        None => false,
    }
}

#[derive(Clone, Debug)]
pub struct Pattern {
    len: Option<ConstructorPatternLen>,
    products: FieldProducts,
    root_len: IntTypeU,
    blocks: Vec<Block>,
}

impl Pattern {
    pub fn new(
        sleigh: &Sleigh,
        input: block::pattern::Pattern,
        table: &Rc<Table>,
    ) -> Result<Self, PatternError> {
        let block::pattern::Pattern { mut blocks } = input;
        let blocks = blocks
            .drain(..)
            .map(|block| Block::new(sleigh, block, table))
            .collect::<Result<Vec<_>, _>>()?;
        let root_len = blocks.iter().map(Block::root_len).sum();
        let len = blocks.is_empty().then(|| PatternLen::Defined(0).into());
        let products = blocks
            .iter()
            .map(Block::produce)
            .try_fold(FieldProducts::default(), FieldProducts::or)?;

        Ok(Self {
            root_len,
            blocks,
            products,
            len,
        })
    }
    pub fn src(&self) -> &InputSource {
        //TODO block even have a src? it is the combination of with_table and
        //the constructor pattern. Use the constructor location?
        self.blocks.first().unwrap(/*TODO*/).src()
    }
    pub fn blocks(&self) -> impl Iterator<Item = &Block> {
        self.blocks.iter()
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
        let mut lens = self.blocks.iter().map(|block| block.len()).copied();
        let first = match lens.next() {
            //empty pattern
            None => {
                self.len = Some(PatternLen::Defined(0).into());
                return Ok(());
            }
            //first len is indefined, we can't calcualte the pattern len yet
            Some(None) => return Ok(()),
            Some(Some(first)) => first,
        };
        let mut final_len = first;
        for len in lens {
            //check if len is undefined
            if let Some(len) = len {
                if let Some(new_len) = final_len.add(len) {
                    final_len = new_len;
                } else {
                    //add will fail if we try to add two recursives
                    return Err(PatternError::MultipleRecursives(
                        self.src().clone(),
                    ));
                }
            } else {
                return Ok(());
            }
        }
        if self.len != Some(final_len) {
            solved.i_did_a_thing();
            self.len = Some(final_len);
        }
        //if not fully finished yet, request another run
        if !is_finished(&self.len) {
            solved.iam_not_finished_location(self.src(), file!(), line!());
        }
        Ok(())
    }
    pub fn produce(&self) -> &FieldProducts {
        &self.products
    }
    pub fn root_len(&self) -> IntTypeU {
        self.root_len
    }
    pub fn len(&self) -> &Option<ConstructorPatternLen> {
        &self.len
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
}

impl TryFrom<Pattern> for semantic::pattern::Pattern {
    type Error = PatternError;

    fn try_from(value: Pattern) -> Result<Self, Self::Error> {
        let Pattern {
            len,
            root_len: _,
            mut blocks,
            products: _,
        } = value;
        let blocks = blocks
            .drain(..)
            .map(|block| block.try_into())
            .collect::<Result<_, _>>()?;
        let len = len.unwrap().basic().unwrap();
        Ok(Self::new(blocks, len))
    }
}

#[derive(Clone, Debug)]
pub enum Block {
    //block with multiple elements unified with ORs
    Or {
        root_len: IntTypeU,
        len: Option<ConstructorPatternLen>,
        fields: Vec<FieldOr>,
        products: FieldProducts,
    },
    //block with multiple elements unified with ANDs
    And {
        left_root_len: IntTypeU,
        left_len: Option<ConstructorPatternLen>,
        left: Vec<FieldAnd>,
        right_root_len: IntTypeU,
        right_len: Option<ConstructorPatternLen>,
        right: Vec<FieldAnd>,
        products: FieldProducts,
    },
}
fn fields_and_solve(
    fields: &mut [FieldAnd],
    solved: &mut impl SolverStatus,
) -> Result<(), PatternError> {
    fields
        .iter_mut()
        .map(|field| match field {
            FieldAnd::SubPattern { sub, .. } => sub.solve(solved),
            FieldAnd::Constraint { .. } | FieldAnd::Field(_) => Ok(()),
        })
        .collect::<Result<_, _>>()
}
fn fields_and_len(
    fields: &mut [FieldAnd],
    src: &InputSource,
) -> Option<Result<ConstructorPatternLen, PatternError>> {
    //FUTURE implement this using try_reduce
    let mut lens = fields.iter().filter_map(|field| {
        match field {
            FieldAnd::Constraint {
                field: ConstraintVariable::Assembly { assembly, .. },
                ..
            }
            | FieldAnd::Field(Reference::Assembly { assembly, .. }) => {
                Some(Some(PatternLen::Defined(assembly.token_len()).into()))
            }
            FieldAnd::Constraint {
                field: ConstraintVariable::Varnode { .. },
                ..
            }
            | FieldAnd::Field(Reference::Varnode { .. }) => None,
            FieldAnd::SubPattern { sub, .. } => Some(*sub.len()),
            FieldAnd::Field(Reference::Table {
                table,
                src: _,
                self_ref: false,
            }) => {
                //on FieldAnd tables need to have a len smaller then the
                //current block
                Some(table.pattern_len().map(|table_len| table_len.into()))
            }
            FieldAnd::Field(Reference::Table {
                self_ref: true,
                table,
                ..
            }) => {
                //if self-recursive, return the table len, if resolved,
                //otherwise just return the recursive len indicator
                let table_len = table.pattern_len();
                if table_len.is_some() {
                    Some(table.pattern_len().map(|table_len| table_len.into()))
                } else {
                    Some(Some(ConstructorPatternLen::NonGrowingRecursive(
                        PatternLen::Defined(0),
                    )))
                }
            }
        }
    });
    let first = match lens.next() {
        //no elements, is an empty pattern, so zero sized
        None => return Some(Ok(PatternLen::Defined(0).into())),
        //there is a element, but it is a unknown len, just return unkown len
        Some(None) => return None,
        //have a first element, and it is a valid len
        Some(Some(first)) => first,
    };
    let mut final_len = first;
    for len in lens {
        let len = len?;
        if let Some(new_len) = final_len.greater(len) {
            final_len = new_len;
        } else {
            return Some(Err(PatternError::MultipleRecursives(src.clone())));
        }
    }
    Some(Ok(final_len))
}

impl Block {
    fn new(
        sleigh: &Sleigh,
        input: block::pattern::Block,
        table: &Rc<Table>,
    ) -> Result<Self, PatternError> {
        let block::pattern::Block { op, mut elements } = input;
        use block::pattern::*;
        let new = match op {
            Some(block::pattern::Op::Or) => {
                let fields = elements
                    .drain(..)
                    .map(|Element { field, ellipsis }| {
                        if ellipsis.is_some() {
                            return Err(todo!("Ellipsis on OR pattern?"));
                        }
                        FieldOr::new(sleigh, field, table)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let root_len = fields.iter().map(FieldOr::root_len).sum();
                let len =
                    fields.is_empty().then(|| PatternLen::Defined(0).into());

                let products = FieldProducts::or_field(
                    fields.iter().map(FieldOr::produce),
                );
                Self::Or {
                    root_len,
                    fields,
                    len,
                    products,
                }
            }
            Some(block::pattern::Op::And) | None => {
                let mut left = vec![];
                let mut right = vec![];
                let mut left_root_len = 0;
                let mut right_root_len = 0;
                for Element { field, ellipsis } in elements.drain(..) {
                    let (elements, elements_root_len) = match ellipsis {
                        None | Some(Ellipsis::Left) => {
                            (&mut left, &mut left_root_len)
                        }
                        Some(Ellipsis::Right) => {
                            (&mut right, &mut right_root_len)
                        }
                    };
                    let element = FieldAnd::new(sleigh, field, table)?;
                    *elements_root_len += element.root_len();
                    elements.push(element);
                }
                let left_len =
                    left.is_empty().then(|| PatternLen::Defined(0).into());
                let right_len =
                    right.is_empty().then(|| PatternLen::Defined(0).into());
                let products = FieldProducts::and_field(
                    left.iter().chain(right.iter()).map(FieldAnd::produce),
                )?;
                Self::And {
                    left_root_len,
                    left_len,
                    left,
                    right_root_len,
                    right_len,
                    right,
                    products,
                }
            }
        };
        Ok(new)
    }
    pub fn src(&self) -> &InputSource {
        //TODO: Is this even possible? Blocks could be a fusion of the
        //with_block and the constructor
        match self {
            Self::And { left, right, .. } => {
                left.last().or_else(|| right.last()).unwrap(/*TODO*/).src()
            }
            Self::Or { fields, .. } => fields.last().unwrap(/*TODO*/).src(),
        }
    }
    pub fn produce(&self) -> &FieldProducts {
        match self {
            Block::Or { products, .. } | Block::And { products, .. } => {
                products
            }
        }
    }
    pub fn solve<T: SolverStatus>(
        &mut self,
        solved: &mut T,
    ) -> Result<(), PatternError> {
        let src = self.src().clone();
        match self {
            Block::Or {
                root_len: _,
                len,
                fields,
                products: _,
            } => {
                //if len is already solved, there is nothing todo
                if is_finished(len) {
                    return Ok(());
                }
                fields
                    .iter_mut()
                    .map(|field| match field {
                        FieldOr::SubPattern { sub, .. } => sub.solve(solved),
                        FieldOr::Constraint { .. } => Ok(()),
                    })
                    .collect::<Result<_, _>>()?;
                let mut lens = fields.iter().map(|field| match field {
                    FieldOr::SubPattern { sub, .. } => *sub.len(),
                    FieldOr::Constraint { field, .. } => {
                        Some(PatternLen::Defined(field.len()).into())
                    }
                });
                let first = match lens.next() {
                    //no elements, is an empty pattern, so zero sized
                    None => {
                        *len = Some(PatternLen::Defined(0).into());
                        return Ok(());
                    }
                    //there is a element, but it is a unknown len, so just stop
                    Some(None) => return Ok(()),
                    //have a first element, and it is a valid len
                    Some(Some(first)) => first,
                };
                //all lens need to have the be the same
                for len in lens {
                    match len {
                        //there is a element, but it is a unknown len, so stop
                        None => return Ok(()),
                        //all lens in the pattern need to have the same len
                        Some(len) if len != first => {
                            return Err(PatternError::InvalidOrLen(src));
                        }
                        Some(_len) /*if _len == first*/ => (),
                    }
                }
                //found the len, update if diferent
                if *len != Some(first) {
                    solved.i_did_a_thing();
                    *len = Some(first);
                }
                //if not fully finished yet, request another run
                if !is_finished(len) {
                    solved.iam_not_finished_location(&src, file!(), line!());
                }
            }
            Block::And {
                left_root_len: _,
                left_len,
                left,
                right_root_len: _,
                right_len,
                right,
                products: _,
            } => {
                //if one side is not solved, try to solve it
                let mut try_solve =
                    |side: &mut [FieldAnd],
                     side_len: &mut Option<ConstructorPatternLen>|
                     -> Result<(), PatternError> {
                        if is_finished(side_len) {
                            return Ok(());
                        }
                        fields_and_solve(side, solved)?;
                        let new_len = match fields_and_len(side, &src) {
                            //have undefined len
                            None => return Ok(()),
                            //found a error
                            Some(Err(err)) => return Err(err),
                            Some(Ok(len)) => len,
                        };
                        if *side_len != Some(new_len) {
                            solved.i_did_a_thing();
                            *side_len = Some(new_len);
                        }
                        //if not fully finished yet, request another run
                        if !is_finished(side_len) {
                            solved.iam_not_finished_location(
                                &src,
                                file!(),
                                line!(),
                            );
                        }
                        Ok(())
                    };
                //solve all the elements
                try_solve(left, left_len)?;
                try_solve(right, right_len)?;
            }
        }
        Ok(())
    }
    pub fn root_len(&self) -> IntTypeU {
        match self {
            Block::Or { root_len: len, .. } => *len,
            Block::And {
                left_root_len,
                right_root_len,
                ..
            } => left_root_len + right_root_len,
        }
    }
    pub fn len(&self) -> &Option<ConstructorPatternLen> {
        match self {
            Block::Or { len, .. } => len,
            //NOTE left_len define the final len, right need to be smaller
            Block::And { left_len, .. } => left_len,
        }
    }
    pub fn constrain(&self, constraint: &mut BitSlice) {
        match self {
            Self::Or { fields, .. } => {
                let mut elements = fields.iter();
                let first = elements.next().expect(
                    "Block with Or operator but no elements is invalid",
                );
                let mut out = bitvec![0; constraint.len()];
                first.constrain(&mut out);

                let mut ele_out = bitvec![0; constraint.len()];
                for ele in elements {
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
            Self::And {
                left_root_len,
                left,
                right_root_len,
                right,
                ..
            } => {
                left.iter().for_each(|ele| ele.constraint(constraint));
                let right_pos: usize = (*left_root_len).try_into().unwrap();
                let right_constraint = &mut constraint[right_pos..];
                if *right_root_len > right_constraint.len().try_into().unwrap()
                {
                    panic!("Constraint right is too small");
                }
                right
                    .iter()
                    .for_each(|ele| ele.constraint(right_constraint));
            }
        }
    }
}
#[derive(Default)]
struct TokenFinder(Option<Rc<Token>>);
impl PatternWalker for TokenFinder {
    fn assembly(&mut self, assembly: &Rc<Assembly>) -> ControlFlow<(), ()> {
        let this_token = assembly.field().map(|field| &field.token);
        match (&mut self.0, this_token) {
            (_, None) => ControlFlow::Continue(()),
            (None, Some(this_token)) => {
                self.0 = Some(Rc::clone(this_token));
                ControlFlow::Continue(())
            }
            (Some(token), Some(this_token))
                if Rc::as_ptr(&token) == Rc::as_ptr(this_token) =>
            {
                ControlFlow::Continue(())
            }
            (Some(_token), Some(_this_token))
                /*if Rc::as_ptr(&_token) != Rc::as_ptr(_this_token)*/ =>
            {
                ControlFlow::Break(())
            }
        }
    }
}

impl TryFrom<Block> for semantic::pattern::Block {
    type Error = PatternError;
    fn try_from(value: Block) -> Result<Self, Self::Error> {
        match value {
            Block::Or {
                root_len,
                len: _,
                mut fields,
                products: _,
            } => {
                let fields = fields
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                Ok(Self::Or {
                    len: root_len,
                    fields,
                })
            }
            Block::And {
                left_root_len: _,
                left_len,
                mut left,
                right_root_len: _,
                right_len,
                mut right,
                products: _,
            } => {
                let left = left
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                let right = right
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                let left_len = left_len.unwrap().basic().unwrap();
                let right_len = right_len.unwrap().basic().unwrap();
                Ok(Self::And {
                    left,
                    left_len,
                    right,
                    right_len,
                })
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum FieldOrProduct<'a> {
    Assembly(&'a Rc<Assembly>),
    SubPattern(&'a FieldProducts),
}

//Field used in Or Expressions
#[derive(Clone, Debug)]
pub enum FieldOr {
    Constraint {
        src: InputSource,
        field: ConstraintVariable,
        constraint: Constraint,
    },
    //TODO This sub (Pattern) can only by Regular, replace this?
    SubPattern {
        src: InputSource,
        sub: Pattern,
    },
}
impl FieldOr {
    fn new(
        sleigh: &Sleigh,
        input: block::pattern::Field,
        table: &Rc<Table>,
    ) -> Result<Self, PatternError> {
        match input {
            block::pattern::Field::Field { field, constraint } => {
                let src = sleigh.input_src(field);
                //TODO decent error
                let constraint =
                    constraint.ok_or(PatternError::InvalidRef(src.clone()))?;
                let constraint = Constraint::new(sleigh, constraint)?;
                let field = sleigh.get_pattern_constraint_variable(field)?;
                Ok(Self::Constraint {
                    src,
                    field,
                    constraint,
                })
            }
            block::pattern::Field::SubPattern { sub, src } => {
                let sub = Pattern::new(sleigh, sub, table)?;
                let src = sleigh.input_src(src);
                Ok(Self::SubPattern { src, sub })
            }
        }
    }
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Constraint { src, .. } => src,
            Self::SubPattern { src, .. } => src,
        }
    }
    pub fn root_len(&self) -> IntTypeU {
        match self {
            Self::Constraint { field, .. } => field.len(),
            Self::SubPattern { sub, .. } => sub.root_len(),
        }
    }
    pub fn produce<'a>(&'a self) -> Option<FieldOrProduct<'a>> {
        match self {
            Self::Constraint {
                src: _,
                field,
                constraint: _,
            } => field.produce().map(FieldOrProduct::Assembly),
            Self::SubPattern { src: _, sub } => {
                Some(FieldOrProduct::SubPattern(sub.produce()))
            }
        }
    }
    pub fn constrain(&self, constraint: &mut BitSlice) {
        match self {
            Self::Constraint {
                field: ConstraintVariable::Assembly { assembly, .. },
                constraint: Constraint { op: CmpOp::Eq, .. },
                ..
            } => {
                if let Some(field) = assembly.field() {
                    field.bit_range.clone().into_iter().for_each(|bit| {
                        constraint.set(bit.try_into().unwrap(), true)
                    });
                }
            }
            //TODO: in some cases, in the `CmpOp::L*` is possible to restrict
            //some bits. Do it?
            Self::SubPattern { sub, .. } => sub.constrain(constraint),
            Self::Constraint { .. } => (),
        }
    }
}
impl TryFrom<FieldOr> for semantic::pattern::FieldOr {
    type Error = PatternError;
    fn try_from(value: FieldOr) -> Result<Self, Self::Error> {
        match value {
            FieldOr::Constraint {
                src,
                field,
                constraint,
            } => {
                let constraint = constraint.into();
                let field = field.into();
                Ok(Self::Constraint {
                    src,
                    field,
                    constraint,
                })
            }
            FieldOr::SubPattern { sub, src } => {
                let sub = sub.try_into()?;
                Ok(Self::SubPattern { sub, src })
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum FieldAndProduct<'a> {
    Assembly(&'a Rc<Assembly>),
    Token(&'a Rc<Token>),
    Table(&'a Rc<Table>),
    SubPattern(&'a FieldProducts),
}
#[derive(Clone, Debug)]
pub enum FieldAnd {
    Constraint {
        field: ConstraintVariable,
        constraint: Constraint,
    },
    Field(Reference),
    SubPattern {
        src: InputSource,
        sub: Pattern,
    },
}
impl FieldAnd {
    fn new(
        sleigh: &Sleigh,
        input: block::pattern::Field,
        table: &Rc<Table>,
    ) -> Result<Self, PatternError> {
        use block::pattern::Field as Input;
        let field = match input {
            Input::Field {
                field,
                constraint: Some(constraint),
            } => {
                let field = sleigh.get_pattern_constraint_variable(field)?;
                let constraint = Constraint::new(sleigh, constraint)?;
                Self::Constraint { field, constraint }
            }
            Input::Field {
                field,
                constraint: None,
            } => {
                let field = sleigh.get_pattern_field(field, table)?;
                Self::Field(field)
            }
            Input::SubPattern { src, sub } => {
                let src = sleigh.input_src(src);
                let sub = Pattern::new(sleigh, sub, table)?;
                Self::SubPattern { sub, src }
            }
        };
        Ok(field)
    }
    pub fn produce<'a>(&'a self) -> Option<FieldAndProduct<'a>> {
        match self {
            //if the constrain is assembly, only produce the token from it
            Self::Constraint {
                field,
                constraint: _,
            } => field
                .produce()
                .map(|ass| ass.field().map(|ass| &ass.token))
                .flatten()
                .map(FieldAndProduct::Token),
            Self::Field(field) => field.produce(),
            Self::SubPattern { src: _, sub } => {
                Some(FieldAndProduct::SubPattern(sub.produce()))
            }
        }
    }
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Constraint { field, .. } => field.src(),
            Self::Field(field) => field.src(),
            Self::SubPattern { src, .. } => src,
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), PatternError> {
        match self {
            FieldAnd::SubPattern { sub, src: _ } => sub.solve(solved),
            FieldAnd::Constraint { .. } | FieldAnd::Field(_) => Ok(()),
        }
    }
    pub fn root_len(&self) -> IntTypeU {
        match self {
            Self::Constraint { field, .. } => field.len(),
            Self::Field(field) => field.root_len(),
            Self::SubPattern { sub, .. } => sub.root_len(),
        }
    }
    fn constraint(&self, constraint: &mut BitSlice) {
        match self {
            Self::Constraint {
                field: ConstraintVariable::Assembly { assembly, .. },
                constraint: Constraint { op: CmpOp::Eq, .. },
                ..
            } => {
                if let Some(field) = assembly.field() {
                    field.bit_range.clone().into_iter().for_each(|bit| {
                        constraint.set(bit.try_into().unwrap(), true)
                    });
                }
            }
            //TODO: in some cases, in the `CmpOp::L*` is possible to restrict
            //some bits. Do it?
            Self::SubPattern { sub, src: _ } => sub.constrain(constraint),
            //TODO: what todo with a table?
            Self::Field(_) | Self::Constraint { .. } => (),
        }
    }
}
impl TryFrom<FieldAnd> for semantic::pattern::FieldAnd {
    type Error = PatternError;
    fn try_from(value: FieldAnd) -> Result<Self, Self::Error> {
        match value {
            FieldAnd::Constraint { field, constraint } => {
                let field = field.into();
                let constraint = constraint.into();
                Ok(Self::Constraint { field, constraint })
            }
            FieldAnd::Field(reference) => Ok(Self::Field(reference.into())),
            FieldAnd::SubPattern { sub, src } => {
                let sub = sub.try_into()?;
                Ok(Self::SubPattern { src, sub })
            }
        }
    }
}

//#[derive(Clone, Debug)]
//pub enum FieldProduct {
//    //value is not explicitly extracted, but the token that it belongs could
//    //be used to extract other values
//    Token(Rc<Token>),
//    //value is explicitly extracted
//    Assembly(Rc<Assembly>),
//    //table is built, in some cases, or in all cases
//    Table(Rc<Table>, bool),
//}

#[derive(Clone, Debug, Default)]
pub struct FieldProducts {
    tokens: HashMap<*const Token, (Rc<Token>, usize)>,
    fields: HashMap<*const Assembly, Rc<Assembly>>,
    tables: HashMap<*const Table, (Rc<Table>, bool)>,
}

impl<'a> From<FieldOrProduct<'a>> for FieldProducts {
    fn from(value: FieldOrProduct<'a>) -> Self {
        match value {
            FieldOrProduct::Assembly(ass) => {
                let mut value = Self::default();
                //TODO maybe or blocks should only produce tokens
                value
                    .fields
                    .entry(Rc::as_ptr(ass))
                    .or_insert(Rc::clone(ass));
                let token = &ass.field().unwrap().token;
                value
                    .tokens
                    .entry(Rc::as_ptr(token))
                    .or_insert((Rc::clone(token), 1));
                value
            }
            FieldOrProduct::SubPattern(sub) => sub.clone(),
        }
    }
}

//ironicly the and-block need to be `or` and the or-block need to be `and`
impl FieldProducts {
    pub fn and(mut self, other: &FieldProducts) -> Self {
        let mut new = Self::default();
        //new will contain the ass/tokens that both have, but all the tables
        //only set the always-produce to false, if only one contains
        for (key, field) in self.fields.drain() {
            if other.fields.contains_key(&key) {
                new.fields.insert(key, field);
            }
        }
        for (key, (field, field_num)) in self.tokens.drain() {
            if let Some((_, other_num)) = other.tokens.get(&key) {
                new.tokens.insert(key, (field, field_num + other_num));
            }
        }
        for (key, (table, mut always)) in self.tables.drain() {
            if always {
                if let Some((_, other_always)) = other.tables.get(&key) {
                    always &= other_always;
                }
            }
            new.tables.insert(key, (table, always));
        }
        for (key, (table, _)) in other.tables.iter() {
            new.tables.entry(*key).or_insert((Rc::clone(table), false));
        }
        new
    }
    //find fields produced in a `or` block
    pub fn or_field<'a>(
        mut fields: impl Iterator<Item = Option<FieldOrProduct<'a>>> + 'a,
    ) -> Self {
        let mut base: Self = if let Some(Some(first)) = fields.next() {
            first.into()
        } else {
            return Self::default();
        };
        for field in fields {
            let field = if let Some(field) = field {
                field.into()
            } else {
                return Self::default();
            };
            match field {
                FieldOrProduct::Assembly(ass) => {
                    //TODO maybe or blocks should only produce tokens
                    //ass/token need to be present in all orFields, or it can't
                    //be produced, so remove all ass/token, but this one, if
                    //exists.
                    //any tables should be set to not-always-produce
                    let token = &ass.field().unwrap().token;
                    let token_ptr = Rc::as_ptr(token);
                    let token = base.tokens.remove(&token_ptr);
                    let ass_ptr = Rc::as_ptr(ass);
                    let ass = base.fields.remove(&ass_ptr);
                    base.tokens.clear();
                    base.fields.clear();
                    base.tables.values_mut().for_each(
                        |(_table, always_produce)| *always_produce = false,
                    );
                    token.map(|token| base.tokens.insert(token_ptr, token));
                    ass.map(|ass| base.fields.insert(ass_ptr, ass));
                }
                FieldOrProduct::SubPattern(sub) => base = base.and(sub),
            }
        }
        base
    }
    pub fn or(mut self, other: &FieldProducts) -> Result<Self, PatternError> {
        for (key, field) in other.fields.iter() {
            match self.fields.entry(*key) {
                std::collections::hash_map::Entry::Occupied(_) => {
                    //TODO return error, here, we need a src todo so
                    todo!("Error field is dulicated on pattern {}", field.name);
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(Rc::clone(field));
                }
            }
        }
        for (key, (field, field_num)) in other.tokens.iter() {
            let (_, num) =
                self.tokens.entry(*key).or_insert((Rc::clone(field), 0));
            *num += field_num;
        }
        for (key, (table, _)) in other.tables.iter() {
            match self.tables.entry(*key) {
                std::collections::hash_map::Entry::Occupied(_) => {
                    //TODO return error, here, we need a src todo so
                    todo!("Error table is dulicated on pattern {}", table.name);
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert((Rc::clone(table), true));
                }
            }
        }
        Ok(self)
    }
    //find fields produced in a `and` block
    pub fn and_field<'a>(
        fields: impl Iterator<Item = Option<FieldAndProduct<'a>>> + 'a,
    ) -> Result<Self, PatternError> {
        fields.filter_map(|x| x).try_fold(
            FieldProducts::default(),
            |mut acc, field| match field {
                //if the ass_field already exists, this is an error, otherwise
                //just add
                FieldAndProduct::Assembly(ass) => {
                    let ptr = Rc::as_ptr(ass);
                    match acc.fields.entry(ptr) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            //TODO return error, here, we need a src todo so
                            todo!(
                                "Error field is dulicated on pattern {}",
                                ass.name
                            );
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(Rc::clone(ass));
                        }
                    }
                    Ok(acc)
                }
                //just add the token, if exists, increase the counter
                FieldAndProduct::Token(token) => {
                    let ptr = Rc::as_ptr(token);
                    let (_, counter) =
                        acc.tokens.entry(ptr).or_insert((Rc::clone(token), 0));
                    *counter += 1;
                    Ok(acc)
                }
                //table is add, if exists, return error
                FieldAndProduct::Table(table) => {
                    let ptr = Rc::as_ptr(table);
                    match acc.tables.entry(ptr) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            //TODO return error, here, we need a src todo so
                            todo!(
                                "Error table is dulicated on pattern {}",
                                table.name
                            );
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert((Rc::clone(table), true));
                        }
                    }
                    Ok(acc)
                }
                FieldAndProduct::SubPattern(sub) => acc.or(sub),
            },
        )
    }
}

#[derive(Clone, Debug)]
pub enum ConstraintVariable {
    Assembly {
        src: InputSource,
        assembly: Rc<Assembly>,
    },
    Varnode {
        src: InputSource,
        varnode: Rc<Varnode>,
    },
    //TODO Table with const export?
}
impl ConstraintVariable {
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Assembly { src, .. } | Self::Varnode { src, .. } => src,
        }
    }
    pub fn len(&self) -> IntTypeU {
        match self {
            ConstraintVariable::Assembly { assembly, .. } => {
                assembly.token_len()
            }
            ConstraintVariable::Varnode { .. } => 0,
        }
    }
    pub fn produce(&self) -> Option<&Rc<Assembly>> {
        match self {
            Self::Assembly { src: _, assembly } => Some(assembly),
            Self::Varnode { .. } => None,
        }
    }
}
impl From<ConstraintVariable> for semantic::pattern::ConstraintVariable {
    fn from(value: ConstraintVariable) -> Self {
        match value {
            ConstraintVariable::Assembly { src, assembly } => {
                Self::Assembly { src, assembly }
            }
            ConstraintVariable::Varnode { src, varnode } => {
                Self::Varnode { src, varnode }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Reference {
    Assembly {
        src: InputSource,
        assembly: Rc<Assembly>,
    },
    Varnode {
        src: InputSource,
        varnode: Rc<Varnode>,
    },
    //tables that extend are not allowed here
    Table {
        self_ref: bool,
        src: InputSource,
        table: Rc<Table>,
    },
}

impl Reference {
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Assembly { src, .. }
            | Self::Varnode { src, .. }
            | Self::Table { src, .. } => src,
        }
    }
    pub fn root_len(&self) -> IntTypeU {
        match self {
            Self::Assembly { assembly, .. } => assembly.token_len(),
            Self::Varnode { .. } => 0,
            Self::Table { .. } => 0,
        }
    }
    pub fn produce<'a>(&'a self) -> Option<FieldAndProduct<'a>> {
        match self {
            Self::Assembly { src: _, assembly } => {
                Some(FieldAndProduct::Assembly(assembly))
            }
            Self::Varnode { .. } => None,
            Self::Table {
                table,
                src: _,
                self_ref: _,
            } => Some(FieldAndProduct::Table(table)),
        }
    }
}

impl From<Reference> for semantic::pattern::Reference {
    fn from(value: Reference) -> Self {
        match value {
            Reference::Assembly { assembly, src } => {
                Self::Assembly { src, assembly }
            }
            Reference::Varnode { varnode, src } => {
                Self::Varnode { src, varnode }
            }
            Reference::Table {
                table,
                src,
                self_ref,
            } => Self::Table {
                self_ref,
                src,
                table: table.reference(),
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct Constraint {
    op: CmpOp,
    value: ConstraintField,
}
impl From<Constraint> for semantic::pattern::Constraint {
    fn from(value: Constraint) -> Self {
        Self::new(value.op, value.value)
    }
}

impl Constraint {
    fn new<'a>(
        sleigh: &Sleigh<'a>,
        input: block::pattern::Constraint<'a>,
    ) -> Result<Self, PatternError> {
        let block::pattern::Constraint { op, value } = input;
        let value = ConstraintField::new(sleigh, value)?;
        Ok(Self { op, value })
    }
}

impl ConstraintField {
    fn new<'a>(
        sleigh: &Sleigh<'a>,
        input: block::pattern::ConstraintValue<'a>,
    ) -> Result<Self, PatternError> {
        let block::pattern::ConstraintValue { expr } = input;
        let builder = DisassemblyBuilder { sleigh };
        let expr = builder.new_expr(expr)?.convert();
        Ok(Self { expr })
    }
}

#[derive(Clone, Debug)]
pub struct DisassemblyBuilder<'a, 'b> {
    sleigh: &'b Sleigh<'a>,
}

impl<'a, 'b> ExprBuilder<'a> for DisassemblyBuilder<'a, 'b> {
    fn read_scope(
        &self,
        name: &'a str,
    ) -> Result<Rc<dyn ReadScope>, DisassemblyError> {
        use super::GlobalScope::*;
        let src = self.sleigh.input_src(name);
        match self
            .sleigh
            .get_global(name)
            .ok_or(DisassemblyError::MissingRef(src.clone()))?
        {
            Assembly(x) => Ok(x.as_read()),
            Varnode(x) if matches!(x.varnode_type, VarnodeType::Context(_)) => {
                Ok(x.as_read())
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
        match block {
            Block::Or { fields, .. } => {
                fields.iter().try_for_each(|field| self.field_or(field))
            }
            Block::And { left, right, .. } => {
                left.iter().try_for_each(|field| self.field_and(field))?;
                right.iter().try_for_each(|field| self.field_and(field))
            }
        }
    }
    fn field_or(&mut self, field: &FieldOr) -> ControlFlow<B, ()> {
        match field {
            FieldOr::Constraint {
                field: ConstraintVariable::Assembly { assembly, .. },
                ..
            } => self.assembly(assembly),
            FieldOr::Constraint {
                field: ConstraintVariable::Varnode { varnode, .. },
                ..
            } => self.varnode(varnode),
            FieldOr::SubPattern { sub, .. } => self.pattern(sub),
        }
    }
    fn field_and(&mut self, field: &FieldAnd) -> ControlFlow<B, ()> {
        match field {
            FieldAnd::Constraint {
                field: ConstraintVariable::Assembly { assembly, .. },
                ..
            } => self.assembly(assembly),
            FieldAnd::Constraint {
                field: ConstraintVariable::Varnode { varnode, .. },
                ..
            } => self.varnode(varnode),
            FieldAnd::Field(field) => self.reference(field),
            FieldAnd::SubPattern { sub, .. } => self.pattern(sub),
        }
    }
    fn assembly(&mut self, _assembly: &Rc<Assembly>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn varnode(&mut self, _varnode: &Rc<Varnode>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn extension(&mut self, table: &Rc<Table>) -> ControlFlow<B, ()> {
        self.table(table)
    }
    fn reference(&mut self, reference: &Reference) -> ControlFlow<B, ()> {
        match reference {
            Reference::Assembly { assembly, .. } => self.assembly(assembly),
            Reference::Varnode { varnode, .. } => self.varnode(varnode),
            Reference::Table { table, .. } => self.table(table),
        }
    }
    fn table(&mut self, _table: &Rc<Table>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
}

impl<'a> Sleigh<'a> {
    pub fn get_pattern_constraint_variable(
        &self,
        name: &'a str,
    ) -> Result<ConstraintVariable, PatternError> {
        let src = self.input_src(name);
        let field = self
            .get_global(name)
            .ok_or(PatternError::MissingRef(src.clone()))?;
        let field = match field {
            GlobalScope::Assembly(x) => ConstraintVariable::Assembly {
                src,
                assembly: Rc::clone(x),
            },
            GlobalScope::Varnode(x) => ConstraintVariable::Varnode {
                src,
                varnode: Rc::clone(x),
            },
            _ => return Err(PatternError::InvalidRef(src)),
        };
        Ok(field)
    }
    pub fn get_pattern_field(
        &self,
        name: &'a str,
        table: &Rc<Table>,
    ) -> Result<Reference, PatternError> {
        let src = self.input_src(name);
        let field = self
            .get_global(name)
            .ok_or(PatternError::MissingRef(src.clone()))?;
        let field = match field {
            GlobalScope::Assembly(x) => Reference::Assembly {
                src,
                assembly: Rc::clone(x),
            },
            GlobalScope::Varnode(x) => Reference::Varnode {
                src,
                varnode: Rc::clone(x),
            },
            GlobalScope::Table(x) => {
                let self_ref = Rc::as_ptr(table) == Rc::as_ptr(x);
                Reference::Table {
                    self_ref,
                    src,
                    table: Rc::clone(x),
                }
            }
            _ => return Err(PatternError::InvalidRef(src)),
        };
        Ok(field)
    }
}
