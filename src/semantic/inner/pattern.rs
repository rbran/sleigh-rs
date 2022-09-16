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
use crate::{semantic, InputSource, Token};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum PatternLen {
    #[default]
    Unrestricted,

    //self Recursive that don't add any body
    Recursive,
    //recursive with a min len
    Min(IntTypeU),

    //Non-Recursive related
    Range {
        min: IntTypeU,
        max: IntTypeU,
    },
    Restricted(IntTypeU),
}
impl PatternLen {
    pub fn min(&self) -> Option<IntTypeU> {
        match self {
            Self::Unrestricted | Self::Recursive => None,
            Self::Restricted(min)
            | Self::Min(min)
            | Self::Range { min, .. } => Some(*min),
        }
    }
    //pub fn max(&self) -> Option<IntTypeU> {
    //    match self {
    //        Self::Unrestricted | Self::Min(_) => None,
    //        Self::Restricted(max) | Self::Range { max, .. } => Some(*max),
    //    }
    //}
    pub fn is_some(&self) -> bool {
        !self.is_none()
    }
    pub fn is_none(&self) -> bool {
        matches!(self, Self::Unrestricted | Self::Recursive)
    }
    pub fn single_len(&self) -> Option<IntTypeU> {
        match self {
            Self::Restricted(value) => Some(*value),
            Self::Unrestricted
            | Self::Min(_)
            | Self::Range { .. }
            | Self::Recursive => None,
        }
    }
    pub fn combine(self, other: Self) -> Self {
        use std::cmp::Ordering::*;
        match (self, other) {
            (Self::Unrestricted, _) | (_, Self::Unrestricted) => {
                Self::Unrestricted
            }
            (Self::Recursive, other) | (other, Self::Recursive) => other,
            (Self::Restricted(x), Self::Min(y))
            | (Self::Min(x), Self::Restricted(y))
            | (Self::Min(x), Self::Min(y))
            | (Self::Min(x), Self::Range { min: y, .. })
            | (Self::Range { min: x, .. }, Self::Min(y)) => Self::Min(x.min(y)),
            (Self::Restricted(x), Self::Restricted(y)) => match x.cmp(&y) {
                Less => Self::Range { min: x, max: y },
                Equal => Self::Restricted(x),
                Greater => Self::Range { min: y, max: x },
            },
            (Self::Restricted(ix @ xa), Self::Range { min: iy, max: ay })
            | (Self::Range { min: ix, max: xa }, Self::Restricted(iy @ ay))
            | (
                Self::Range { min: ix, max: xa },
                Self::Range { min: iy, max: ay },
            ) => {
                let min = ix.min(iy);
                let max = xa.max(ay);
                if min == max {
                    Self::Restricted(min)
                } else {
                    Self::Range { min, max }
                }
            }
        }
    }
    pub fn add_value(self, add: IntTypeU) -> Self {
        match self {
            Self::Unrestricted | Self::Recursive => self,
            Self::Restricted(value) => Self::Restricted(value + add),
            Self::Min(value) => Self::Min(value + add),
            Self::Range { min, max } => Self::Range {
                min: min + add,
                max: max + add,
            },
        }
    }
    pub fn add(self, other: Self) -> Self {
        match (self, other) {
            (Self::Unrestricted, _) | (_, Self::Unrestricted) => {
                Self::Unrestricted
            }
            (Self::Recursive, _) | (_, Self::Recursive) => Self::Recursive,
            (Self::Restricted(x), Self::Min(y))
            | (Self::Min(x), Self::Restricted(y))
            | (Self::Min(x), Self::Min(y))
            | (Self::Min(x), Self::Range { min: y, .. })
            | (Self::Range { min: x, .. }, Self::Min(y)) => Self::Min(x + y),
            (Self::Restricted(x), Self::Restricted(y)) => {
                Self::Restricted(x + y)
            }
            (Self::Restricted(ix @ ax), Self::Range { min: iy, max: ay })
            | (Self::Range { min: ix, max: ax }, Self::Restricted(iy @ ay))
            | (
                Self::Range { min: ix, max: ax },
                Self::Range { min: iy, max: ay },
            ) => {
                let min = ix + iy;
                let max = ax + ay;
                Self::Range { min, max }
            }
        }
    }
}

impl TryFrom<PatternLen> for semantic::pattern::PatternLen {
    type Error = PatternError;

    fn try_from(value: PatternLen) -> Result<Self, Self::Error> {
        match value {
            PatternLen::Unrestricted => todo!(),
            PatternLen::Recursive => {
                todo!()
            }
            PatternLen::Min(min) => Ok(Self::Range { min, max: None }),
            PatternLen::Range { min, max } => Ok(Self::Range {
                min,
                max: Some(max),
            }),
            PatternLen::Restricted(value) => Ok(Self::Defined(value)),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Pattern {
    pub len: PatternLen,
    //if the pattern the table it belongs
    pub blocks: Vec<Block>,
}

impl Pattern {
    //TODO improve the new/extend interface
    //fn new(
    //    sleigh: &Sleigh,
    //    mut input: block::pattern::Pattern,
    //) -> Result<Self, PatternError> {
    //    let mut new = Self::default();
    //    new.blocks = input
    //        .blocks
    //        .drain(..)
    //        .map(|x| Block::new(sleigh, x))
    //        .collect::<Result<Vec<_>, _>>()?;
    //    Ok(new)
    //}
    pub fn extend(
        &mut self,
        sleigh: &Sleigh,
        mut input: block::pattern::Pattern,
    ) -> Result<(), PatternError> {
        //the last block from self will be combined with the first from
        //the input block with and & op
        let last_block = self.blocks.pop();
        let blocks = match (last_block, input.blocks.len()) {
            //no input, do nothing
            (_, 0) => return Ok(()),
            //self is empty, just parse the input
            (None, _) => input.blocks.drain(..),
            //self exists, merge last block with the first of the input
            (Some(last_block), _) => {
                let mut blocks = input.blocks.drain(..);
                //extend the last block with the first block from the with block
                let first = match blocks.next() {
                    Some(block::pattern::Block {
                        op: Some(block::pattern::Op::Or),
                        ..
                    }) => {
                        todo!("TODO Or extention/error")
                    }
                    None => unreachable!(),
                    Some(block) => block,
                };
                let new_block = Block::new(sleigh, first)?;
                match (last_block, new_block) {
                    (Block::Or { .. }, _) | (_, Block::Or { .. }) => {
                        todo!("TODO Or extention/error")
                    }
                    (Block::Expansive { .. }, _)
                    | (_, Block::Expansive { .. }) => {
                        todo!("TODO expansive extention/error")
                    }
                    (
                        Block::And {
                            fields: mut last_fields,
                            ..
                        },
                        Block::And {
                            fields: mut new_fields,
                            ..
                        },
                    ) => {
                        let len = None;
                        last_fields.extend(new_fields.drain(..));
                        self.blocks.push(Block::And {
                            fields: last_fields,
                            len,
                        })
                    }
                }
                blocks
            }
        };
        //parse the input
        for block in blocks {
            let block = Block::new(sleigh, block)?;
            self.blocks.push(block);
        }
        Ok(())
    }
    pub fn src(&self) -> &InputSource {
        self.blocks.first().unwrap(/*TODO*/).src()
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), PatternError> {
        if self.len.is_some() {
            return Ok(());
        }
        //FUTURE: convert into iterator with try_reduce
        let mut blocks = self.blocks.iter_mut();
        let mut len = if let Some(block) = blocks.next() {
            block.solve(solved)?;
            block.len()
        } else {
            return Ok(());
        };
        for block in blocks {
            block.solve(solved)?;
            len = len.add(block.len());
        }
        if !len.is_none() {
            solved.iam_not_finished_location(self.src(), file!(), line!());
        }
        if matches!(self.len, PatternLen::Recursive) {
            if len.is_some() {
                solved.i_did_a_thing();
                self.len = len;
            }
        } else {
            if len != self.len {
                solved.i_did_a_thing();
                self.len = len;
            }
        }
        Ok(())
    }
    pub fn len(&self) -> PatternLen {
        self.len
    }
    pub fn root_len(&self) -> IntTypeU {
        self.blocks.iter().map(|block| block.root_len()).sum()
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

    fn try_from(mut value: Pattern) -> Result<Self, Self::Error> {
        let len = value.len.try_into().unwrap(/*TODO*/);
        let blocks = value
            .blocks
            .drain(..)
            .map(|x| x.try_into())
            .collect::<Result<_, _>>()?;
        Ok(semantic::pattern::Pattern::new(len, blocks))
    }
}

#[derive(Clone, Debug)]
pub enum Block {
    //block with multiple elements unified with ORs
    Or {
        len: Option<IntTypeU>,
        fields: Vec<FieldOr>,
    },
    //block with multiple elements unified with ANDs
    And {
        len: Option<IntTypeU>,
        fields: Vec<FieldAnd>,
    },
    //TODO Pass-Through block, just call a table with optional contexts.
    //Can be both expansive and non-expansive

    //And block but with one or more sub tables that extend the size
    Expansive {
        //left/right len need to be smaller then extension min_len
        left_len: Option<IntTypeU>,
        left: Vec<FieldAnd>,
        //extension can also be Pattern, but I'll forbiden for now
        extension: Rc<Table>,
        right_len: Option<IntTypeU>,
        right: Vec<FieldAnd>,
    },
}
fn process_or(
    len: &mut Option<IntTypeU>,
    fields: &mut [FieldOr],
    solved: &mut impl SolverStatus,
) -> Result<(), PatternError> {
    let mut new_len = Some(0);
    for field in fields {
        let field_len = match field {
            FieldOr::Constraint { assembly, .. } => Some(assembly.token_len()),
            FieldOr::SubPattern(sub_pattern) => {
                sub_pattern.solve(solved)?;
                sub_pattern.len()
            }
        };
        if field_len.is_none() {
            solved.iam_not_finished_location(field.src(), file!(), line!());
        }
        new_len = new_len.zip(field_len).map(|(x, y)| x.max(y));
    }
    if *len != new_len {
        solved.i_did_a_thing();
        *len = new_len;
    }
    Ok(())
}

fn process_and(
    len: &mut Option<IntTypeU>,
    fields: &mut [FieldAnd],
    solved: &mut impl SolverStatus,
) -> Result<(), PatternError> {
    let mut new_len = Some(0);
    for field in fields {
        let field_len = match field {
            FieldAnd::Constraint {
                field: ConstraintVariable::Assembly { assembly, .. },
                ..
            }
            | FieldAnd::Field(Reference::Assembly { assembly, .. }) => {
                Some(assembly.token_len())
            }
            FieldAnd::Constraint {
                field: ConstraintVariable::Varnode { .. },
                ..
            }
            | FieldAnd::Field(Reference::Varnode { .. }) => Some(0),
            FieldAnd::SubPattern(sub_pattern) => {
                sub_pattern.solve(solved)?;
                let len = sub_pattern.len();
                if len.is_none() {
                    solved.iam_not_finished_location(
                        sub_pattern.src(),
                        file!(),
                        line!(),
                    );
                }
                len
            }
            FieldAnd::Field(Reference::Table { table, src }) => {
                //on FieldAnd tables need to have a len smaller then the
                //current block
                match table.pattern_len() {
                    PatternLen::Unrestricted => None,
                    PatternLen::Range { max: value, .. }
                    | PatternLen::Restricted(value) => Some(value),
                    PatternLen::Recursive | PatternLen::Min(_) => {
                        todo!("Invalid size at: {}", src)
                    }
                }
            }
        };
        if field_len.is_none() {
            solved.iam_not_finished_location(field.src(), file!(), line!());
        }
        new_len = new_len.zip(field_len).map(|(x, y)| x.max(y));
    }
    if *len != new_len {
        solved.i_did_a_thing();
        *len = new_len;
    }
    Ok(())
}

impl Block {
    fn new(
        sleigh: &Sleigh,
        mut input: block::pattern::Block,
    ) -> Result<Self, PatternError> {
        let expansive = input
            .elements
            .iter()
            .find(|ele| ele.ellipsis.is_some())
            .is_some();
        if expansive {
            if matches!(input.op, Some(block::pattern::Op::Or)) {
                todo!("Expansive block can only use AND");
            }
            let mut left = vec![];
            let mut right = vec![];
            let mut extension = None;
            for ele in input.elements.drain(..) {
                match ele.ellipsis {
                    //extension
                    None => {
                        let field = if let block::pattern::Field::Field {
                            field,
                            constraint: None,
                        } = ele.field
                        {
                            field
                        } else {
                            todo!("Only table is allowed to be the extension of block")
                        };
                        match sleigh.get_pattern_field(field)? {
                            Reference::Table { src, table } => {
                                extension
                                    .replace(table)
                                    .map_or(Ok(()), |_| {
                                        Err(PatternError::InvalidRef(src))
                                    })?;
                            }
                            Reference::Varnode { src, .. }
                            | Reference::Assembly { src, .. } => {
                                return Err(PatternError::InvalidRef(src))
                            }
                        }
                    }
                    //left or right
                    Some(side) => {
                        let ele = FieldAnd::new(sleigh, ele.field)?;
                        let side = match side {
                            block::pattern::Ellipsis::Left => &mut left,
                            block::pattern::Ellipsis::Right => &mut right,
                        };
                        side.push(ele);
                    }
                }
            }
            let extension = extension.unwrap(/*TODO error here*/);
            Ok(Self::Expansive {
                left_len: None,
                right_len: None,
                left,
                right,
                extension,
            })
        } else {
            let len = None;
            let new = match input.op {
                Some(block::pattern::Op::Or) => {
                    let fields = input
                        .elements
                        .drain(..)
                        .map(|ele| FieldOr::new(sleigh, ele.field))
                        .collect::<Result<_, _>>()?;
                    Self::Or { len, fields }
                }
                Some(block::pattern::Op::And) | None => {
                    let fields: Vec<FieldAnd> = input
                        .elements
                        .drain(..)
                        .map(|ele| FieldAnd::new(sleigh, ele.field))
                        .collect::<Result<_, _>>()?;
                    Self::And { len, fields }
                }
            };
            Ok(new)
        }
    }
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Or { fields, .. } => fields.last().unwrap(/*TODO*/).src(),
            Self::And { fields, .. } => fields.last().unwrap(/*TODO*/).src(),
            Self::Expansive { left, .. } => left.last().unwrap(/*TODO*/).src(),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), PatternError> {
        match self {
            Self::Or { len, fields } => process_or(len, fields, solved),
            Self::And { len, fields } => process_and(len, fields, solved),
            Self::Expansive {
                left_len,
                left,
                extension: _,
                right_len,
                right,
            } => {
                process_and(left_len, left, solved)?;
                process_and(right_len, right, solved)
            }
        }
    }
    pub fn len(&self) -> PatternLen {
        match self {
            Self::Expansive { extension, .. } => extension.pattern_len(),
            Self::Or { len, .. } | Self::And { len, .. } => {
                len.map(PatternLen::Restricted).unwrap_or_default()
            }
        }
    }
    pub fn root_len(&self) -> IntTypeU {
        match self {
            Self::Or { len, .. } | Self::And { len, .. } => len.unwrap(),
            Self::Expansive {
                left_len,
                right_len,
                ..
            } => left_len.unwrap() + right_len.unwrap(),
        }
    }
    pub fn constrain(&self, constraint: &mut BitSlice) {
        match self {
            Self::Or { len, fields } => {
                if len.unwrap() > constraint.len().try_into().unwrap() {
                    panic!("Constraint is too small");
                }
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
            Self::And { len, fields } => {
                if len.unwrap() > constraint.len().try_into().unwrap() {
                    panic!("Constraint is too small");
                }
                fields.iter().for_each(|ele| ele.constraint(constraint));
            }
            Self::Expansive {
                left_len,
                left,
                extension: _,
                right_len,
                right,
            } => {
                if left_len.unwrap() > constraint.len().try_into().unwrap() {
                    panic!("Constraint left is too small");
                }
                left.iter().for_each(|ele| ele.constraint(constraint));
                let right_pos: usize = left_len.unwrap().try_into().unwrap();
                let right_constraint = &mut constraint[right_pos..];
                if right_len.unwrap()
                    > right_constraint.len().try_into().unwrap()
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
            Block::Or { len, mut fields } => {
                let len = len.unwrap(/*TODO*/);
                let fields = fields
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                Ok(Self::Or { len, fields })
            }
            Block::And { len, mut fields } => {
                let len = len.unwrap(/*TODO*/);
                let fields = fields
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                Ok(Self::And { len, fields })
            }
            Block::Expansive {
                left_len,
                mut left,
                extension,
                right_len,
                mut right,
            } => {
                let left_len = left_len.unwrap(/*TODO*/);
                let right_len = right_len.unwrap(/*TODO*/);
                let left = left
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                let right = right
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                let extension = extension.reference();
                Ok(Self::Expansive {
                    left_len,
                    left,
                    extension,
                    right_len,
                    right,
                })
            }
        }
    }
}

//Field used in Or Expressions
#[derive(Clone, Debug)]
pub enum FieldOr {
    Constraint {
        src: InputSource,
        assembly: Rc<Assembly>,
        constraint: Constraint,
    },
    SubPattern(SubPattern),
}
impl FieldOr {
    fn new(
        sleigh: &Sleigh,
        input: block::pattern::Field,
    ) -> Result<Self, PatternError> {
        match input {
            block::pattern::Field::Field { field, constraint } => {
                let src = sleigh.input_src(field);
                //TODO decent error
                let constraint =
                    constraint.ok_or(PatternError::InvalidRef(src.clone()))?;
                let constraint = Constraint::new(sleigh, constraint)?;
                let assembly = match sleigh.get_pattern_field(field)? {
                    Reference::Assembly { assembly, .. } => assembly,
                    //TODO more decriptive error
                    _ => return Err(PatternError::InvalidRef(src)),
                };
                Ok(Self::Constraint {
                    src,
                    assembly,
                    constraint,
                })
            }
            block::pattern::Field::SubPattern(sub_pattern) => {
                SubPattern::new(sleigh, sub_pattern).map(Self::SubPattern)
            }
        }
    }
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Constraint { src, .. } => src,
            Self::SubPattern(sub_pattern) => sub_pattern.src(),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), PatternError> {
        match self {
            FieldOr::Constraint { .. } => Ok(()),
            FieldOr::SubPattern(sub_pattern) => sub_pattern.solve(solved),
        }
    }
    pub fn len(&self) -> Option<IntTypeU> {
        match self {
            Self::Constraint { assembly, .. } => Some(assembly.token_len()),
            Self::SubPattern(sub_pattern) => sub_pattern.len(),
        }
    }
    pub fn constrain(&self, constraint: &mut BitSlice) {
        match self {
            Self::Constraint {
                assembly,
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
            Self::SubPattern(sub) => sub.constraint(constraint),
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
                assembly,
                constraint,
            } => {
                let constraint = constraint.into();
                Ok(Self::Constraint {
                    src,
                    assembly,
                    constraint,
                })
            }
            FieldOr::SubPattern(sub_pattern) => {
                Ok(Self::SubPattern(sub_pattern.try_into()?))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum FieldAnd {
    Constraint {
        field: ConstraintVariable,
        constraint: Constraint,
    },
    Field(Reference),
    SubPattern(SubPattern),
}
impl FieldAnd {
    fn new(
        sleigh: &Sleigh,
        input: block::pattern::Field,
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
                let field = sleigh.get_pattern_field(field)?;
                Self::Field(field)
            }
            Input::SubPattern(x) => {
                Self::SubPattern(SubPattern::new(sleigh, x)?)
            }
        };
        Ok(field)
    }
    pub fn src(&self) -> &InputSource {
        match self {
            Self::Constraint { field, .. } => field.src(),
            Self::Field(field) => field.src(),
            Self::SubPattern(sub) => sub.src(),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), PatternError> {
        match self {
            FieldAnd::SubPattern(sub_pattern) => sub_pattern.solve(solved),
            FieldAnd::Constraint { .. } | FieldAnd::Field(_) => Ok(()),
        }
    }
    pub fn root_len(&self) -> Option<IntTypeU> {
        match self {
            Self::Constraint { field, .. } => Some(field.len()),
            Self::Field(field) => field.len().single_len(),
            Self::SubPattern(sub_pattern) => sub_pattern.len(),
        }
    }
    pub fn len(&self) -> Option<IntTypeU> {
        match self {
            Self::Constraint { field, .. } => Some(field.len()),
            Self::Field(field) => Some(
                field
                    .len()
                    .single_len()
                    .expect("Unable to get field len exact value"),
            ),
            Self::SubPattern(sub_pattern) => sub_pattern.len(),
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
            Self::SubPattern(sub) => sub.constraint(constraint),
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
            FieldAnd::SubPattern(sub_pattern) => {
                Ok(Self::SubPattern(sub_pattern.try_into()?))
            }
        }
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
    pub fn len(&self) -> PatternLen {
        match self {
            Reference::Assembly { assembly, .. } => {
                PatternLen::Restricted(assembly.token_len())
            }
            Reference::Varnode { .. } => PatternLen::Restricted(0),
            Reference::Table { table, .. } => table.pattern_len(),
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
            Reference::Table { table, src } => Self::Table {
                src,
                table: table.reference(),
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct SubPattern {
    src: InputSource,
    blocks: Vec<SubBlock>,
    //sub_pattern need to have a defined len
    len: Option<IntTypeU>,
}
impl SubPattern {
    fn new(
        sleigh: &Sleigh,
        mut input: block::pattern::Pattern,
    ) -> Result<Self, PatternError> {
        let blocks = input
            .blocks
            .drain(..)
            .map(|x| SubBlock::new(sleigh, x))
            .collect::<Result<Vec<_>, _>>()?;
        //TODO implement a src on the pattern itself (parentenses location)
        let src = blocks.get(0).unwrap().first_src().unwrap().clone();
        let len = None;
        Ok(Self { src, blocks, len })
    }
    pub fn src(&self) -> &InputSource {
        &self.src
    }
    pub fn blocks(&self) -> &[SubBlock] {
        self.blocks.as_slice()
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), PatternError> {
        if self.len.is_some() {
            return Ok(());
        }
        //FUTURE: convert into iterator with try_reduce
        let mut blocks = self.blocks.iter_mut();
        let mut len = if let Some(block) = blocks.next() {
            block.solve(solved)?;
            block.len()
        } else {
            return Ok(());
        };
        for block in blocks {
            block.solve(solved)?;
            len = len.zip(block.len()).map(|(x, y)| x + y);
        }
        if len.is_none() {
            solved.iam_not_finished_location(self.src(), file!(), line!());
        }
        if len != self.len {
            solved.i_did_a_thing();
            self.len = len;
        }
        Ok(())
    }
    pub fn len(&self) -> Option<IntTypeU> {
        self.len
    }
    fn constraint(&self, constraint: &mut BitSlice) {
        let mut current = constraint;
        for block in self.blocks.iter() {
            block.constrain(current);
            let block_len = block
                .len()
                .expect("Can't constraint pattern before fully solved");
            current = &mut current[block_len.try_into().unwrap()..];
        }
    }
}
impl TryFrom<SubPattern> for semantic::pattern::SubPattern {
    type Error = PatternError;

    fn try_from(mut value: SubPattern) -> Result<Self, Self::Error> {
        let src = value.src;
        let len = value.len.unwrap(/*TODO*/);
        let blocks = value
            .blocks
            .drain(..)
            .map(|x| x.try_into())
            .collect::<Result<_, _>>()?;
        Ok(Self::new(src, blocks, len))
    }
}

//Blocks found in sub-patterns, they never include any expanding tables
#[derive(Clone, Debug)]
pub enum SubBlock {
    //block with multiple elements unified with ORs
    Or {
        len: Option<IntTypeU>,
        fields: Vec<FieldOr>,
    },
    //block with multiple elements unified with ANDs
    And {
        len: Option<IntTypeU>,
        fields: Vec<FieldAnd>,
    },
}

impl SubBlock {
    fn new(
        sleigh: &Sleigh,
        mut input: block::pattern::Block,
    ) -> Result<Self, PatternError> {
        let expansive = input
            .elements
            .iter()
            .find(|ele| ele.ellipsis.is_some())
            .is_some();
        if expansive {
            todo!("Exapnsive block is not allowed in sub_blocks")
        }
        let len = None;
        let new = match input.op {
            Some(block::pattern::Op::Or) => {
                let fields = input
                    .elements
                    .drain(..)
                    .map(|ele| FieldOr::new(sleigh, ele.field))
                    .collect::<Result<_, _>>()?;
                Self::Or { len, fields }
            }
            Some(block::pattern::Op::And) | None => {
                let fields = input
                    .elements
                    .drain(..)
                    .map(|ele| FieldAnd::new(sleigh, ele.field))
                    .collect::<Result<_, _>>()?;
                Self::And { len, fields }
            }
        };
        Ok(new)
    }
    fn first_src(&self) -> Option<&InputSource> {
        match self {
            SubBlock::Or { fields, .. } => fields.get(0).map(|x| x.src()),
            SubBlock::And { fields, .. } => fields.get(0).map(|x| x.src()),
        }
    }
    pub fn solve(
        &mut self,
        solved: &mut impl SolverStatus,
    ) -> Result<(), PatternError> {
        match self {
            SubBlock::Or { len, fields } => process_or(len, fields, solved),
            SubBlock::And { len, fields } => process_and(len, fields, solved),
        }
    }
    pub fn len(&self) -> Option<IntTypeU> {
        match self {
            SubBlock::Or { len, .. } => *len,
            SubBlock::And { len, .. } => *len,
        }
    }
    fn constrain(&self, constraint: &mut BitSlice) {
        match self {
            SubBlock::Or { len, fields } => {
                if len.unwrap() > constraint.len().try_into().unwrap() {
                    panic!("Constraint is too small");
                }
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
            SubBlock::And { len, fields } => {
                if len.unwrap() > constraint.len().try_into().unwrap() {
                    panic!("Constraint is too small");
                }
                fields.iter().for_each(|ele| ele.constraint(constraint));
            }
        }
    }
}
impl TryFrom<SubBlock> for semantic::pattern::SubBlock {
    type Error = PatternError;
    fn try_from(value: SubBlock) -> Result<Self, Self::Error> {
        let mut token = TokenFinder::default();
        if token.sub_block(&value).is_break() {
            todo!("Error here, unreachable?");
        }
        let token = token.0;
        match value {
            SubBlock::Or { len, mut fields } => {
                let len = len.unwrap(/*TODO*/);
                let fields = fields
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                Ok(Self::Or { len, fields, token })
            }
            SubBlock::And { len, mut fields } => {
                let len = len.unwrap(/*TODO*/);
                let fields = fields
                    .drain(..)
                    .map(|field| field.try_into())
                    .collect::<Result<_, _>>()?;
                Ok(Self::And { len, fields, token })
            }
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
        Ok(Self {
            op: input.op,
            value: ConstraintField::new(sleigh, input.value)?,
        })
    }
}

impl ConstraintField {
    fn new<'a>(
        sleigh: &Sleigh<'a>,
        input: block::pattern::ConstraintValue<'a>,
    ) -> Result<Self, PatternError> {
        let builder = DisassemblyBuilder { sleigh };
        let expr = builder.new_expr(input.expr)?.convert();
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
    fn start(&mut self, pattern: &Pattern) -> ControlFlow<B, ()> {
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
            Block::And { fields, .. } => {
                fields.iter().try_for_each(|field| self.field_and(field))
            }
            Block::Expansive {
                left,
                extension,
                right,
                ..
            } => {
                left.iter().try_for_each(|field| self.field_and(field))?;
                right.iter().try_for_each(|field| self.field_and(field))?;
                self.extension(extension)
            }
        }
    }
    fn field_or(&mut self, field: &FieldOr) -> ControlFlow<B, ()> {
        match field {
            FieldOr::Constraint { assembly, .. } => self.assembly(assembly),
            FieldOr::SubPattern(sub_pattern) => self.sub_pattern(sub_pattern),
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
            FieldAnd::SubPattern(sub_pattern) => self.sub_pattern(sub_pattern),
        }
    }
    fn assembly(&mut self, _assembly: &Rc<Assembly>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn varnode(&mut self, _varnode: &Rc<Varnode>) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn extension(&mut self, _table: &Table) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn sub_pattern(&mut self, sub_pattern: &SubPattern) -> ControlFlow<B, ()> {
        sub_pattern
            .blocks
            .iter()
            .try_for_each(|block| self.sub_block(block))
    }
    fn reference(&mut self, reference: &Reference) -> ControlFlow<B, ()> {
        match reference {
            Reference::Assembly { assembly, .. } => self.assembly(assembly),
            Reference::Varnode { varnode, .. } => self.varnode(varnode),
            Reference::Table { table, .. } => self.table(table),
        }
    }
    fn table(&mut self, _table: &Table) -> ControlFlow<B, ()> {
        ControlFlow::Continue(())
    }
    fn sub_block(&mut self, sub_block: &SubBlock) -> ControlFlow<B, ()> {
        match sub_block {
            SubBlock::Or { fields, .. } => {
                fields.iter().try_for_each(|field| self.field_or(field))
            }
            SubBlock::And { fields, .. } => {
                fields.iter().try_for_each(|field| self.field_and(field))
            }
        }
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
            GlobalScope::Table(x) => Reference::Table {
                src,
                table: Rc::clone(x),
            },
            _ => return Err(PatternError::InvalidRef(src)),
        };
        Ok(field)
    }
}
