use std::rc::Rc;

use crate::base::IntTypeU;
use crate::semantic::assembly::Assembly;
use crate::semantic::inner::table::Table;
use crate::semantic::inner::GlobalScope;
use crate::semantic::pattern::{ConstraintValue, PatternError};
use crate::semantic::table::DisassemblyError;
use crate::semantic::varnode::{Varnode, VarnodeType};
use crate::{semantic, InputSource};

use crate::syntax::block;
use crate::syntax::block::pattern::{CmpOp, Ellipsis, Op};

use super::disassembly::{ExprBuilder, ReadScope};
use super::Sleigh;

use bitvec::prelude::*;

#[derive(Clone, Debug, Default)]
pub struct Pattern {
    pub blocks: Vec<Block>,
}

impl TryFrom<Pattern> for semantic::pattern::Pattern {
    type Error = PatternError;

    fn try_from(mut value: Pattern) -> Result<Self, Self::Error> {
        let blocks = value
            .blocks
            .drain(..)
            .map(|x| x.try_into())
            .collect::<Result<_, _>>()?;
        Ok(semantic::pattern::Pattern { blocks })
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    op: Option<Op>,
    min_size: Option<IntTypeU>,
    elements: Vec<Element>,
}

impl Block {
    fn new(
        sleigh: &Sleigh,
        mut input: block::pattern::Block,
    ) -> Result<Self, PatternError> {
        let op = input.op;
        let elements = input
            .elements
            .drain(..)
            .map(|ele| Element::new(sleigh, ele))
            .collect::<Result<Vec<_>, _>>()?;
        let mut min_size = None;
        for ele in elements.iter() {
            let new_size = ele.field.min_size();
            match (&mut min_size, new_size) {
                (_, None) => (),
                (None, new_size) => min_size = new_size,
                (Some(min_size), Some(new_size)) if *min_size == new_size => (),
                (Some(_min_size), Some(_new_size)) => {
                    /*if *_min_size != _new_size*/
                    //TODO error here
                    return Err(PatternError::InvalidRef(ele.src().clone()));
                }
            }
        }
        Ok(Block {
            op,
            elements,
            min_size,
        })
    }
    pub fn src(&self) -> &InputSource {
        self.elements.first().unwrap(/*TODO*/).src()
    }
    pub fn constrain(&self, constraint: &mut BitSlice) {
        if matches!(&self.op, Some(Op::Or)) {
            let mut elements = self.elements.iter();
            let mut out = if let Some(first) = elements.next() {
                let mut ele_out = bitvec![0; constraint.len()];
                first.field.constraint(&mut ele_out);
                ele_out
            } else {
                unreachable!(
                    "Block with Or operator but no elements is invalid"
                )
            };

            let mut ele_out = bitvec![0; constraint.len()];
            for ele in elements {
                ele_out.set_elements(0);
                ele.field.constraint(&mut ele_out);
                for (mut out, ele) in out.iter_mut().zip(ele_out.iter()) {
                    *out = *out & *ele;
                }
            }
            for (mut out, ele) in constraint.iter_mut().zip(out.iter()) {
                *out = *out | *ele;
            }
        } else {
            self.elements
                .iter()
                .for_each(|ele| ele.field.constraint(constraint));
        }
    }
}

impl TryFrom<Block> for semantic::pattern::Block {
    type Error = PatternError;
    fn try_from(mut value: Block) -> Result<Self, Self::Error> {
        let op = value.op;
        let elements = value
            .elements
            .drain(..)
            .map(|ele| ele.try_into())
            .collect::<Result<_, _>>()?;
        Ok(semantic::pattern::Block { op, elements })
    }
}

#[derive(Clone, Debug)]
pub struct Element {
    field: Field,
    ellipsis: Option<Ellipsis>,
}
impl TryFrom<Element> for semantic::pattern::Element {
    type Error = PatternError;
    fn try_from(value: Element) -> Result<Self, Self::Error> {
        let field = value.field.try_into()?;
        let ellipsis = value.ellipsis.clone();
        Ok(Self { field, ellipsis })
    }
}

impl Element {
    fn new(
        sleigh: &Sleigh,
        input: block::pattern::Element,
    ) -> Result<Self, PatternError> {
        Ok(Self {
            field: Field::new(sleigh, input.field)?,
            ellipsis: input.ellipsis,
        })
    }
    pub fn src(&self) -> &InputSource {
        self.field.src()
    }
}

#[derive(Clone, Debug)]
pub enum Field {
    Field {
        src: InputSource,
        field: Reference,
        constraint: Option<Constraint>,
    },
    SubPattern(Pattern),
}
impl TryFrom<Field> for semantic::pattern::Field {
    type Error = PatternError;
    fn try_from(value: Field) -> Result<Self, Self::Error> {
        match value {
            Field::Field {
                field, constraint, ..
            } => {
                let field = field.into();
                let constraint = constraint.map(|x| x.into());
                Ok(Self::Field { field, constraint })
            }
            Field::SubPattern(x) => Ok(Self::SubPattern(x.try_into()?)),
        }
    }
}
impl Field {
    fn new(
        sleigh: &Sleigh,
        input: block::pattern::Field,
    ) -> Result<Self, PatternError> {
        use block::pattern::Field::*;
        Ok(match input {
            Field { field, constraint } => Self::Field {
                src: sleigh.input_src(field),
                field: sleigh.get_pattern_field(field)?,
                constraint: constraint
                    .map(|x| Constraint::new(sleigh, x))
                    .transpose()?,
            },
            SubPattern(x) => Self::SubPattern(Pattern::new(sleigh, x)?),
        })
    }
    pub fn src(&self) -> &InputSource {
        match self {
            Field::Field { src, .. } => src,
            Field::SubPattern(sub) => sub.src(),
        }
    }
    pub fn min_size(&self) -> Option<IntTypeU> {
        match self {
            Field::Field {
                field: Reference::Assembly(ass),
                ..
            } => ass.field().map(|field| field.token.size),
            Field::Field {
                field: Reference::Table(_),
                ..
            } => {
                //TODO verify that table is min_size or bigger
                None
            }
            Field::Field { .. } => None,
            Field::SubPattern(sub) => Some(sub.min_size()),
        }
    }
    fn constraint(&self, constraint: &mut BitSlice) {
        match self {
            Field::Field {
                field: Reference::Assembly(ass),
                constraint: Some(Constraint { op: CmpOp::Eq, .. }),
                ..
            } => {
                if let Some(field) = ass.field() {
                    field.bit_range.clone().into_iter().for_each(|bit| {
                        constraint.set(bit.try_into().unwrap(), true)
                    });
                }
            }
            //TODO: in some cases, in the `CmpOp::L*` is possible to restrict
            //some bits. Do it?
            Field::SubPattern(sub) => sub.constrain(constraint),
            //TODO: what todo with a table?
            _ => (),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Reference {
    Assembly(Rc<Assembly>),
    Varnode(Rc<Varnode>),
    Table(Rc<Table>),
}
impl From<Reference> for semantic::pattern::Reference {
    fn from(value: Reference) -> Self {
        match value {
            Reference::Assembly(x) => Self::Assembly(x),
            Reference::Varnode(x) => Self::Varnode(x),
            Reference::Table(x) => Self::Table(x.reference()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Constraint {
    op: CmpOp,
    value: ConstraintValue,
}
impl From<Constraint> for semantic::pattern::Constraint {
    fn from(value: Constraint) -> Self {
        Self {
            op: value.op,
            value: value.value,
        }
    }
}

impl Constraint {
    fn new<'a>(
        sleigh: &Sleigh<'a>,
        input: block::pattern::Constraint<'a>,
    ) -> Result<Self, PatternError> {
        Ok(Self {
            op: input.op,
            value: ConstraintValue::new(sleigh, input.value)?,
        })
    }
}

impl ConstraintValue {
    fn new<'a>(
        sleigh: &Sleigh<'a>,
        input: block::pattern::ConstraintValue<'a>,
    ) -> Result<Self, PatternError> {
        let builder = DisassemblyBuilder { sleigh };
        let expr = builder.new_expr(input.expr)?;
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

impl<'a> Sleigh<'a> {
    pub fn get_pattern_field(
        &self,
        name: &'a str,
    ) -> Result<Reference, PatternError> {
        let src = self.input_src(name);
        let field = self
            .get_global(name)
            .ok_or(PatternError::MissingRef(src.clone()))?;
        let field = match field {
            GlobalScope::Assembly(x) => Reference::Assembly(Rc::clone(x)),
            GlobalScope::Varnode(x) => Reference::Varnode(Rc::clone(x)),
            GlobalScope::Table(x) => Reference::Table(Rc::clone(x)),
            _ => return Err(PatternError::InvalidRef(src.clone())),
        };
        Ok(field)
    }
}

impl Pattern {
    fn new(
        sleigh: &Sleigh,
        mut input: block::pattern::Pattern,
    ) -> Result<Self, PatternError> {
        let blocks = input
            .blocks
            .drain(..)
            .map(|x| Block::new(sleigh, x))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { blocks })
    }
    pub fn extend(
        &mut self,
        sleigh: &Sleigh,
        mut input: block::pattern::Pattern,
    ) -> Result<(), PatternError> {
        //the last block from self will be combined with the first from
        //the input block with and & op
        let blocks = match (self.blocks.last_mut(), input.blocks.len()) {
            //no input, do nothing
            (_, 0) => return Ok(()),
            //self is empty, just parse the input
            (None, _) => input.blocks.drain(..),
            //self exists, merge last block with the first of the input
            (Some(last_block), _) => {
                let mut input = input.blocks.drain(..);
                //extend the last block with the first block from the with block
                let block::pattern::Block {
                    op: op_first,
                    elements: elements_first,
                } = input.next().unwrap();
                if last_block.op == Some(Op::Or) || op_first == Some(Op::Or) {
                    return Err(PatternError::InvalidMixOp(
                        last_block.elements.first().unwrap(/*TODO*/).src().clone(),
                    ));
                }
                for ele in elements_first {
                    let ele = Element::new(sleigh, ele)?;
                    let new_size = ele.field.min_size();
                    //TODO this is somewhat duplicated code
                    match (&mut last_block.min_size, new_size) {
                        (_, None) => (),
                        (None, new_size) => last_block.min_size = new_size,
                        (Some(min_size), Some(new_size))
                            if *min_size == new_size =>
                        {
                            ()
                        }
                        (Some(_min_size), Some(_new_size)) => {
                            /*if *_min_size != _new_size*/
                            //TODO error here
                            return Err(PatternError::InvalidRef(
                                ele.src().clone(),
                            ));
                        }
                    }
                    last_block.elements.push(ele);
                }
                input
            }
        };
        //parse the input
        for block in blocks {
            let block = Block::new(sleigh, block)?;
            self.blocks.push(block);
        }
        Ok(())
    }
    //pub fn new(input: block::pattern::Pattern) -> Result<Self, PatternError> {
    //    todo!()
    //}
    pub fn src(&self) -> &InputSource {
        self.blocks.first().unwrap(/*TODO*/).src()
    }
    pub fn min_size(&self) -> IntTypeU {
        self.blocks
            .iter()
            .map(|block| block.min_size.unwrap_or(0))
            .sum()
    }
    pub fn constrain(&self, constraint: &mut BitSlice) {
        let mut current = constraint;
        for block in self.blocks.iter() {
            block.constrain(current);
            current =
                &mut current[block.min_size.unwrap_or(0).try_into().unwrap()..];
        }
    }
    //TODO instead of a bit-vec where 1 is constrained and 0 not, should we
    //use a BitVec of Free/Set(1)/Set(0)? This way we can detect conflicts in
    //the constrain, such `c0102=3 & c0203=0`
    pub fn pattern_constrait(&self) -> BitVec {
        let size = self.min_size();
        let mut value = bitvec![0; size.try_into().unwrap()];
        self.constrain(&mut value);
        value
    }
}
