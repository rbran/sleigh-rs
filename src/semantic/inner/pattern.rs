use std::rc::Rc;

use crate::semantic;
use crate::semantic::assembly::Assembly;
use crate::semantic::inner::table::Table;
use crate::semantic::inner::GlobalScope;
use crate::semantic::pattern::{ConstraintValue, PatternError};
use crate::semantic::table::DisassemblyError;
use crate::semantic::varnode::{Varnode, VarnodeType};

use crate::syntax::block;
use crate::syntax::block::pattern::{CmpOp, Ellipsis, Op};

use super::disassembly::{ExprBuilder, ReadScope};
use super::Sleigh;

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
    first: Element,
    rest: Vec<(Op, Element)>,
}

impl TryFrom<Block> for semantic::pattern::Block {
    type Error = PatternError;
    fn try_from(mut value: Block) -> Result<Self, Self::Error> {
        let first = value.first.try_into()?;
        let rest = value
            .rest
            .drain(..)
            .map(|(op, ele)| ele.try_into().map(|ele| (op, ele)))
            .collect::<Result<_, _>>()?;
        Ok(semantic::pattern::Block { first, rest })
    }
}

impl Block {
    fn new(
        sleigh: &Sleigh,
        mut input: block::pattern::Block,
    ) -> Result<Self, PatternError> {
        Ok(Self {
            first: Element::new(sleigh, input.first)?,
            rest: input
                .rest
                .drain(..)
                .map(|(op, ele)| Element::new(sleigh, ele).map(|x| (op, x)))
                .collect::<Result<_, _>>()?,
        })
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
}

#[derive(Clone, Debug)]
pub enum Field {
    Field {
        field: Reference,
        constraint: Option<Constraint>,
    },
    SubPattern(Pattern),
}
impl TryFrom<Field> for semantic::pattern::Field {
    type Error = PatternError;
    fn try_from(value: Field) -> Result<Self, Self::Error> {
        match value {
            Field::Field { field, constraint } => {
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
                field: sleigh.get_pattern_field(field)?,
                constraint: constraint
                    .map(|x| Constraint::new(sleigh, x))
                    .transpose()?,
            },
            SubPattern(x) => Self::SubPattern(Pattern::new(sleigh, x)?),
        })
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
        Ok(Self {
            blocks: input
                .blocks
                .drain(..)
                .map(|x| Block::new(sleigh, x))
                .collect::<Result<_, _>>()?,
        })
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
            (Some(last), _) => {
                let mut drain = input.blocks.drain(..);
                //len != 0 so first is never None
                let block::pattern::Block { first, mut rest } =
                    drain.next().unwrap();
                //blocks are merged with AND op
                last.rest.push((Op::And, Element::new(sleigh, first)?));
                for (op, ele) in rest.drain(..) {
                    last.rest.push((op, Element::new(sleigh, ele)?));
                }
                drain
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
}
