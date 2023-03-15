use std::ops::ControlFlow;

use indexmap::IndexMap;

use crate::semantic::inner::table::Table;
use crate::semantic::pattern::PatternError;
use crate::semantic::table::DisassemblyError;
use crate::semantic::token::Token;
use crate::semantic::{GlobalAnonReference, GlobalElement, GlobalReference};
use crate::{syntax, Span};

use super::disassembly::{Expr, ExprBuilder, ReadScope};
use super::token::TokenField;
use super::varnode::Context;
use super::Sleigh;

mod pattern;
pub use pattern::*;

mod block;
pub use block::*;

mod verification;
pub use verification::*;

mod constraint;
pub use constraint::*;

mod pattern_len;
pub use pattern_len::*;

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
        use crate::semantic::inner::disassembly::ExprElement::*;
        use crate::semantic::inner::disassembly::ReadScope::*;
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

pub type FinalProducedTable = crate::semantic::pattern::ProducedTable;
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

pub type FinalProducedTokenField = crate::semantic::pattern::ProducedTokenField;
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

pub type FinalConstraintValue = crate::semantic::pattern::ConstraintValue;
#[derive(Clone, Debug)]
pub struct ConstraintValue {
    pub expr: Expr,
}

impl ConstraintValue {
    fn new<'a>(
        sleigh: &Sleigh,
        input: syntax::block::pattern::ConstraintValue,
    ) -> Result<Self, PatternError> {
        let syntax::block::pattern::ConstraintValue { expr } = input;
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
pub struct DisassemblyBuilder<'a> {
    sleigh: &'a Sleigh,
}

impl<'a> ExprBuilder for DisassemblyBuilder<'a> {
    fn read_scope(
        &mut self,
        name: &str,
        src: &Span,
    ) -> Result<ReadScope, DisassemblyError> {
        use super::GlobalScope::*;
        match self
            .sleigh
            .get_global(name)
            .ok_or(DisassemblyError::MissingRef(src.clone()))?
        {
            TokenField(x) => Ok(ReadScope::TokenField(
                GlobalReference::from_element(x, src.clone()),
            )),
            Context(x) => Ok(ReadScope::Context(
                GlobalReference::from_element(x, src.clone()),
            )),
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
