use std::rc::Rc;

use thiserror::Error;

use crate::Span;

use super::table::Table;
use super::token::TokenField;
use super::varnode::{Context, Varnode};
use super::{disassembly, GlobalReference, InstNext, InstStart};

#[derive(Clone, Debug, Error)]
pub enum DisplayError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),
}

#[derive(Clone, Debug, Default)]
pub struct Display(Box<[DisplayScope]>);

impl Display {
    pub fn new(elements: Box<[DisplayScope]>) -> Self {
        Self(elements)
    }
    pub fn elements(&self) -> &[DisplayScope] {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub enum DisplayScope {
    Varnode(GlobalReference<Varnode>),
    //Bitrange(GlobalReference<Bitrange>),
    Context(GlobalReference<Context>),
    TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Disassembly(Rc<disassembly::Variable>),
    Table(GlobalReference<Table>),
    Space,
    Literal(String),
}
