use std::rc::Rc;

use thiserror::Error;

use crate::{InputSource, TokenField, Varnode};

use super::table::Table;
use super::varnode::Context;
use super::{disassembly, GlobalReference, InstNext, InstStart};

#[derive(Clone, Debug, Error)]
pub enum DisplayError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
    #[error("Missing Ref {0}")]
    MissingRef(InputSource),
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
    Literal(String),
}
