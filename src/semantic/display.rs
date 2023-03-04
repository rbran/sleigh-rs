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
pub struct Display {
    mneumonic: Option<String>,
    elements: Box<[DisplayScope]>,
}

impl Display {
    pub(crate) fn new(
        mneumonic: Option<String>,
        elements: Box<[DisplayScope]>,
    ) -> Self {
        Self {
            mneumonic,
            elements,
        }
    }
    pub fn mneumonic(&self) -> Option<&str> {
        self.mneumonic.as_ref().map(String::as_str)
    }
    pub fn elements<'a>(
        &'a self,
    ) -> impl Iterator<Item = DisplayScopeElements<'a>> + 'a {
        self.mneumonic
            .as_ref()
            .into_iter()
            .map(String::as_str)
            .map(DisplayScopeElements::Mneumonic)
            .chain(self.elements.iter().map(DisplayScopeElements::from))
    }
}

#[derive(Clone, Debug)]
pub(crate) enum DisplayScope {
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

impl<'a> From<&'a DisplayScope> for DisplayScopeElements<'a> {
    fn from(value: &'a DisplayScope) -> Self {
        match value {
            DisplayScope::Varnode(x) => Self::Varnode(x),
            DisplayScope::Context(x) => Self::Context(x),
            DisplayScope::TokenField(x) => Self::TokenField(x),
            DisplayScope::InstStart(x) => Self::InstStart(x),
            DisplayScope::InstNext(x) => Self::InstNext(x),
            DisplayScope::Disassembly(x) => Self::Disassembly(x),
            DisplayScope::Table(x) => Self::Table(x),
            DisplayScope::Space => Self::Space,
            DisplayScope::Literal(x) => Self::Literal(x),
        }
    }
}

#[derive(Clone, Debug)]
pub enum DisplayScopeElements<'a> {
    Varnode(&'a GlobalReference<Varnode>),
    //Bitrange(GlobalReference<Bitrange>),
    Context(&'a GlobalReference<Context>),
    TokenField(&'a GlobalReference<TokenField>),
    InstStart(&'a GlobalReference<InstStart>),
    InstNext(&'a GlobalReference<InstNext>),
    Disassembly(&'a Rc<disassembly::Variable>),
    Table(&'a GlobalReference<Table>),
    Space,
    Literal(&'a str),
    Mneumonic(&'a str),
}
