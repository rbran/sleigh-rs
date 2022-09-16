use std::rc::Rc;

use thiserror::Error;

use crate::InputSource;

use super::table::Table;
use super::{assembly, disassembly, varnode};

#[derive(Clone, Debug, Error)]
pub enum DisplayError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
    #[error("Missing Ref {0}")]
    MissingRef(InputSource),
}

#[derive(Clone, Debug, Default)]
pub struct Display(Vec<Element>);

impl Display {
    pub fn new(elements: Vec<Element>) -> Self {
        Self(elements)
    }
    pub fn elements(&self) -> &[Element] {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub enum Element {
    Varnode(Rc<varnode::Varnode>),
    Assembly(Rc<assembly::Assembly>),
    Disassembly(Rc<disassembly::Variable>),
    Table(Rc<Table>),
    Literal(String),
}
