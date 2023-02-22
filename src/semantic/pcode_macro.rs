use std::rc::{Rc, Weak};

use super::token::TokenField;
use super::varnode::{Bitrange, Context, Varnode};
use super::{GlobalReference, InstNext, InstStart};
use thiserror::Error;

use crate::{from_error, Span};

use super::execution::{Execution, ExecutionError};

#[derive(Clone, Debug, Error)]
#[error("PcodeMacro error: sub {sub}")]
pub struct PcodeMacroError {
    pub macro_pos: Span,
    pub sub: PcodeMacroErrorSub,
}
pub trait ToPcodeMacroError<X> {
    fn to_pcode_macro(
        self,
        macro_pos: Span,
    ) -> Result<X, PcodeMacroError>;
}
impl<'a, X, T> ToPcodeMacroError<X> for Result<X, T>
where
    T: Into<PcodeMacroErrorSub>,
{
    fn to_pcode_macro(
        self,
        macro_pos: Span,
    ) -> Result<X, PcodeMacroError> {
        self.map_err(|e| PcodeMacroError {
            macro_pos,
            sub: e.into(),
        })
    }
}
#[derive(Clone, Debug, Error)]
pub enum PcodeMacroErrorSub {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),

    #[error("Execution Error")]
    Execution(ExecutionError),
}
from_error!(PcodeMacroErrorSub, ExecutionError, Execution);

#[derive(Clone, Debug)]
pub struct Parameter {
    pub name: Rc<str>,
    //TODO
}
impl Parameter {
    pub fn new(name: &str) -> Self {
        Parameter {
            name: Rc::from(name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ReadScope {
    Varnode(GlobalReference<Varnode>),
    Context(GlobalReference<Context>),
    Bitrange(GlobalReference<Bitrange>),
    TokenField(GlobalReference<TokenField>),
    InstStart(GlobalReference<InstStart>),
    InstNext(GlobalReference<InstNext>),
    Parameter(Rc<Parameter>),
}
#[derive(Clone, Debug)]
pub enum WriteScope {
    Varnode(GlobalReference<Varnode>),
    Context(GlobalReference<Context>),
    Bitrange(GlobalReference<Bitrange>),
    Parameter(Rc<Parameter>),
}

#[derive(Debug, Clone)]
pub struct PcodeMacroInstance {
    //pub params: Vec<Rc<Parameter>>,
    pub execution: Execution,
    //me: Weak<Self>,
    pub parent: Weak<PcodeMacro>,
}

impl PcodeMacroInstance {
    pub fn new(
        execution: Execution,
        //me: Weak<Self>,
        parent: Weak<PcodeMacro>,
    ) -> Self {
        Self { execution, parent }
    }
}

#[derive(Clone, Debug)]
pub struct PcodeMacro {
    instances: Box<[Rc<PcodeMacroInstance>]>,
    //TODO use Parameter and not Variable
    //TODO but first imple dyn Read/Write Scope for final Execution
    //pub params: RefCell<IndexMap<Rc<str>, Rc<Variable>>>,
    //pub execution: RefCell<Execution>,
    //TODO
}

//use crate::processor;
impl PcodeMacro {
    pub fn new(instances: Box<[Rc<PcodeMacroInstance>]>) -> Self {
        Self { instances }
    }
    pub fn instances(&self) -> &[Rc<PcodeMacroInstance>] {
        &self.instances
    }
}
