use std::cell::RefCell;
use std::rc::{Rc, Weak};

use super::assembly::Assembly;
use super::varnode::Varnode;
use thiserror::Error;

use crate::{from_error, InputSource};

use super::execution::{Execution, ExecutionError};

#[derive(Clone, Debug, Error)]
#[error("PcodeMacro error: sub {sub}")]
pub struct PcodeMacroError {
    pub macro_pos: InputSource,
    pub sub: PcodeMacroErrorSub,
}
pub trait ToPcodeMacroError<X> {
    fn to_pcode_macro(
        self,
        macro_pos: InputSource,
    ) -> Result<X, PcodeMacroError>;
}
impl<'a, X, T> ToPcodeMacroError<X> for Result<X, T>
where
    T: Into<PcodeMacroErrorSub>,
{
    fn to_pcode_macro(
        self,
        macro_pos: InputSource,
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
    InvalidRef(InputSource),
    #[error("Missing Ref {0}")]
    MissingRef(InputSource),

    #[error("Execution Error")]
    Execution(ExecutionError),
}
from_error!(PcodeMacroErrorSub, ExecutionError, Execution);

#[derive(Clone, Debug)]
pub struct Parameter {
    name: Rc<str>,
    //TODO
}
impl Parameter {
    pub fn new(name: Rc<str>) -> Self {
        Parameter { name }
    }
}

#[derive(Clone, Debug)]
pub enum ReadScope {
    Varnode(Rc<Varnode>),
    Assembly(Rc<Assembly>),
    Parameter(Rc<Parameter>),
}
#[derive(Clone, Debug)]
pub enum WriteScope {
    Varnode(Rc<Varnode>),
    Parameter(Rc<Parameter>),
}

#[derive(Debug, Clone)]
pub struct PcodeMacroInstance {
    //pub params: Vec<Rc<Parameter>>,
    pub execution: Execution,
    //me: Weak<Self>,
    pub parent: Weak<PcodeMacro>,
}

#[derive(Clone, Debug)]
pub struct PcodeMacro {
    pub name: Rc<str>,
    pub instances: RefCell<Vec<Rc<PcodeMacroInstance>>>,
    //TODO use Parameter and not Variable
    //TODO but first imple dyn Read/Write Scope for final Execution
    //pub params: RefCell<HashMap<Rc<str>, Rc<Variable>>>,
    //pub execution: RefCell<Execution>,
    //TODO
}

//use crate::processor;
impl PcodeMacro {
    pub fn new_empty(name: Rc<str>) -> Self {
        PcodeMacro {
            name,
            instances: RefCell::default(),
        }
    }
}
