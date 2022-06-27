use std::rc::Rc;

use thiserror::Error;

use crate::InputSource;

#[derive(Clone, Debug, Error)]
pub enum UserFunctionError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
}

#[derive(Clone, Debug)]
pub struct UserFunction {
    pub name: Rc<str>,
}
