use thiserror::Error;

use crate::Span;

use super::GlobalElement;

#[derive(Clone, Debug, Error)]
pub enum UserFunctionError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
}

#[derive(Clone, Debug)]
pub struct UserFunction(Span);

impl GlobalElement<UserFunction> {
    pub fn new_user_function(name: &str, src: Span) -> Self {
        Self::new_from(name, UserFunction(src))
    }
}

impl UserFunction {
    pub fn location(&self) -> &Span {
        &self.0
    }
}
