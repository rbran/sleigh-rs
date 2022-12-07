use thiserror::Error;

use crate::InputSource;

use super::GlobalElement;

#[derive(Clone, Debug, Error)]
pub enum UserFunctionError {
    #[error("Invalid Ref {0}")]
    InvalidRef(InputSource),
}

#[derive(Clone, Debug)]
pub struct UserFunction(InputSource);

impl GlobalElement<UserFunction> {
    pub fn new_user_function(name: &str, src: InputSource) -> Self {
        Self::new_from(name, UserFunction(src))
    }
}

impl UserFunction {
    pub fn location(&self) -> &InputSource {
        &self.0
    }
}
