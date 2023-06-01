use crate::Span;

#[derive(Clone, Debug)]
pub struct UserFunction(pub(crate) Span);

impl UserFunction {
    pub fn location(&self) -> &Span {
        &self.0
    }
}
