use crate::Span;

#[derive(Clone, Debug)]
pub struct UserFunction {
    name: Box<str>,
    location: Span,
}

impl UserFunction {
    pub(crate) fn new(name: Box<str>, location: Span) -> Self {
        Self { name, location }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn location(&self) -> &Span {
        &self.location
    }
}
