use std::cell::Cell;

use crate::semantic::execution::Variable as FinalVariable;
use crate::Span;

use super::FieldSize;

#[derive(Clone, Debug)]
pub struct Variable {
    // note variable in macros get the name of their alias
    pub name: String,
    //pub scope: RefCell<VariableScope>,
    pub size: Cell<FieldSize>,
    ///location of the variable declaration, if declared explicitly
    /// NOTE: PcodeMacro Params src is located in `Parameter`
    pub src: Span,
    pub explicit: bool,
}

impl Variable {
    pub fn new(
        name: String,
        src: Span,
        size: Option<FieldSize>,
        explicit: bool,
    ) -> Self {
        Self {
            name,
            //scope,
            size: Cell::new(size.unwrap_or(FieldSize::new_unsized())),
            src,
            explicit,
        }
    }
    pub fn convert(self) -> FinalVariable {
        FinalVariable {
            name: self.name.into(),
            len_bits: self.size.get().possible_value().unwrap(),
            location: self.explicit.then_some(self.src),
        }
    }
}
