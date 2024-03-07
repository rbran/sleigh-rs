use std::cell::Cell;

use crate::semantic::execution::Variable as FinalVariable;
use crate::Span;

use super::FieldSize;

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    //pub scope: RefCell<VariableScope>,
    pub size: Cell<FieldSize>,
    ///location of the variable declaration, if declared explicitly
    /// NOTE: PcodeMacro Params src is located in `Parameter`
    pub src: Option<Span>,
}

impl Variable {
    pub fn new(name: String, src: Option<Span>) -> Self {
        Self {
            name,
            //scope,
            size: Cell::new(FieldSize::new_unsized()),
            src,
        }
    }
    pub fn convert(self) -> FinalVariable {
        FinalVariable {
            name: self.name.into(),
            len_bits: self.size.get().possible_value().unwrap(),
            location: self.src,
        }
    }
}
