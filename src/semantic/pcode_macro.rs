use crate::semantic::{PcodeMacroId, Span};

use super::execution::{Execution, VariableId};

#[derive(Clone, Debug)]
pub struct Parameter {
    pub variable_id: VariableId,
    pub location: Span,
    //TODO
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PcodeMacroCallId {
    pub(crate) macro_id: PcodeMacroId,
    pub(crate) instance_id: PcodeMacroInstanceId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PcodeMacroInstanceId(pub(crate) usize);

#[derive(Debug, Clone)]
pub struct PcodeMacroInstance {
    pub parameters: Box<[Parameter]>,
    pub execution: Execution,
}

#[derive(Clone, Debug)]
pub struct PcodeMacro {
    pub location: Span,
    pub instances: Box<[PcodeMacroInstance]>,
    //TODO
}
