use std::rc::Rc;

use super::inner::PrintFlags;
use super::{GlobalReference, PrintBase, PrintFmt};
use crate::{IntTypeS, NonZeroTypeU, Varnode};

#[derive(Clone, Debug)]
pub enum Meaning {
    ///The value is a Literal, signed if print_fmt.signed is set.
    ///In Display, print the value with this Format.
    ///In Disassembly, expand this value into IntTypeS.
    ///In Execution, the value will automatically expanded to the required len.
    Literal(PrintFmt),
    ///The value is translated with a varnode using this index.
    ///In Display, print the varnode name.
    ///In Disassembly, TODO: is unknown.
    ///In Execution, read/write are done on the underlying varnode.
    Variable(Rc<[(usize, GlobalReference<Varnode>)]>),
    ///The Value is translated into this string.
    ///In Display, print the string.
    ///In Disassembly, TODO: is unknown.
    ///In Execution, threat like a literal value.
    Name(Rc<[(usize, String)]>),
    ///The Value is translated into a signed value with this Format.
    ///In Display, print the translated value using this index.
    ///In Disassembly, use the translated value.
    ///In Execution, the value is translanted and is automatically expanded to
    ///the required len.
    Value(PrintBase, Rc<[(usize, IntTypeS)]>),
}

impl Meaning {
    pub(crate) fn new_variable(
        print_flags: &PrintFlags,
        variables: &Rc<[(usize, GlobalReference<Varnode>)]>,
    ) -> Option<Self> {
        if print_flags.is_set() {
            //can't set variable if a print flag is also set
            return None;
        }
        Some(Meaning::Variable(Rc::clone(variables)))
    }
    pub(crate) fn new_name(
        print_flags: &PrintFlags,
        names: &Rc<[(usize, String)]>,
    ) -> Option<Self> {
        if print_flags.is_set() {
            //can't set variable if a print flag is also set
            return None;
        }
        Some(Meaning::Name(Rc::clone(names)))
    }
    pub(crate) fn new_value(
        print_flags: &PrintFlags,
        values: &Rc<[(usize, IntTypeS)]>,
    ) -> Option<Self> {
        //value is printed in dec by default
        let base = print_flags.base.unwrap_or(PrintBase::Dec);
        Some(Meaning::Value(base, Rc::clone(values)))
    }
    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal(_))
    }
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable { .. })
    }
    pub fn is_signed(&self) -> bool {
        match self {
            Meaning::Literal(print_fmt) => print_fmt.signed(),
            Meaning::Value(_, _) => true,
            Meaning::Variable(_) | Meaning::Name(_) => false,
        }
    }
    ///The len in bytes this value will be read/write in execution context.
    ///Only a value that is translated into varnode have a len known.
    pub fn exec_len_bytes(&self) -> Option<NonZeroTypeU> {
        match self {
            Self::Variable(vars) => {
                Some(vars.first().unwrap().1.element().len_bytes)
            }
            Self::Literal(_) | Self::Name(_) | Self::Value(_, _) => None,
        }
    }
}
