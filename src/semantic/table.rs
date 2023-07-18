use crate::pattern::BitConstraint;
use crate::semantic::display::Display;
use crate::semantic::execution::Execution;
use crate::semantic::pattern::{Pattern, PatternLen};
use crate::{NumberNonZeroUnsigned, Span};

#[derive(Clone, Copy, Debug, Default)]
pub enum ExecutionExport {
    //don't return
    #[default]
    None,
    //value that is known at Dissassembly time
    Const(NumberNonZeroUnsigned),
    //value that can be know at execution time
    Value(NumberNonZeroUnsigned),
    //References/registers and other mem locations, all with the same size
    Reference(NumberNonZeroUnsigned),
    //multiple source, can by any kind of return, value or address,
    //but all with the same size
    Multiple(NumberNonZeroUnsigned),
}

impl ExecutionExport {
    pub fn len(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::None => None,
            Self::Const(len)
            | Self::Value(len)
            | Self::Reference(len)
            | Self::Multiple(len) => Some(*len),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ConstructorId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VariantId(pub usize);

#[derive(Clone, Debug)]
pub struct Constructor {
    pub pattern: Pattern,
    pub display: Display,
    pub execution: Option<Execution>,
    pub location: Span,
    // the bit pattern for all possible variants in the pattern,
    // impossible variants are simply not present here
    pub(crate) variants_bits:
        Box<[(VariantId, Box<[BitConstraint]>, Box<[BitConstraint]>)]>,
}

impl Constructor {
    pub fn variants<'a>(
        &'a self,
    ) -> impl Iterator<
        Item = (VariantId, &'a [BitConstraint], &'a [BitConstraint]),
    > + 'a {
        self.variants_bits.iter().map(|(id, context, token)| {
            (*id, context.as_ref(), token.as_ref())
        })
    }
    pub fn variant(
        &self,
        id: VariantId,
    ) -> (&[BitConstraint], &[BitConstraint]) {
        self.variants_bits
            .iter()
            .find(|(current, _, _)| *current == id)
            .map(|(_, c, t)| (c.as_ref(), t.as_ref()))
            .unwrap()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Matcher {
    pub constructor: ConstructorId,
    pub variant_id: VariantId,
}

#[derive(Clone, Debug)]
pub struct Table {
    pub(crate) name: Box<str>,
    pub(crate) is_root: bool,
    pub(crate) constructors: Box<[Constructor]>,
    pub matcher_order: Box<[Matcher]>,
    pub export: ExecutionExport,
    pub pattern_len: PatternLen,
}

impl Table {
    pub fn is_root(&self) -> bool {
        self.is_root
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }
    pub fn constructor(&self, id: ConstructorId) -> &Constructor {
        &self.constructors[id.0]
    }
}
