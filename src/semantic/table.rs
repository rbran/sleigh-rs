use super::execution::ExportLen;
use crate::pattern::BitConstraint;
use crate::semantic::display::Display;
use crate::semantic::execution::Execution;
use crate::semantic::pattern::{Pattern, PatternLen};
use crate::{NumberNonZeroUnsigned, Span};

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
    // TODO VariantId is the position on the vector, just use the index
    #[allow(clippy::type_complexity)]
    pub(crate) variants_bits:
        Box<[(VariantId, Box<[BitConstraint]>, Box<[BitConstraint]>)]>,
}

impl Constructor {
    pub fn variants(
        &self,
    ) -> impl Iterator<Item = (VariantId, &'_ [BitConstraint], &'_ [BitConstraint])>
    {
        self.variants_bits
            .iter()
            .map(|(id, context, token)| (*id, context.as_ref(), token.as_ref()))
    }
    /// return the variant constraint for the context and pattern
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
    pub export: Option<ExportLen>,
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
    pub fn matcher_order(&self) -> &[Matcher] {
        &self.matcher_order
    }
}
