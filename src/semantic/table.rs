use crate::pattern::BitConstraint;
use crate::semantic::display::Display;
use crate::semantic::execution::Execution;
use crate::semantic::pattern::{Pattern, PatternLen};
use crate::{NumberNonZeroUnsigned, Span};

use super::inner::table::ConstructorOrdering;

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

#[derive(Clone, Debug)]
pub struct Constructor {
    pub pattern: Pattern,
    pub display: Display,
    pub execution: Option<Execution>,
    pub location: Span,
    // the bit pattern for all possible variants in the pattern,
    // impossible variants are simply not present here
    pub(crate) variants_bits:
        Box<[(usize, (Box<[BitConstraint]>, Box<[BitConstraint]>))]>,
    // the bit pattern for the union of all patterns
    pub(crate) context_bits: Box<[BitConstraint]>,
    pub(crate) pattern_bits: Box<[BitConstraint]>,
}

impl Constructor {
    /// the union off all variants into a single generic bit pattern,
    /// NOTE: context have no configurable endianes, the bit order is always
    /// from msb (index 0) to lsb (index len - 1)
    pub fn context_bits(&self) -> &[BitConstraint] {
        &self.context_bits
    }

    /// all variants of this pattern, NOTE: context have not endianes, is always
    /// from the msb to the lsb
    pub fn context_variants(
        &self,
    ) -> impl Iterator<Item = (usize, &[BitConstraint])> {
        self.variants_bits
            .iter()
            .map(|(i, (c, _v))| (*i, c.as_ref()))
    }

    pub fn pattern_bits(&self) -> &[BitConstraint] {
        &self.pattern_bits
    }

    pub fn pattern_variants(
        &self,
    ) -> impl Iterator<Item = (usize, &[BitConstraint])> {
        self.variants_bits
            .iter()
            .map(|(i, (_c, v))| (*i, v.as_ref()))
    }

    ////7.8.1. Matching
    ////one pattern contains the other if all the cases that match the contained,
    ////also match the pattern.
    ////eg: `a` contains `b` if all cases that match `b` also match `a`. In other
    ////words `a` is a special case of `b`.
    ////NOTE the opose don't need to be true.
    pub(crate) fn ordering(&self, other: &Self) -> ConstructorOrdering {
        let self_pattern_len = self.pattern.bits_produced();
        let other_pattern_len = other.pattern.bits_produced();
        let max_pattern_len = self_pattern_len.max(other_pattern_len);
        let self_extend = max_pattern_len - self_pattern_len;
        let other_extend = max_pattern_len - other_pattern_len;

        fn produce_iter<'a>(
            constructor: &'a Constructor,
            extend: usize,
        ) -> impl Iterator<Item = BitConstraint> + 'a {
            constructor
                .context_bits()
                .iter()
                .chain(constructor.pattern_bits().iter())
                .cloned()
                .chain(
                    (0..extend)
                        .into_iter()
                        .map(|_| BitConstraint::Unrestrained),
                )
        }
        let self_bits = produce_iter(self, self_extend);
        let other_bits = produce_iter(other, other_extend);

        self_bits.zip(other_bits).collect()
    }
}

#[derive(Clone, Debug)]
pub struct Table {
    pub(crate) name: Box<str>,
    pub(crate) is_root: bool,
    pub(crate) constructors: Box<[Constructor]>,
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
