use crate::PatternLen;

use super::{Block, Pattern};

#[derive(Clone, Debug)]
pub struct PatternConstraint {
    pub total_len: PatternLen,
    pub blocks: Vec<BlockConstraint>,
}

#[derive(Clone, Debug)]
pub struct BlockConstraint {
    pub len: PatternLen,
    pub base: Vec<BitConstraint>,
    //0 or 2 or more, never 1
    pub variants: Vec<Vec<BitConstraint>>,
}

/// Represent how a bit is limited in a pattern
#[derive(Clone, Copy, Debug, Default)]
pub enum BitConstraint {
    //can have any value
    #[default]
    Unrestrained,
    //only one value possible 0->false, 1->true
    Defined(bool),
    //the value is limited depending on other bits.
    Restrained,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SinglePatternOrdering {
    Eq,
    Conflict,
    Contains,
    Contained,
}
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct MultiplePatternOrdering {
    pub contained: usize,
    pub contains: usize,
    pub conflict: usize,
}

pub trait PatternBits<'a, I>: Clone + Iterator<Item = I> + Clone + 'a
where
    I: Iterator<Item = BitConstraint> + Clone + 'a,
{
}
#[derive(Clone, Copy)]
pub enum BlockConstraintIter<'a> {
    Single(usize, &'a [BitConstraint]),
    //Empty vars means end
    Multiple {
        len: usize,
        base: &'a [BitConstraint],
        vars: &'a [Vec<BitConstraint>],
    },
}
#[derive(Clone, Copy)]
pub struct BitConstraintIter<'a> {
    len: usize,
    base: &'a [BitConstraint],
    variant: Option<&'a [BitConstraint]>,
}
impl<'a> Iterator for BlockConstraintIter<'a> {
    type Item = BitConstraintIter<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            BlockConstraintIter::Multiple { vars: [], .. } => None,
            BlockConstraintIter::Single(len, base) => {
                let iter = BitConstraintIter {
                    len: *len,
                    base,
                    variant: None,
                };
                *self = Self::Multiple {
                    len: *len,
                    base,
                    vars: &[],
                };
                Some(iter)
            }
            BlockConstraintIter::Multiple {
                len,
                base,
                vars: [first, rest @ ..],
            } => {
                let iter = BitConstraintIter {
                    len: *len,
                    base,
                    variant: Some(first),
                };
                *self = Self::Multiple {
                    len: *len,
                    base,
                    vars: rest,
                };
                Some(iter)
            }
        }
    }
}
impl<'a> Iterator for BitConstraintIter<'a> {
    type Item = BitConstraint;
    fn next(&mut self) -> Option<Self::Item> {
        let Some((first_base, rest_base)) = self.base.split_first() else {
                    //concat the Unrestructed to fill the len.
                    match self.len {
                        0 => return None,
                        _x => {
                            self.len -= 1;
                            return Some(BitConstraint::Unrestrained)
                        },
                    }
                };
        if let Some((first_var, rest_var)) =
            self.variant.map(|var| var.split_first().unwrap())
        {
            *self = Self {
                len: self.len,
                base: rest_base,
                variant: Some(rest_var),
            };
            Some(first_base.most_restrictive(*first_var).unwrap())
        } else {
            *self = Self {
                len: self.len,
                base: rest_base,
                variant: None,
            };
            Some(*first_base)
        }
    }
}
impl<'a> PatternBits<'a, BitConstraintIter<'a>> for BlockConstraintIter<'a> {}

impl SinglePatternOrdering {
    pub fn is_eq(self) -> bool {
        matches!(self, Self::Eq)
    }
    pub fn is_contains(self) -> bool {
        matches!(self, Self::Contains)
    }
    pub fn is_contained(self) -> bool {
        matches!(self, Self::Contained)
    }
    pub fn is_conflict(self) -> bool {
        matches!(self, Self::Conflict)
    }
    pub fn combine(self, other: Self) -> Self {
        use SinglePatternOrdering::*;
        match (self, other) {
            (Conflict, _) | (_, Conflict) => Conflict,
            (Eq, other) | (other, Eq) => other,
            (Contains, Contains) | (Contained, Contained) => self,
            (Contains, Contained) | (Contained, Contains) => Conflict,
        }
    }
}
impl std::iter::FromIterator<SinglePatternOrdering> for SinglePatternOrdering {
    fn from_iter<T: IntoIterator<Item = SinglePatternOrdering>>(
        iter: T,
    ) -> Self {
        use SinglePatternOrdering::*;
        let mut acc = Eq;
        for i in iter {
            acc = acc.combine(i);
            if acc == Conflict {
                return Conflict;
            }
        }
        acc
    }
}

impl MultiplePatternOrdering {
    pub fn add(&mut self, ord: SinglePatternOrdering) {
        match ord {
            SinglePatternOrdering::Eq => (),
            SinglePatternOrdering::Conflict => self.conflict += 1,
            SinglePatternOrdering::Contains => self.contains += 1,
            SinglePatternOrdering::Contained => self.contained += 1,
        }
    }
}

impl PatternConstraint {
    pub fn new(pattern: &Pattern) -> Self {
        let blocks = pattern
            .blocks
            .iter()
            .scan(false, |found_undefined_len, block| {
                if *found_undefined_len {
                    return None;
                }
                //parse until and including a block with multiple possible len.
                if block.len.unwrap().basic().unwrap().single_len().is_none() {
                    //let this pass, next one will be blocked
                    *found_undefined_len = true;
                }
                Some(block)
            })
            .map(BlockConstraint::new)
            .collect();
        Self {
            total_len: pattern.len.unwrap().basic().unwrap(),
            blocks,
        }
    }
    pub fn bits_len(&self) -> usize {
        self.blocks.iter().map(BlockConstraint::bits_len).sum()
    }
    //TODO ugly return type, do better
    pub fn bits<'a>(
        &'a self,
        len: usize,
    ) -> std::iter::Chain<
        std::iter::Flatten<
            std::iter::Map<
                std::slice::Iter<'a, BlockConstraint>,
                impl FnMut(&'a BlockConstraint) -> BlockConstraintIter<'a>,
            >,
        >,
        BlockConstraintIter<'a>,
    > {
        let len = self.bits_len().max(len);
        self.blocks
            .iter()
            .map(move |block| block.bits())
            .flatten()
            .chain(BlockConstraintIter::Single(len, &[]))
    }
    //7.8.1. Matching
    //one pattern contains the other if all the cases that match the contained,
    //also match the pattern.
    //eg: `a` contains `b` if all cases that match `b` also match `a`. In other
    //words `a` is a special case of `b`.
    //NOTE the opose don't need to be true.
    pub fn ordering(&self, other: &Self) -> MultiplePatternOrdering {
        let len_bits = self.bits_len().max(other.bits_len());

        use BitConstraint::*;
        use SinglePatternOrdering::*;
        let mut variant_ordering = MultiplePatternOrdering::default();
        for self_variant in self.bits(len_bits) {
            for other_variant in other.bits(len_bits) {
                let cmp: SinglePatternOrdering = self_variant
                    .clone()
                    .zip(other_variant)
                    .map(|(self_bit, other_bit)| match (self_bit, other_bit) {
                        (Defined(_) | Restrained, Defined(_) | Restrained)
                        | (Unrestrained, Unrestrained) => Eq,
                        (Unrestrained, _) => Contained,
                        (_, Unrestrained) => Contains,
                    })
                    .collect();
                variant_ordering.add(cmp);
            }
        }
        variant_ordering
    }
}

impl BlockConstraint {
    pub fn new(block: &Block) -> Self {
        let base_len = block.root_len();
        let mut base = vec![BitConstraint::default(); base_len];
        block.constraint_bits_base(&mut base);
        Self {
            len: block.len.unwrap().basic().unwrap(),
            base,
            variants: block.constraint_bits_variants(),
        }
    }
    pub fn root_len(&self) -> usize {
        self.base.len()
    }
    pub fn bits_len(&self) -> usize {
        self.len.max().unwrap_or(self.len.min()).try_into().unwrap()
    }
    pub fn bits<'a>(&'a self) -> BlockConstraintIter<'a> {
        let len = self.bits_len();
        if self.variants.is_empty() {
            BlockConstraintIter::Single(len, &self.base)
        } else {
            BlockConstraintIter::Multiple {
                len,
                base: &self.base,
                vars: &self.variants,
            }
        }
    }
}

impl BitConstraint {
    pub fn define(self, bit: bool) -> Option<Self> {
        match self {
            Self::Unrestrained => Some(Self::Defined(bit)),
            Self::Defined(old_bit) if old_bit == bit => Some(self),
            Self::Defined(_old_bit) => None,
            // TODO this may not be possible, we are unable to verify that now
            Self::Restrained => Some(Self::Defined(bit)),
        }
    }
    /// select the most restrictive from both, None if they conflict
    pub fn most_restrictive(self, other: Self) -> Option<Self> {
        match (self, other) {
            //if one is unrestrained, just return the other
            (Self::Unrestrained, other) | (other, Self::Unrestrained) => {
                Some(other)
            }
            // both have the same value
            (Self::Restrained, Self::Restrained) => Some(self),
            (Self::Defined(self_value), Self::Defined(other_value))
                if self_value == other_value =>
            {
                Some(self)
            }
            // conflicting values
            (Self::Defined(_), Self::Defined(_)) => None,
            // TODO this may not be possible, we are unable to verify that now
            (other @ Self::Defined(_), Self::Restrained)
            | (Self::Restrained, other @ Self::Defined(_)) => Some(other),
        }
    }
    /// select the least restrictive from both
    pub fn least_restrictive(self, other: Self) -> Self {
        match (self, other) {
            (Self::Unrestrained, _other) | (_other, Self::Unrestrained) => {
                Self::Unrestrained
            }
            (Self::Defined(self_value), Self::Defined(other_value))
                if self_value != other_value =>
            {
                Self::Unrestrained
            }
            //both have the same value
            (Self::Defined(_), Self::Defined(_)) => self,
            (Self::Restrained, Self::Restrained)
            | (Self::Defined(_), Self::Restrained)
            | (Self::Restrained, Self::Defined(_)) => Self::Restrained,
        }
    }
}
