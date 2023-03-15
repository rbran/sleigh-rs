use super::{Block, Pattern};
use crate::PatternLen;

#[derive(Clone, Debug)]
pub struct PatternConstraint {
    pub total_len: PatternLen,
    pub blocks: Vec<BlockConstraint>,
    pub total_variants_possible: usize,
}

#[derive(Clone, Debug)]
pub struct BlockConstraint {
    pub len: PatternLen,
    pub base: Vec<BitConstraint>,
    //number of possible variants for blocks prior to this one
    pub variants_possible_prior: usize,
    //0 or 2 or more, never 1
    pub variants: Option<Vec<Vec<BitConstraint>>>,
    //used on the creation, true if in use, so only modify on false
    pub variants_lock: bool,
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
    //this is an impossible value, requiring it to be 0/1 at the same time
    Impossible,
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
    pub conflicts: usize,
}

#[derive(Clone, Copy)]
pub struct BlockConstraintIter<'a> {
    extra_len: usize,
    base: &'a [BitConstraint],
    var: Option<&'a [BitConstraint]>,
}
impl<'a> Iterator for BlockConstraintIter<'a> {
    type Item = BitConstraint;
    fn next(&mut self) -> Option<Self::Item> {
        match (self.extra_len, self.base.split_first()) {
            //there are bits to produce, so some one
            (_, Some((first_base, rest_base))) => {
                self.base = rest_base;
                //return the value, and apply the variant, if any
                if let Some((first_var, rest_var)) =
                    self.var.map(|var| var.split_first().unwrap())
                {
                    self.var = Some(rest_var);
                    Some(first_base.most_restrictive(*first_var))
                } else {
                    Some(*first_base)
                }
            }
            //nothing else to produce
            (0, None) => return None,
            //no more bit to consume, but still extra bit to output
            (_x, None) => {
                self.extra_len -= 1;
                return Some(BitConstraint::Unrestrained);
            }
        }
    }
}

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
impl std::iter::FromIterator<SinglePatternOrdering>
    for MultiplePatternOrdering
{
    fn from_iter<T: IntoIterator<Item = SinglePatternOrdering>>(
        iter: T,
    ) -> Self {
        let mut acc = Self::default();
        for i in iter {
            acc.add(i);
        }
        acc
    }
}

impl MultiplePatternOrdering {
    pub fn add(&mut self, ord: SinglePatternOrdering) {
        match ord {
            SinglePatternOrdering::Eq => (),
            SinglePatternOrdering::Conflict => self.conflicts += 1,
            SinglePatternOrdering::Contains => self.contains += 1,
            SinglePatternOrdering::Contained => self.contained += 1,
        }
    }
}

impl PatternConstraint {
    pub fn new(pattern: &Pattern) -> Self {
        let mut variants_counter = 1;
        let mut blocks = Vec::with_capacity(pattern.blocks.len());
        for block in pattern.blocks.iter() {
            //parse until and including a block with multiple possible len.
            let is_undefined_len =
                block.len.unwrap().basic().unwrap().single_len().is_none();

            //create the block and update the variants_counter
            let block = BlockConstraint::new(block, variants_counter);
            if let Some(variants) = &block.variants {
                variants_counter *= variants.len();
            }
            blocks.push(block);

            //stop parsing if the len is not defined
            if is_undefined_len {
                break;
            }
        }
        Self {
            total_len: pattern.len.unwrap().basic().unwrap(),
            blocks,
            total_variants_possible: variants_counter,
        }
    }
    pub fn bits_len(&self) -> usize {
        self.blocks.iter().map(BlockConstraint::bits_produced).sum()
    }
    // Create a iterator that produces other iterator :-P
    // First (Outer) iterator produce all the possible variants of this pattern.
    // Second (Inner) iterator produce the bits for this variant of the pattern.
    pub fn bits<'a>(
        &'a self,
        len: usize,
    ) -> impl Iterator<Item = impl Iterator<Item = BitConstraint> + Clone + 'a>
           + Clone
           + 'a {
        let extra_len = len.saturating_sub(self.bits_len());
        // a little helper to make this move easy
        #[derive(Clone, Copy)]
        struct ExtraBits(usize);
        impl<'a> Iterator for ExtraBits {
            type Item = BitConstraint;
            fn next(&mut self) -> Option<Self::Item> {
                (self.0 > 0).then(|| {
                    self.0 -= 1;
                    BitConstraint::Unrestrained
                })
            }
        }
        let extra_len_chain = ExtraBits(extra_len);
        (0..self.total_variants_possible)
            .into_iter()
            .map(move |variant_id| {
                self.blocks
                    .iter()
                    .filter_map(move |block| {
                        let bits = block.bits(variant_id);
                        bits.clone()
                            .all(|bit| !bit.is_impossible())
                            .then_some(bits)
                    })
                    .flatten()
                    .chain(extra_len_chain)
            })
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
            if self_variant.clone().any(|bit| bit.is_impossible()) {
                continue;
            }
            for other_variant in other.bits(len_bits) {
                if other_variant.clone().any(|bit| bit.is_impossible()) {
                    continue;
                }
                let cmp: SinglePatternOrdering = self_variant
                    .clone()
                    .zip(other_variant)
                    .map(|(self_bit, other_bit)| match (self_bit, other_bit) {
                        (Impossible, _) | (_, Impossible) => unreachable!(),
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
    pub fn new(block: &Block, variants_prior: usize) -> Self {
        let base_len = block.root_len();
        let mut new = Self {
            len: block.len.unwrap().basic().unwrap(),
            variants_possible_prior: variants_prior,
            base: vec![BitConstraint::default(); base_len],
            variants: None,
            variants_lock: false,
        };
        block.constraint(&mut new, 0);
        new
    }
    pub fn root_len(&self) -> usize {
        self.base.len()
    }
    pub fn bits_produced(&self) -> usize {
        self.len.max().unwrap_or(self.len.min()).try_into().unwrap()
    }
    pub fn bits<'a>(&'a self, variant_id: usize) -> BlockConstraintIter<'a> {
        let extra_len = self.bits_produced().saturating_sub(self.root_len());
        let var = self.variants.as_ref().map(|variants| {
            let var_id =
                (variant_id % self.variants_possible_prior) % variants.len();
            variants[var_id].as_slice()
        });
        BlockConstraintIter {
            extra_len,
            base: &self.base,
            var,
        }
    }
}

impl BitConstraint {
    fn is_impossible(&self) -> bool {
        matches!(self, Self::Impossible)
    }
    pub fn define(self, bit: bool) -> Self {
        match self {
            Self::Impossible => Self::Impossible,
            Self::Unrestrained => Self::Defined(bit),
            Self::Defined(old_bit) if old_bit == bit => self,
            Self::Defined(_old_bit) => Self::Impossible,
            // TODO this may not be possible, we are unable to verify that now
            Self::Restrained => Self::Defined(bit),
        }
    }
    /// select the most restrictive from both, None if they conflict
    pub fn most_restrictive(self, other: Self) -> Self {
        match (self, other) {
            (Self::Impossible, _) | (_, Self::Impossible) => Self::Impossible,
            //if one is unrestrained, just return the other
            (Self::Unrestrained, other) | (other, Self::Unrestrained) => other,
            // both have the same value
            (Self::Restrained, Self::Restrained) => self,
            (Self::Defined(self_value), Self::Defined(other_value))
                if self_value == other_value =>
            {
                self
            }
            // conflicting values
            (Self::Defined(_), Self::Defined(_)) => Self::Impossible,
            // TODO this may not be possible, we are unable to verify that now
            (other @ Self::Defined(_), Self::Restrained)
            | (Self::Restrained, other @ Self::Defined(_)) => other,
        }
    }
    /// select the least restrictive from both
    pub fn least_restrictive(self, other: Self) -> Self {
        match (self, other) {
            (Self::Impossible, other) | (other, Self::Impossible) => other,
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
