use crate::PatternLen;

use super::Pattern;

#[derive(Clone, Debug)]
pub struct PatternConstraint {
    pub total_len: PatternLen,
    pub bits: Vec<BitConstraint>,
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
pub enum PatternConstraintOrdering {
    Eq,
    Conflict,
    Contains,
    Contained,
}

impl PatternConstraintOrdering {
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
}

impl PatternConstraint {
    pub fn new(pattern: &Pattern) -> Self {
        let mut bits_len = 0usize;
        for block in pattern.blocks.iter() {
            let block_len = block.len.unwrap().basic().unwrap();
            let block_root_len = usize::try_from(block_len.min()).unwrap() * 8;
            bits_len += block_root_len;
            //this block is not exacly sized, so ignore all blocks after that
            if block_len.single_len().is_none() {
                break;
            }
        }
        //not constraint the bits
        let mut bits = vec![BitConstraint::default(); bits_len];
        let mut current = &mut bits[..];
        for block in pattern.blocks.iter() {
            let block_len = block.len.unwrap().basic().unwrap();
            let block_root_len = usize::try_from(block_len.min()).unwrap() * 8;
            block.constraint_bits(&mut current[..block_root_len]);
            //next block
            current = &mut current[block_root_len..];
            //this block is the last one, so just stop
            if current.is_empty() {
                break;
            }
        }
        Self {
            total_len: pattern.len.unwrap().basic().unwrap(),
            bits,
        }
    }
    pub fn bits<'a>(
        &'a self,
        len_bits: usize,
    ) -> impl Iterator<Item = BitConstraint> + 'a {
        let len_extra = len_bits.saturating_sub(self.bits.len());
        self.bits.iter().copied().chain(
            (0..len_extra)
                .into_iter()
                .map(|_| BitConstraint::Unrestrained),
        )
    }
    //7.8.1. Matching
    //one pattern contains the other if all the cases that match the contained,
    //also match the pattern.
    //eg: `a` contains `b` if all cases that match `b` also match `a`. In other
    //words `a` is a special case of `b`.
    //NOTE the opose don't need to be true.
    pub fn partial_cmp(
        &self,
        other: &Self,
    ) -> Option<PatternConstraintOrdering> {
        ////TODO: comparable if have the same len?
        //(self.total_len != other.total_len).then_some(())?;

        let len_bits = self.bits.len().max(other.bits.len());

        use BitConstraint::*;
        use PatternConstraintOrdering::*;
        let bits_cmp = self.bits(len_bits).zip(other.bits(len_bits)).map(
            |(self_bit, other_bit)| match (self_bit, other_bit) {
                (Defined(_) | Restrained, Defined(_) | Restrained)
                | (Unrestrained, Unrestrained) => Eq,
                (Unrestrained, _) => Contained,
                (_, Unrestrained) => Contains,
            },
        );

        let mut acc_cmp = Eq;
        for bit_cmp in bits_cmp {
            match (bit_cmp, acc_cmp) {
                (Conflict, _) | (_, Conflict) => unreachable!(),
                //found a diference
                (x, Eq) => acc_cmp = x,
                //doesn't change the state
                (Eq, _) | (Contains, Contains) | (Contained, Contained) => (),
                //can't have a contain/contains, that is a conflict
                (Contains, Contained) | (Contained, Contains) => {
                    return Some(Conflict)
                }
            }
        }
        Some(acc_cmp)
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
