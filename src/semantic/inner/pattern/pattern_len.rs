use crate::semantic::pattern::PatternLen;
use crate::NumberUnsigned;

//Describe a Block/Pattern possible len
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstructorPatternLen {
    /// Cases with the pattern call it own table, NOTE: the table can only call
    /// itself once.
    /// self-Recursive, non-growing, basically unrestricted, but indicating
    /// to tables that this don't change the table.pattern_len directly.
    /// Value is the len that this constructor will generate, not including
    /// the recursive itself
    NonGrowingRecursive(PatternLen),
    /// self-Recrusive, growing, similat to NonGrowing, but is possible that this
    /// keep calling itself, in a infinite growing patter. It is the context job
    /// to limit the size of it.
    /// grow, is the len of the size that will be added to the len, and non_grow,
    /// is the value that was taken from NonGrowing Recursive
    GrowingRecursive {
        grow: PatternLen,
        non_grow: PatternLen,
    },

    /// the final state, this means the len have a possible final value,
    /// although it could be restricted further, eg: len is 2-8bytes, but is
    /// found that the len is never bigger then 6, so len is update to 2-6bytes.
    Basic(PatternLen),
}
impl ConstructorPatternLen {
    pub fn single_len(&self) -> Option<NumberUnsigned> {
        match self {
            Self::Basic(basic) => basic.single_len(),
            Self::NonGrowingRecursive(_) | Self::GrowingRecursive { .. } => {
                None
            }
        }
    }
    pub fn is_basic(&self) -> bool {
        matches!(self, Self::Basic(_))
    }
    pub fn basic(&self) -> Option<PatternLen> {
        match self {
            Self::Basic(basic) => Some(*basic),
            Self::NonGrowingRecursive(_) | Self::GrowingRecursive { .. } => {
                None
            }
        }
    }
    ///if is some kind of recursive
    pub fn is_recursive(&self) -> bool {
        match self {
            Self::Basic(basic) => basic.is_recursive(),
            Self::NonGrowingRecursive(_) | Self::GrowingRecursive { .. } => {
                true
            }
        }
    }
    ///the min possible pattern len size, None means the min can't be calculated
    ///because this is a recursive and the len depends on the other constructors
    pub fn min(&self) -> Option<NumberUnsigned> {
        self.basic().map(|len| len.min())
    }
    ///the max possible pattern len size, None is infinite maximum possible len
    pub fn max(&self) -> Option<NumberUnsigned> {
        self.basic().and_then(|len| len.max())
    }
    //TODO replace Option with Result?
    pub fn add(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Self::Basic(x), Self::Basic(y)) => Some(Self::Basic(x.concat(y))),
            //NonGrowingRecursize concat with a basic block, result in a
            //GrowingRecursive
            (Self::NonGrowingRecursive(non_grow), Self::Basic(basic))
            | (Self::Basic(basic), Self::NonGrowingRecursive(non_grow)) => {
                Some(Self::GrowingRecursive {
                    grow: basic,
                    non_grow,
                })
            }
            //Growing Recursive concat with a basic, just grows
            (Self::GrowingRecursive { grow, non_grow }, Self::Basic(basic))
            | (Self::Basic(basic), Self::GrowingRecursive { grow, non_grow }) => {
                Some(Self::GrowingRecursive {
                    grow: grow.concat(basic),
                    non_grow,
                })
            }
            //a pattern can only have one SelfRecursive, so this is invalid
            (
                Self::GrowingRecursive { .. } | Self::NonGrowingRecursive(_),
                Self::GrowingRecursive { .. } | Self::NonGrowingRecursive(_),
            ) => None,
        }
    }
    pub fn greater(self, other: Self) -> Option<Self> {
        match (self, other) {
            (
                Self::GrowingRecursive { .. } | Self::NonGrowingRecursive(_),
                Self::GrowingRecursive { .. } | Self::NonGrowingRecursive(_),
            ) => None,
            (Self::Basic(x), Self::Basic(y)) => Some(Self::Basic(x.greater(y))),
            (
                Self::Basic(x) | Self::NonGrowingRecursive(x),
                Self::Basic(y) | Self::NonGrowingRecursive(y),
            ) => Some(Self::NonGrowingRecursive(x.greater(y))),
            (Self::Basic(_), Self::GrowingRecursive { .. })
            | (Self::GrowingRecursive { .. }, Self::Basic(_)) => {
                //This only happen if recursive block is in a sub_pattern
                //what i think is not allowed
                unimplemented!()
            }
        }
    }
}
impl From<PatternLen> for ConstructorPatternLen {
    fn from(value: PatternLen) -> Self {
        Self::Basic(value)
    }
}
