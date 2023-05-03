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

impl SinglePatternOrdering {
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

impl BitConstraint {
    pub fn is_impossible(&self) -> bool {
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
