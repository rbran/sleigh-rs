use std::cell::Cell;
use std::ops::{Bound, RangeBounds};

use crate::{NumberNonZeroUnsigned, NumberUnsigned};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FieldRange {
    min: NumberNonZeroUnsigned,
    max: NumberNonZeroUnsigned,
}
impl FieldRange {
    fn new(
        min: NumberNonZeroUnsigned,
        max: Option<NumberNonZeroUnsigned>,
    ) -> Self {
        Self {
            min,
            max: max.unwrap_or(
                NumberNonZeroUnsigned::new(NumberUnsigned::MAX).unwrap(),
            ),
        }
    }
    fn new_unsize() -> Self {
        Self::new(1.try_into().unwrap(), None)
    }
    fn set_min(self, new_min: NumberNonZeroUnsigned) -> Option<Self> {
        if self.max < new_min {
            //max is less then the new_min, unable to set this value
            return None;
        }
        let min = self.min.max(new_min);
        Some(Self { min, max: self.max })
    }
    fn set_max(self, new_max: NumberNonZeroUnsigned) -> Option<Self> {
        if self.min > new_max {
            //unable to set this max value
            return None;
        }
        let max = self.max.min(new_max);
        Some(Self { min: self.min, max })
    }
    fn single_value(&self) -> Option<NumberNonZeroUnsigned> {
        (self.min == self.max).then_some(self.min)
    }
}
impl RangeBounds<NumberNonZeroUnsigned> for FieldRange {
    fn start_bound(&self) -> Bound<&NumberNonZeroUnsigned> {
        Bound::Included(&self.min)
    }

    fn end_bound(&self) -> Bound<&NumberNonZeroUnsigned> {
        Bound::Included(&self.max)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FieldAuto {
    None,
    Min,
    Value(NumberNonZeroUnsigned),
}
impl FieldAuto {
    fn in_range(self, range: &FieldRange) -> Self {
        match &self {
            Self::None | Self::Min => self,
            Self::Value(value) if range.contains(value) => self,
            Self::Value(_) => Self::None,
        }
    }
}

pub const FIELD_SIZE_BOOL: FieldSize = FieldSize::Unsized {
    range: FieldRange {
        min: unsafe { NumberNonZeroUnsigned::new_unchecked(1) },
        max: unsafe {
            NumberNonZeroUnsigned::new_unchecked(NumberUnsigned::MAX)
        },
    },
    possible: FieldAuto::Min,
};
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FieldSize {
    Unsized {
        range: FieldRange,
        possible: FieldAuto,
    },
    Value(NumberNonZeroUnsigned),
}
impl FieldSize {
    pub fn new_bits(bits: NumberNonZeroUnsigned) -> Self {
        Self::Value(bits)
    }
    pub fn new_bytes(bytes: NumberNonZeroUnsigned) -> Self {
        Self::Value(
            NumberNonZeroUnsigned::new(bytes.get().checked_mul(8).unwrap())
                .unwrap(),
        )
    }
    pub fn new_unsized() -> Self {
        Self::Unsized {
            range: FieldRange::new_unsize(),
            possible: FieldAuto::None,
        }
    }
    pub fn is_undefined(&self) -> bool {
        matches!(
            self,
            Self::Unsized {
                possible: FieldAuto::None,
                ..
            }
        )
    }
    pub fn is_possible(&self) -> bool {
        !self.is_undefined()
    }
    pub fn is_final(&self) -> bool {
        self.final_value().is_some()
    }
    pub fn final_value(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::Value(value) => Some(*value),
            Self::Unsized { .. } => None,
        }
    }
    pub fn update_action<F>(&mut self, mut action: F) -> Option<bool>
    where
        F: FnMut(Self) -> Option<Self>,
    {
        let new_size = action(*self)?;
        let changed = *self != new_size;
        *self = new_size;
        Some(changed)
    }
    pub fn set_final_value(
        self,
        final_value: NumberNonZeroUnsigned,
    ) -> Option<Self> {
        match self {
            //if already value, check if value is eq
            Self::Value(value) => (value == final_value).then_some(self),
            //if in range, valid, otherwise invalid
            Self::Unsized { range, .. } => range
                .contains(&final_value)
                .then_some(Self::Value(final_value)),
        }
    }
    pub fn min(&self) -> NumberNonZeroUnsigned {
        match self {
            Self::Value(value) => *value,
            Self::Unsized { range, .. } => range.min,
        }
    }
    pub fn set_min(self, min: NumberNonZeroUnsigned) -> Option<Self> {
        match self {
            Self::Value(value) => (min <= value).then_some(self),
            Self::Unsized { range, possible } => {
                let range = range.set_min(min)?;
                if let Some(value) = range.single_value() {
                    return Some(Self::Value(value));
                }
                let possible = possible.in_range(&range);
                Some(Self::Unsized { range, possible })
            }
        }
    }
    pub fn max(&self) -> NumberNonZeroUnsigned {
        match self {
            Self::Value(value) => *value,
            Self::Unsized { range, .. } => range.max,
        }
    }
    pub fn set_max(self, max: NumberNonZeroUnsigned) -> Option<Self> {
        match self {
            Self::Value(value) => (max >= value).then_some(self),
            Self::Unsized { range, possible } => {
                let range = range.set_max(max)?;
                if let Some(value) = range.single_value() {
                    return Some(Self::Value(value));
                }
                let possible = possible.in_range(&range);
                Some(Self::Unsized { range, possible })
            }
        }
    }
    pub fn possible_value(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::Value(value) => Some(*value),
            Self::Unsized {
                possible: FieldAuto::Value(value),
                ..
            } => Some(*value),
            Self::Unsized {
                possible: FieldAuto::Min,
                ..
            } => Some(self.min()),
            Self::Unsized {
                possible: FieldAuto::None,
                ..
            } => None,
        }
    }
    pub fn possible_min(&self) -> bool {
        matches!(
            self,
            Self::Unsized {
                possible: FieldAuto::Min,
                ..
            }
        )
    }
    pub fn set_possible_min(mut self) -> Self {
        match self {
            Self::Value(_)
            | Self::Unsized {
                possible: FieldAuto::Value(_),
                ..
            } => {}
            Self::Unsized {
                ref mut possible, ..
            } => *possible = FieldAuto::Min,
        }
        self
    }
    pub fn set_possible_value(
        mut self,
        pos_value: NumberNonZeroUnsigned,
    ) -> Option<Self> {
        match self {
            Self::Unsized { range, .. } if !range.contains(&pos_value) => None,
            Self::Unsized {
                ref mut possible, ..
            } => {
                *possible = FieldAuto::Value(pos_value);
                Some(self)
            }
            Self::Value(value) => (value == pos_value).then_some(self),
        }
    }
    pub fn intersection(self, other: Self) -> Option<Self> {
        self.set_min(self.min().max(other.min()))?
            .set_max(self.max().min(other.max()))
    }
}
impl Default for FieldSize {
    fn default() -> Self {
        Self::new_unsized()
    }
}

pub trait FieldSizeIntersectIter: Sized {
    fn all_reduce_set<R, S>(self, reduce: R, set: S) -> Option<bool>
    where
        R: FnMut(FieldSize, FieldSize) -> Option<FieldSize>,
        S: FnMut(&mut dyn FieldSizeMut, &FieldSize) -> Option<bool>;
    fn all_same_lenght(self) -> Option<bool> {
        self.all_reduce_set(FieldSize::intersection, |me, new_len| {
            let new_me = me.get().intersection(*new_len)?;
            me.set(new_me)
        })
    }
}

impl<'a: 'b, 'b> FieldSizeIntersectIter
    for &'a mut [&'b mut dyn FieldSizeMut]
{
    fn all_reduce_set<R, S>(self, reduce: R, mut set: S) -> Option<bool>
    where
        R: FnMut(FieldSize, FieldSize) -> Option<FieldSize>,
        S: FnMut(&'b mut dyn FieldSizeMut, &FieldSize) -> Option<bool>,
    {
        let final_len = self
            .into_iter()
            .map(|x| x.get())
            .try_fold(FieldSize::default(), reduce)?;
        self.into_iter()
            .map(|size_mut| set(*size_mut, &final_len))
            .try_fold(false, |acc, x| Some(acc | x?))
    }
}

//TODO make it a enum, that is better
pub trait FieldSizeMut {
    fn get(&self) -> FieldSize;
    fn set(&mut self, size: FieldSize) -> Option<bool>;
}

impl<'a> dyn FieldSizeMut + 'a {
    pub fn update_action(
        &mut self,
        mut action: impl FnMut(FieldSize) -> Option<FieldSize>,
    ) -> Option<bool> {
        let new_size = action(self.get())?;
        self.set(new_size)
    }
}

impl<'a, T> FieldSizeMut for T
where
    T: std::borrow::BorrowMut<dyn FieldSizeMut + 'a>,
{
    fn get(&self) -> FieldSize {
        self.borrow().get()
    }
    fn set(&mut self, size: FieldSize) -> Option<bool> {
        self.borrow_mut().set(size)
    }
}
impl<'a> FieldSizeMut for &'a Cell<FieldSize> {
    fn get(&self) -> FieldSize {
        (*self).get()
    }
    fn set(&mut self, size: FieldSize) -> Option<bool> {
        let modify = self.get() != size;
        if modify {
            self.replace(size);
        }
        Some(modify)
    }
}
pub struct FieldSizeMutRef<'a>(&'a mut FieldSize);
impl<'a> FieldSizeMut for FieldSizeMutRef<'a> {
    fn get(&self) -> FieldSize {
        *self.0
    }
    fn set(&mut self, size: FieldSize) -> Option<bool> {
        let modify = *self.0 != size;
        if modify {
            *self.0 = size;
        }
        Some(modify)
    }
}
impl<'a> From<&'a mut FieldSize> for FieldSizeMutRef<'a> {
    fn from(input: &'a mut FieldSize) -> Self {
        Self(input)
    }
}
//don't allow mutation
pub struct FieldSizeMutOwned(FieldSize);
impl FieldSizeMut for FieldSizeMutOwned {
    fn get(&self) -> FieldSize {
        self.0
    }
    fn set(&mut self, size: FieldSize) -> Option<bool> {
        (self.0 == size).then_some(false)
    }
}
impl<'a> From<FieldSize> for FieldSizeMutOwned {
    fn from(input: FieldSize) -> Self {
        Self(input)
    }
}
