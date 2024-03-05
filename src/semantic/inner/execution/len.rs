use std::cell::Cell;
use std::ops::{Bound, RangeBounds};

use crate::NumberNonZeroUnsigned;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FieldRange {
    min: Option<NumberNonZeroUnsigned>,
    max: Option<NumberNonZeroUnsigned>,
}
impl FieldRange {
    fn new(
        min: Option<NumberNonZeroUnsigned>,
        max: Option<NumberNonZeroUnsigned>,
    ) -> Self {
        Self { min, max }
    }
    fn new_unsize() -> Self {
        Self::new(None, None)
    }
    fn set_min(self, new_min: NumberNonZeroUnsigned) -> Option<Self> {
        if matches!(self.max, Some(max) if max < new_min) {
            //max is less then the new_min, unable to set this value
            return None;
        }
        let min = self
            .min
            .map(|old_min| old_min.max(new_min))
            .unwrap_or(new_min);
        Some(Self {
            min: Some(min),
            max: self.max,
        })
    }
    fn set_max(self, new_max: NumberNonZeroUnsigned) -> Option<Self> {
        if matches!(self.min, Some(min) if min > new_max) {
            //unable to set this max value
            return None;
        }
        let max = self
            .max
            .map(|old_max| old_max.min(new_max))
            .unwrap_or(new_max);
        Some(Self {
            min: self.min,
            max: Some(max),
        })
    }
    fn single_value(&self) -> Option<NumberNonZeroUnsigned> {
        self.min
            .zip(self.max)
            .and_then(|(min, max)| (min == max).then_some(min))
    }
}
impl RangeBounds<NumberNonZeroUnsigned> for FieldRange {
    fn start_bound(&self) -> Bound<&NumberNonZeroUnsigned> {
        // range always starts from 1
        self.min
            .as_ref()
            .map(Bound::Included)
            .unwrap_or(Bound::Unbounded)
    }

    fn end_bound(&self) -> Bound<&NumberNonZeroUnsigned> {
        self.max
            .as_ref()
            .map(Bound::Included)
            .unwrap_or(Bound::Unbounded)
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
        min: Some(unsafe { NumberNonZeroUnsigned::new_unchecked(1) }),
        max: None,
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
    pub fn min_bits(&self) -> NumberNonZeroUnsigned {
        match self {
            Self::Value(value) => *value,
            Self::Unsized { range, .. } => {
                // field can't be smaller then 1
                range.min.unwrap_or(NumberNonZeroUnsigned::new(1).unwrap())
            }
        }
    }
    pub fn set_min_bytes(self, min: NumberNonZeroUnsigned) -> Option<Self> {
        let min = NumberNonZeroUnsigned::new(min.get() * 8).unwrap();
        self.set_min_bits(min)
    }
    pub fn set_min_bits(self, min: NumberNonZeroUnsigned) -> Option<Self> {
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
    pub fn max_bits(&self) -> NumberNonZeroUnsigned {
        match self {
            Self::Value(value) => *value,
            Self::Unsized { range, .. } => {
                // that will be technically true LOL
                range.max.unwrap_or(NumberNonZeroUnsigned::MAX)
            }
        }
    }
    pub fn set_max_bytes(self, max: NumberNonZeroUnsigned) -> Option<Self> {
        let max = NumberNonZeroUnsigned::new(max.get() * 8).unwrap();
        self.set_max_bits(max)
    }
    pub fn set_max_bits(self, max: NumberNonZeroUnsigned) -> Option<Self> {
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
            } => Some(self.min_bits()),
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
        self.set_min_bits(self.min_bits().max(other.min_bits()))?
            .set_max_bits(self.max_bits().min(other.max_bits()))
    }
}
impl Default for FieldSize {
    fn default() -> Self {
        Self::new_unsized()
    }
}

pub fn restrict_field_same_size(
    fields: &mut [&mut dyn FieldSizeMut],
) -> Option<bool> {
    let final_len = fields
        .iter_mut()
        .map(|x| x.get())
        .try_fold(FieldSize::default(), |me, new_len| {
            me.intersection(new_len)
        })?;
    fields
        .iter_mut()
        .map(|size_mut| size_mut.set(final_len))
        .try_fold(false, |acc, x| Some(acc | x?))
}

//TODO make it a enum, that is better
pub trait FieldSizeMut {
    fn get(&self) -> FieldSize;
    fn set(&mut self, size: FieldSize) -> Option<bool>;
}

impl<'a> dyn FieldSizeMut + 'a {
    pub fn as_dyn(&mut self) -> &mut dyn FieldSizeMut {
        self
    }
    pub fn update_action(
        &mut self,
        mut action: impl FnMut(FieldSize) -> Option<FieldSize>,
    ) -> Option<bool> {
        let new_size = action(self.get())?;
        self.set(new_size)
    }
}

impl FieldSizeMut for &Cell<FieldSize> {
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
impl FieldSizeMut for FieldSize {
    fn get(&self) -> FieldSize {
        *self
    }
    fn set(&mut self, size: FieldSize) -> Option<bool> {
        let modify = *self != size;
        if modify {
            *self = size;
        }
        Some(modify)
    }
}
impl FieldSizeMut for &mut FieldSize {
    fn get(&self) -> FieldSize {
        (**self).get()
    }
    fn set(&mut self, size: FieldSize) -> Option<bool> {
        (*self).set(size)
    }
}
//don't allow mutation
pub struct FieldSizeUnmutable(FieldSize);
impl FieldSizeMut for FieldSizeUnmutable {
    fn get(&self) -> FieldSize {
        self.0
    }
    fn set(&mut self, size: FieldSize) -> Option<bool> {
        (self.0 == size).then_some(false)
    }
}
impl From<FieldSize> for FieldSizeUnmutable {
    fn from(input: FieldSize) -> Self {
        Self(input)
    }
}
