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
pub enum FieldSize {
    Unsized {
        range: FieldRange,
        possible_min: bool,
        possible_value: Option<NumberNonZeroUnsigned>,
    },
    Value(NumberNonZeroUnsigned),
}

impl FieldSize {
    pub fn new_bool() -> Self {
        Self::default()
            .set_min_bits(1.try_into().unwrap())
            .unwrap()
            .set_possible_min()
    }
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
            possible_min: false,
            possible_value: None,
        }
    }
    pub fn is_unrestricted(&self) -> bool {
        matches!(
            self,
            Self::Unsized {
                range: FieldRange {
                    min: None,
                    max: None
                },
                ..
            }
        )
    }
    pub fn is_undefined(&self) -> bool {
        !self.is_possible()
    }
    pub fn is_fully_undefined(&self) -> bool {
        matches!(
            self,
            Self::Unsized {
                possible_min: _,
                possible_value: None,
                range: FieldRange {
                    min: None,
                    max: None
                }
            }
        )
    }
    pub fn is_possible(&self) -> bool {
        matches!(
            self,
            Self::Unsized {
                possible_value: Some(_),
                ..
            } | Self::Unsized {
                range: FieldRange { min: Some(_), .. },
                possible_min: true,
                ..
            } | Self::Value(_)
        )
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
    #[must_use]
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
    pub fn min_bits(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::Value(value) => Some(*value),
            Self::Unsized { range, .. } => range.min,
        }
    }
    pub fn set_min_bytes(self, min: NumberNonZeroUnsigned) -> Option<Self> {
        let min = NumberNonZeroUnsigned::new(min.get() * 8).unwrap();
        self.set_min_bits(min)
    }
    pub fn set_min_bits(self, min: NumberNonZeroUnsigned) -> Option<Self> {
        match self {
            Self::Value(value) => (min <= value).then_some(self),
            Self::Unsized {
                range,
                possible_value,
                possible_min,
            } => {
                let range = range.set_min(min)?;
                if let Some(value) = range.single_value() {
                    return Some(Self::Value(value));
                }
                let possible_value =
                    possible_value.filter(|value| range.contains(value));
                Some(Self::Unsized {
                    range,
                    possible_min,
                    possible_value,
                })
            }
        }
    }
    pub fn max_bits(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::Value(value) => Some(*value),
            Self::Unsized { range, .. } => range.max,
        }
    }
    pub fn set_max_bytes(self, max: NumberNonZeroUnsigned) -> Option<Self> {
        let max = NumberNonZeroUnsigned::new(max.get() * 8).unwrap();
        self.set_max_bits(max)
    }
    pub fn set_max_bits(self, max: NumberNonZeroUnsigned) -> Option<Self> {
        match self {
            Self::Value(value) => (max >= value).then_some(self),
            Self::Unsized {
                range,
                possible_min,
                possible_value,
            } => {
                let range = range.set_max(max)?;
                if let Some(value) = range.single_value() {
                    return Some(Self::Value(value));
                }
                let possible_value =
                    possible_value.filter(|value| range.contains(value));
                Some(Self::Unsized {
                    range,
                    possible_value,
                    possible_min,
                })
            }
        }
    }
    pub fn possible_value(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::Value(value) => Some(*value),
            Self::Unsized {
                possible_value: Some(value),
                ..
            } => Some(*value),
            Self::Unsized {
                possible_min: true,
                range:
                    FieldRange {
                        min: Some(value), ..
                    },
                ..
            } => Some(*value),
            Self::Unsized { .. } => None,
        }
    }
    pub fn possible_min(&self) -> bool {
        match self {
            Self::Value(_) => false,
            Self::Unsized { possible_min, .. } => *possible_min,
        }
    }
    pub fn set_possible_min(mut self) -> Self {
        match &mut self {
            Self::Value(_) => {}
            Self::Unsized { possible_min, .. } => *possible_min = true,
        }
        self
    }
    pub fn set_possible_bytes(
        self,
        pos_bytes: NumberNonZeroUnsigned,
    ) -> Option<Self> {
        let pos_bits = (pos_bytes.get() * 8).try_into().unwrap();
        self.set_possible_bits(pos_bits)
    }
    pub fn set_possible_bits(
        mut self,
        pos_bits: NumberNonZeroUnsigned,
    ) -> Option<Self> {
        match self {
            Self::Unsized { range, .. } if !range.contains(&pos_bits) => None,
            Self::Unsized {
                ref mut possible_value,
                ..
            } => {
                *possible_value = Some(pos_bits);
                Some(self)
            }
            Self::Value(value) => (value == pos_bits).then_some(self),
        }
    }
    pub fn intersection(self, other: Self) -> Option<Self> {
        let mut result = self;
        if let Some(min_bits) = other.min_bits() {
            result = result.set_min_bits(min_bits)?;
        }
        if let Some(max_bits) = other.max_bits() {
            result = result.set_max_bits(max_bits)?;
        }
        Some(result)
    }
}
impl Default for FieldSize {
    fn default() -> Self {
        Self::new_unsized()
    }
}

//TODO make it a enum, that is better
pub trait FieldSizeMut {
    fn get(&self) -> FieldSize;
    fn set(&mut self, size: FieldSize) -> Option<bool>;
}

impl<'a> dyn FieldSizeMut + 'a {
    #[must_use]
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
impl FieldSizeMut for &mut FieldSize {
    fn get(&self) -> FieldSize {
        **self
    }
    fn set(&mut self, size: FieldSize) -> Option<bool> {
        let modify = **self != size;
        if modify {
            **self = size;
        }
        Some(modify)
    }
}
//don't allow mutation
pub struct FieldSizeUnmutable(pub(crate) FieldSize);
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

fn intersect_all(fields: &mut [&mut dyn FieldSizeMut]) -> Option<bool> {
    let final_len = fields
        .iter_mut()
        .map(|x| x.get())
        .try_fold(FieldSize::default(), |me, new_len| {
            me.intersection(new_len)
        })?;
    // update all the fields with the final size
    fields
        .iter_mut()
        .map(|size_mut| {
            size_mut
                .get()
                .intersection(final_len)
                .and_then(|s| size_mut.set(s))
        })
        .try_fold(false, |acc, x| Some(acc | x?))
}

fn set_possible(
    x: &mut dyn FieldSizeMut,
    possible_min: bool,
    possible_value: Option<NumberNonZeroUnsigned>,
) -> Option<bool> {
    if !possible_min && possible_value.is_none() {
        return Some(false);
    }
    x.update_action(|mut x| {
        if possible_min {
            x = x.set_possible_min();
        }
        if let Some(value) = possible_value {
            x = x.set_possible_bits(value)?;
        }
        Some(x)
    })
}

/// comparison between a and b
pub fn a_cmp_b(
    a: &mut dyn FieldSizeMut,
    b: &mut dyn FieldSizeMut,
) -> Option<bool> {
    let mut modified = false;
    // a and b have the same size
    modified |= intersect_all(&mut [a, b])?;

    // TODO both should have the same possible value?

    Some(modified)
}

/// a and b are the same value
pub fn a_equivalent_b(
    a: &mut dyn FieldSizeMut,
    b: &mut dyn FieldSizeMut,
) -> Option<bool> {
    let mut modified = false;
    // a and b have the same size
    modified |= intersect_all(&mut [a, b])?;

    let a_value = a.get();
    let b_value = b.get();
    // if one is possible min, both are
    let possible_min = a_value.possible_min() | b_value.possible_min();

    let possible_value = match (a_value.possible_value(), b_value.possible_value()) {
        (None, None) => None,
        (Some(value), None) | (None, Some(value)) => Some(value),
        (Some(a), Some(b)) if a== b => Some(a),
        (Some(_a), Some(_b)) /*if _a != _b*/ => None,
    };

    modified |= set_possible(a, possible_min, possible_value).unwrap();
    modified |= set_possible(b, possible_min, possible_value).unwrap();

    Some(modified)
}

/// value b is put into a, AKA a = b;
pub fn a_receive_b(
    a: &mut dyn FieldSizeMut,
    b: &mut dyn FieldSizeMut,
) -> Option<bool> {
    let mut modified = false;

    // if a size is unrestricted and no possible value is set, use b
    if a.get().is_fully_undefined() {
        modified |= a.set(b.get())?;
    } else {
        // a and b have the same size
        modified |= intersect_all(&mut [a, b])?;
    }

    // if b allow possible min, a also allow min
    if b.get().possible_min() {
        modified |= a.update_action(|a| Some(a.set_possible_min())).unwrap();
    }

    Some(modified)
}

/// a operation that result into b, don't change size
pub fn a_generate_b(
    a: &mut dyn FieldSizeMut,
    b: &mut dyn FieldSizeMut,
) -> Option<bool> {
    let mut modified = false;
    // both have the same size
    modified |= intersect_all(&mut [a, b])?;

    // any possible value that a have, b will also have
    if let FieldSize::Unsized {
        range: _,
        possible_min,
        possible_value,
    } = a.get()
    {
        modified |= b
            .update_action(|mut x| {
                if possible_min {
                    x = x.set_possible_min();
                }
                if let Some(value) = possible_value {
                    // NOTE same value will always be possible
                    x = x.set_possible_bits(value).unwrap();
                }
                Some(x)
            })
            .unwrap();
    }

    Some(modified)
}

/// a extend into b
pub fn a_extend_b(
    a: &mut dyn FieldSizeMut,
    b: &mut dyn FieldSizeMut,
) -> Option<bool> {
    let mut modified = false;
    // a need to be smaller or equal to b
    if let Some(max_bits) = b.get().max_bits() {
        modified |= a.set(a.get().set_max_bits(max_bits)?)?;
    }

    // b need to be smaller or equal to a
    if let Some(min_bits) = b.get().min_bits() {
        modified |= b.set(b.get().set_min_bits(min_bits)?)?;
    }

    Some(modified)
}

/// operation with a and b, outputing c
pub fn a_b_generate_c(
    a: &mut dyn FieldSizeMut,
    b: &mut dyn FieldSizeMut,
    c: &mut dyn FieldSizeMut,
) -> Option<bool> {
    let mut modified = false;
    // both have the same size
    modified |= intersect_all(&mut [a, b, c])?;

    let a_value = a.get();
    let b_value = b.get();
    let c_value = c.get();
    //TODO is that really right?
    match c_value {
        // any possible value that c have, a and b will also have
        FieldSize::Unsized {
            range: _,
            possible_min,
            possible_value,
        } if possible_min || possible_value.is_some() => {
            modified |= set_possible(a, possible_min, possible_value).unwrap();
            modified |= set_possible(b, possible_min, possible_value).unwrap();
        }
        // if c un fully undefined, get the possible values from a and b
        _ if c_value.is_undefined() => {
            if a_value.possible_min() && b_value.possible_min() {
                c.update_action(|c| Some(c.set_possible_min())).unwrap();
            }
        }
        _ => {}
    }

    Some(modified)
}
