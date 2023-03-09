use sleigh4rust::Endian;

use crate::{NumberNonZeroUnsigned, RangeBits, Span};

use super::meaning::Meaning;
use super::GlobalElement;

#[derive(Clone, Debug)]
pub struct Token {
    pub src: Span,
    pub len_bytes: NumberNonZeroUnsigned,
    pub endian: Endian,
}

impl Token {
    pub(crate) fn new(
        src: Span,
        len_bytes: NumberNonZeroUnsigned,
        endian: Endian,
    ) -> Self {
        Self {
            src,
            len_bytes,
            endian,
        }
    }
    pub fn location(&self) -> &Span {
        &self.src
    }
    pub fn len_bytes(&self) -> NumberNonZeroUnsigned {
        self.len_bytes
    }
    pub fn endian(&self) -> Endian {
        self.endian
    }
}

#[derive(Clone, Debug)]
pub struct TokenField {
    pub location: Span,
    pub range: RangeBits,
    pub token: GlobalElement<Token>,
    pub meaning: Meaning,
}
impl TokenField {
    pub(crate) fn new(
        src: Span,
        range: RangeBits,
        token: GlobalElement<Token>,
        meaning: Meaning,
    ) -> Self {
        Self {
            location: src,
            range,
            token,
            meaning,
        }
    }
    pub fn location(&self) -> &Span {
        &self.location
    }
    pub fn range(&self) -> &RangeBits {
        &self.range
    }
    pub fn token(&self) -> &GlobalElement<Token> {
        &self.token
    }
    pub fn meaning(&self) -> &Meaning {
        &self.meaning
    }
}
