use crate::{InputSource, Meaning, NonZeroTypeU, RangeBits};

use super::{Endian, GlobalElement};

#[derive(Clone, Debug)]
pub struct Token {
    pub src: InputSource,
    pub len_bytes: NonZeroTypeU,
    pub endian: Endian,
}

impl Token {
    pub(crate) fn new(
        src: InputSource,
        len_bytes: NonZeroTypeU,
        endian: Endian,
    ) -> Self {
        Self {
            src,
            len_bytes,
            endian,
        }
    }
    pub fn location(&self) -> &InputSource {
        &self.src
    }
    pub fn len_bytes(&self) -> NonZeroTypeU {
        self.len_bytes
    }
    pub fn endian(&self) -> Endian {
        self.endian
    }
}

#[derive(Clone, Debug)]
pub struct TokenField {
    pub location: InputSource,
    pub range: RangeBits,
    pub token: GlobalElement<Token>,
    pub meaning: Meaning,
}
impl TokenField {
    pub(crate) fn new(
        src: InputSource,
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
    pub fn location(&self) -> &InputSource {
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
