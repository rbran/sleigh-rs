use std::ops::{Bound, RangeBounds};
use std::path::Path;
use std::rc::Rc;

mod preprocessor;

pub(crate) mod semantic;
pub(crate) mod syntax;

use preprocessor::FilePreProcessor;
pub(crate) use preprocessor::PreprocessorError;

use semantic::pcode_macro::PcodeMacroError;
use semantic::table::TableError;
use syntax::{BitRangeLsbLen, BitRangeLsbMsb};
use thiserror::Error;

pub use semantic::meaning::Meaning;
pub use semantic::pattern::{Block, Pattern, PatternLen};
pub use semantic::pcode_macro::PcodeMacro;
pub use semantic::space::Space;
pub use semantic::table::{Constructor, Table};
pub use semantic::token::{Token, TokenField};
pub use semantic::user_function::UserFunction;
pub use semantic::varnode::{Bitrange, Context, Varnode};
pub use semantic::*;


pub type FloatType = f64;
pub type NumberUnsigned = u64;
pub type NumberSigned = i64;
pub type NumberSuperSigned = i128;
pub type NumberSuperUnsigned = u128;
pub type NumberNonZeroUnsigned = core::num::NonZeroU64;
pub type NumberNonZeroSigned = core::num::NonZeroI64;
pub type NumberNonZeroSuperSigned = core::num::NonZeroI128;
pub type DisassemblyType = NumberSuperSigned;

//old naming convention
pub type IntTypeU = NumberUnsigned;
pub type IntTypeS = NumberSigned;
pub type NonZeroTypeU = NumberNonZeroUnsigned;
pub type NonZeroTypeS = NumberNonZeroSigned;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Endian {
    Little,
    Big,
}

impl Endian {
    pub fn is_little(&self) -> bool {
        matches!(self, Self::Little)
    }
    pub fn is_big(&self) -> bool {
        matches!(self, Self::Big)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Number {
    Positive(NumberUnsigned),
    Negative(NumberUnsigned),
}

impl Number {
    pub fn unsigned(self) -> Option<NumberUnsigned> {
        match self {
            Number::Positive(value) => Some(value),
            _ => None,
        }
    }
    pub fn signed(self) -> Option<NumberSigned> {
        match self {
            Number::Positive(value) => NumberSigned::try_from(value).ok(),
            Number::Negative(value) => NumberSigned::try_from(value)
                .ok()
                .and_then(NumberSigned::checked_neg),
        }
    }
    pub fn signed_super(self) -> NumberSuperSigned {
        match self {
            Number::Positive(value) => NumberSuperSigned::from(value),
            Number::Negative(value) => -NumberSuperSigned::from(value),
        }
    }

    pub fn as_unsigned(&self) -> Option<NumberUnsigned> {
        match self {
            Number::Positive(value) => Some(*value),
            Number::Negative(value) => {
                //TODO improve this to use Super Signed?
                NumberSigned::try_from(*value)
                    .ok()?
                    .checked_neg()
                    .map(|x| x as NumberUnsigned)
            }
        }
    }
}
impl From<u64> for Number {
    fn from(value: u64) -> Self {
        Self::Positive(value)
    }
}
impl From<i64> for Number {
    fn from(value: i64) -> Self {
        if value.is_negative() {
            Self::Negative(value.unsigned_abs())
        } else {
            Self::Positive(value as u64)
        }
    }
}

//constants used only for debug purposes, don't commit with a diferent value
pub(crate) const DISABLE_EXECUTION_PARSING: bool = true;

pub const IDENT_INSTRUCTION: &str = "instruction";
pub const IDENT_INST_START: &str = "inst_start";
pub const IDENT_INST_NEXT: &str = "inst_next";
pub const IDENT_EPSILON: &str = "epsilon";
pub const IDENT_CONST: &str = "const";
pub const IDENT_UNIQUE: &str = "unique";

#[derive(Clone, Debug)]
pub struct ParamNumber {
    min: usize,
    max: Option<usize>,
}

impl ParamNumber {
    pub fn new(min: usize, max: Option<usize>) -> Self {
        Self { min, max }
    }
}

impl RangeBounds<usize> for ParamNumber {
    fn start_bound(&self) -> Bound<&usize> {
        Bound::Included(&self.min)
    }

    fn end_bound(&self) -> Bound<&usize> {
        match &self.max {
            Some(max) => Bound::Included(max),
            None => Bound::Unbounded,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitRange(std::ops::Range<NumberUnsigned>);
impl BitRange {
    pub fn new(
        start: NumberUnsigned,
        end: NumberUnsigned,
        span: Span,
    ) -> Result<Self, SleighError> {
        if start >= end {
            return Err(SleighError::InvalidBitrange(span));
        }
        Ok(Self(start..end))
    }
    pub fn len(&self) -> NumberNonZeroUnsigned {
        let len = self.0.end - self.0.start;
        NumberNonZeroUnsigned::new(len).unwrap()
    }
    pub fn start(&self) -> NumberUnsigned {
        self.0.start
    }
    pub fn end(&self) -> NumberNonZeroUnsigned {
        NumberNonZeroUnsigned::new(self.0.end).unwrap_or_else(|| unreachable!())
    }
    pub fn field_min_len(&self) -> NumberNonZeroUnsigned {
        self.end()
    }
}
impl TryFrom<BitRangeLsbMsb> for BitRange {
    type Error = SleighError;
    fn try_from(value: BitRangeLsbMsb) -> Result<Self, Self::Error> {
        Self::new(value.lsb_bit, value.msb_bit + 1, value.src)
    }
}
impl TryFrom<BitRangeLsbLen> for BitRange {
    type Error = SleighError;
    fn try_from(value: BitRangeLsbLen) -> Result<Self, Self::Error> {
        Self::new(value.lsb_bit, value.lsb_bit + value.n_bits, value.src)
    }
}
impl RangeBounds<NumberUnsigned> for BitRange {
    fn start_bound(&self) -> Bound<&NumberUnsigned> {
        self.0.start_bound()
    }
    fn end_bound(&self) -> Bound<&NumberUnsigned> {
        self.0.end_bound()
    }
}

#[derive(Error, Debug)]
pub enum SleighError {
    #[error("Missing global endian definition")]
    EndianMissing,

    #[error("Multiple alignment definitions")]
    AlignmentMult,
    #[error("Multiple global endian definitions")]
    EndianMult,
    #[error("Unable during the pre-processor phase {0}")]
    PreProcessor(#[from] PreprocessorError),

    #[error("Invalid Endian at {0}")]
    InvalidEndian(Span),
    #[error("Endian defined multiple times {0}")]
    MultipleEndian(Span),

    #[error("Invalid Allignment {0}")]
    InvalidAlignment(Span),
    #[error("Alignment defined multiple times {0}")]
    MultipleAlignment(Span),

    #[error("Space missing size attribute {0}")]
    SpaceSizeMissing(Span),
    #[error("Space duplicated attribute {0}")]
    SpaceAttMultiple(Span),
    #[error("Space attribute value is invalid {0}")]
    SpaceInvalidAtt(Span),

    #[error("Token size is invalid {0}")]
    TokenSizeInvalid(Span),
    #[error("Token size need to be multiple of 8 {0}")]
    TokenSizeNonByte(Span),

    #[error("Token Field size is invalid {0}")]
    TokenFieldSizeInvalid(Span),
    #[error("Token Field Attribute is declared mutliple times {0}")]
    TokenFieldAttDup(Span),

    #[error("Varnode size is invalid {0}")]
    VarnodeInvalidSize(Span),

    #[error("Invalid statement at {0}")]
    UnableToParse(Span, nom::error::ErrorKind),
    #[error("Invalid statement at {0}")]
    StatementInvalid(Span),

    #[error("Undefined space at {0}")]
    SpaceUndefined(Span),
    #[error("Invalid space at {0}")]
    SpaceInvalid(Span),

    #[error("Undefined varnode at {0}")]
    VarnodeUndefined(Span),
    #[error("Invalid varnode at {0}")]
    VarnodeInvalid(Span),

    #[error("Context Attribute is declared mutliple times {0}")]
    ContextAttDup(Span),

    #[error("Invalid bitrange at {0}")]
    InvalidBitrange(Span),

    #[error("Attach undefined variable at {0}")]
    AttachUndefinedVariable(Span),
    #[error("Attach invalid variable at {0}")]
    AttachInvalidVariable(Span),
    #[error("Attach multiple times at {0}")]
    AttachMultiple(Span),

    #[error("Global declared a second time {0}")]
    DuplicatedGlobal(Span),

    #[error("Dual ellipsis {0}")]
    DualEllipsis(Span),

    #[error("Unexpecte eof")]
    UnexpectedEof,

    #[error("BitRange size is invalid {0}")]
    BitrangeInvalidSize(Span),
    #[error("Context size is invalid {0}")]
    ContextInvalidSize(Span),

    //TODO doit better
    #[error("Table Error: {0}")]
    Table(#[from] TableError),
    #[error("PcodeMacro Error {0}")]
    PcodeMacro(#[from] PcodeMacroError),
    //TODO delete this
    #[error("SemanticError {0}")]
    SemanticError(#[from] SemanticError),
}

#[derive(Debug, Clone)]
pub enum Span {
    Macro(MacroSpan),
    File(FileSpan),
}
impl Span {
    pub fn start_line(&self) -> u64 {
        match self {
            Span::Macro(span) => span.start.line,
            Span::File(span) => span.start.line,
        }
    }
    pub fn end_line(&self) -> u64 {
        match self {
            Span::Macro(span) => span.end_line,
            Span::File(span) => span.end_line,
        }
    }
    pub fn start_column(&self) -> u64 {
        match self {
            Span::Macro(span) => span.start.column,
            Span::File(span) => span.start.column,
        }
    }
    pub fn end_column(&self) -> u64 {
        match self {
            Span::Macro(span) => span.end_column,
            Span::File(span) => span.end_column,
        }
    }
    fn combine(start: Location, end: Location) -> Self {
        match (start, end) {
            (Location::Macro(start), Location::Macro(end)) => {
                Self::combine_macros(start, end)
            }
            (Location::File(start), Location::File(end)) => {
                Self::combine_files(start, end)
            }
            (Location::Macro(_), Location::File(_))
            | (Location::File(_), Location::Macro(_)) => todo!(),
        }
    }
    fn combine_files(start: FileLocation, end: FileLocation) -> Self {
        Self::File(FileSpan::combine(start, end))
    }
    fn combine_macros(start: MacroLocation, end: MacroLocation) -> Self {
        Self::Macro(MacroSpan::combine(start, end))
    }
    fn start(self) -> Location {
        match self {
            Span::Macro(x) => Location::Macro(x.start()),
            Span::File(x) => Location::File(x.start()),
        }
    }
    fn end(self) -> Location {
        match self {
            Span::Macro(x) => Location::Macro(x.end()),
            Span::File(x) => Location::File(x.end()),
        }
    }
}
impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location(")?;
        match self {
            Span::Macro(x) => x.fmt(f)?,
            Span::File(x) => x.fmt(f)?,
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub enum Location {
    Macro(MacroLocation),
    File(FileLocation),
}
impl Location {
    pub fn into_span(&self, end_line: u64, end_column: u64) -> Span {
        match self {
            Location::Macro(x) => {
                Span::Macro(x.into_span(end_line, end_column))
            }
            Location::File(x) => Span::File(x.into_span(end_line, end_column)),
        }
    }
}
impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location(")?;
        match self {
            Location::Macro(x) => x.fmt(f)?,
            Location::File(x) => x.fmt(f)?,
        }
        write!(f, ")")
    }
}
#[derive(Debug, Clone)]
pub struct MacroLocation {
    pub value: FileSpan,
    pub expansion: FileSpan,
    pub line: u64,
    pub column: u64,
}
impl MacroLocation {
    fn into_span(&self, end_line: u64, end_column: u64) -> MacroSpan {
        MacroSpan {
            start: self.clone(),
            end_line,
            end_column,
        }
    }
}
impl std::fmt::Display for MacroLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "MacroSpan({}, {}, {}:{})",
            self.value,
            self.expansion,
            self.line + 1,
            self.column + 1,
        )
    }
}
#[derive(Debug, Clone)]
pub struct MacroSpan {
    pub start: MacroLocation,
    pub end_line: u64,
    pub end_column: u64,
}
impl MacroSpan {
    fn start(self) -> MacroLocation {
        self.start
    }

    fn end(self) -> MacroLocation {
        let mut end = self.start;
        end.line = self.end_line;
        end.column = self.end_column;
        end
    }

    fn combine(start: MacroLocation, end: MacroLocation) -> MacroSpan {
        assert_eq!(start.value.start.file, end.value.start.file);
        assert!(start.line <= end.line);
        assert!(if start.line == end.line {
            start.column <= end.column
        } else {
            true
        });
        MacroSpan {
            start,
            end_line: end.line,
            end_column: end.column,
        }
    }
}
impl std::fmt::Display for MacroSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "MacroSpan({}, {}, {}:{}, {}:{})",
            self.start.value,
            self.start.expansion,
            self.start.line + 1,
            self.start.column + 1,
            self.end_line + 1,
            self.end_column + 1,
        )
    }
}
#[derive(Debug, Clone)]
pub struct FileSpan {
    pub start: FileLocation,
    pub end_line: u64,
    pub end_column: u64,
}
impl FileSpan {
    fn start(self) -> FileLocation {
        self.start
    }

    fn end(self) -> FileLocation {
        let mut end = self.start;
        end.line = self.end_line;
        end.column = self.end_column;
        end
    }

    fn combine(start: FileLocation, end: FileLocation) -> FileSpan {
        assert_eq!(start.file, end.file);
        assert!(start.line <= end.line);
        assert!(if start.line == end.line {
            start.column <= end.column
        } else {
            true
        });
        FileSpan {
            start,
            end_line: end.line,
            end_column: end.column,
        }
    }
}
impl std::fmt::Display for FileSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "FileSpan({}, start:{}:{}, end:{}:{})",
            self.start.file.as_os_str().to_string_lossy(),
            self.start.line + 1,
            self.start.column + 1,
            self.end_line + 1,
            self.end_column + 1,
        )
    }
}
#[derive(Clone, Debug, Error)]
pub struct FileLocation {
    pub file: Rc<Path>,
    pub line: u64,
    pub column: u64,
}
impl FileLocation {
    fn into_span(&self, end_line: u64, end_column: u64) -> FileSpan {
        FileSpan {
            start: self.clone(),
            end_line,
            end_column,
        }
    }
}
impl std::fmt::Display for FileLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file.as_os_str().to_string_lossy(),
            self.line + 1,
            self.column + 1,
        )
    }
}

pub fn file_to_sleigh(filename: &Path) -> Result<Sleigh, SleighError> {
    let mut pro = FilePreProcessor::new(&filename)?;
    let mut buf = vec![];
    let syntax = crate::syntax::Sleigh::parse(&mut pro, &mut buf, false)?;
    let sleigh = crate::semantic::Sleigh::new(syntax)?;
    Ok(sleigh)
}
