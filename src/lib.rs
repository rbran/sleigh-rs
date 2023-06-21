use std::ops::{Bound, RangeBounds};
use std::path::Path;
use std::rc::Rc;

use thiserror::Error;

pub(crate) mod preprocessor;
pub(crate) mod semantic;
pub(crate) mod syntax;

use preprocessor::FilePreProcessor;
use preprocessor::PreprocessorError;

use syntax::{BitRangeLsbLen, BitRangeLsbMsb};

pub use semantic::disassembly;
pub use semantic::display;
pub use semantic::meaning;
pub use semantic::pattern;
pub use semantic::pcode_macro;
pub use semantic::space;
pub use semantic::table;
pub use semantic::token;
pub use semantic::user_function;
pub use semantic::varnode;
pub use semantic::{
    AttachLiteralId, AttachNumberId, AttachVarnodeId, BitrangeId, ContextId,
    GlobalScope, PcodeMacroId, Sleigh, SpaceId, TableId, TokenFieldId, TokenId,
    UserFunctionId, VarnodeId,
};

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

//constants used only for debug purposes, don't commit with a diferent value
pub(crate) const DISABLE_EXECUTION_PARSING: bool = true;

pub(crate) const IDENT_INSTRUCTION: &str = "instruction";
pub(crate) const IDENT_INST_START: &str = "inst_start";
pub(crate) const IDENT_INST_NEXT: &str = "inst_next";
pub(crate) const IDENT_EPSILON: &str = "epsilon";

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
    pub fn is_positive(&self) -> bool {
        matches!(self, Number::Positive(x) if *x != 0)
    }
    pub fn is_negative(&self) -> bool {
        matches!(self, Number::Negative(x) if *x != 0)
    }
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
    pub(crate) fn bits_required(&self) -> u32 {
        let value = match self {
            Number::Positive(value) | Number::Negative(value)
                if *value != 0 =>
            {
                value.ilog2() + 1
            }
            _ => 1,
        };
        let signal = match self {
            Number::Positive(_) => 0,
            Number::Negative(_) => 1,
        };
        value + signal
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

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FieldBits(pub(crate) std::ops::Range<NumberUnsigned>);
impl FieldBits {
    pub fn new(start: NumberUnsigned, end: NumberUnsigned) -> Self {
        if start >= end {
            unreachable!()
        }
        Self(start..end)
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
impl TryFrom<BitRangeLsbMsb> for FieldBits {
    type Error = SleighError;
    fn try_from(value: BitRangeLsbMsb) -> Result<Self, Self::Error> {
        let start = value.lsb_bit;
        let end = value.msb_bit + 1;
        if start >= end {
            return Err(SleighError::InvalidBitrange(value.src));
        }
        Ok(Self::new(start, end))
    }
}
impl TryFrom<BitRangeLsbLen> for FieldBits {
    type Error = SleighError;
    fn try_from(value: BitRangeLsbLen) -> Result<Self, Self::Error> {
        let start = value.lsb_bit;
        let end = value.lsb_bit + value.n_bits;
        if start >= end {
            return Err(SleighError::InvalidBitrange(value.src));
        }
        Ok(Self::new(start, end))
    }
}
impl RangeBounds<NumberUnsigned> for FieldBits {
    fn start_bound(&self) -> Bound<&NumberUnsigned> {
        self.0.start_bound()
    }
    fn end_bound(&self) -> Bound<&NumberUnsigned> {
        self.0.end_bound()
    }
}

#[derive(Error, Debug)]
pub enum SleighError {
    #[error("Unable during the pre-processor phase {0}")]
    PreProcessor(#[from] PreprocessorError),

    #[error("Missing global endian definition")]
    EndianMissing,
    #[error("Missing default Space Address")]
    SpaceMissingDefault,

    #[error("Multiple global endian definitions")]
    EndianMultiple,

    #[error("Name already taken first")]
    NameDuplicated,

    #[error("Invalid Endian at {0}")]
    InvalidEndian(Span),
    #[error("Endian defined multiple times {0}")]
    MultipleEndian(Span),

    #[error("Multiple alignment definitions")]
    AlignmentMultiple,
    #[error("Invalid Allignment {0}")]
    AlignmentInvalid(Span),

    #[error("Space missing size attribute {0}")]
    SpaceSizeMissing(Span),
    #[error("Space duplicated attribute {0}")]
    SpaceAttMultiple(Span),
    #[error("Space attribute value is invalid {0}")]
    SpaceInvalidAtt(Span),
    #[error("Space don't have the size att {0}")]
    SpaceMissingSize(Span),
    #[error("Space don't have the size att {0}")]
    SpaceInvalidSize(Span),
    #[error("Multiple default Space Address")]
    SpaceMultipleDefault,

    #[error("Token size is invalid {0}")]
    TokenSizeInvalid(Span),
    #[error("Token size need to be multiple of 8 {0}")]
    TokenSizeNonByte(Span),

    #[error("Field can't be attached {0}")]
    AttachInvalid(Span),

    #[error("Token Field size is invalid {0}")]
    TokenFieldSizeInvalid(Span),
    #[error("Token Field Attribute is declared mutliple times {0}")]
    TokenFieldAttDup(Span),
    #[error("Token Field attached mutliple times {0}")]
    TokenFieldAttachDup(Span),

    #[error("Varnode size is invalid {0}")]
    VarnodeInvalidSize(Span),
    #[error("Undefined varnode at {0}")]
    VarnodeUndefined(Span),
    #[error("Invalid varnode at {0}")]
    VarnodeInvalid(Span),

    #[error("Invalid statement at {0}")]
    UnableToParse(Span, nom::error::ErrorKind),
    #[error("Invalid statement at {0}")]
    StatementInvalid(Span),

    #[error("Undefined space at {0}")]
    SpaceUndefined(Span),
    #[error("Invalid space at {0}")]
    SpaceInvalid(Span),

    #[error("Context can't be attached to a number {0}")]
    ContextAttachNumber(Span),
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
    #[error("Table error at ({location}) Error: {error}")]
    Table { location: Span, error: TableError },
    #[error("Pcrode error at ({location}) Error: {error}")]
    PacodeMacro {
        location: Span,
        error: PcodeMacroError,
    },

    #[error("Invalid With Block")]
    WithBlock {
        location: Span,
        error: WithBlockError,
    },
}

#[derive(Clone, Debug, Error)]
pub enum TableError {
    #[error("Table Constructor can't be inserted in invalid Table name")]
    TableNameInvalid,

    #[error("Table Constructor have invalid Export size")]
    TableConstructorExportSizeInvalid,

    #[error("Pattern Error: {0}")]
    Pattern(#[from] PatternError),
    #[error("Disassembly Error: {0}")]
    Disassembly(#[from] DisassemblyError),
    #[error("Display Error: {0}")]
    Display(#[from] DisplayError),
    #[error("Execution Error: {0}")]
    Execution(#[from] ExecutionError),
}

#[derive(Clone, Debug, Error)]
pub enum PatternError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),
    #[error("Unable to merge Blocks mixing & and | {0}")]
    InvalidMixOp(Span),

    #[error("Invalid Recursive at {0}")]
    InvalidRecursive(Span),
    #[error("In Or block, all elements need to have the same len")]
    InvalidOrLen(Span),
    #[error("Each patern can only have a single recursive")]
    MultipleRecursives(Span),
    #[error("Mix `|` and `&` operations on pattern is forbidden")]
    MixOperations(Span),
    #[error("Field produced multiple times at {0} and {1}")]
    MultipleProduction(Span, Span),
    #[error("Field produced is implicit and abiguous")]
    AmbiguousProduction(Span),

    #[error("Invalid assignment Error")]
    ConstraintExpr(#[from] DisassemblyError),
}

#[derive(Clone, Debug, Error)]
pub enum DisassemblyError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),

    #[error("GlobalSet Address Ref missing {0}")]
    GlobalsetAddressMissing(Span),
    #[error("GlobalSet Address Ref invalid {0}")]
    GlobalsetAddressInvalid(Span),

    #[error("GlobalSet Address Ref not a context {0}")]
    GlobalsetAddressNotContext(Span),
}

#[derive(Clone, Debug, Error)]
pub enum DisplayError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),
}

#[derive(Clone, Debug, Error)]
pub enum ExecutionError {
    #[error("Invalid Ref: {0}")]
    InvalidRef(Span),
    #[error("Missing Ref: {0}")]
    MissingRef(Span),

    //TODO migrate this to PcodeMacro
    #[error("Macro don't allow Build statements")]
    MacroBuildInvalid,

    #[error("Invalid Var declaration {0}")]
    InvalidVarDeclare(Span),
    #[error("Invalid Var Len {0}")]
    InvalidVarLen(Span),
    #[error("Label invalid {0}")]
    InvalidLabel(Span),
    #[error("Label not found {0}")]
    MissingLabel(Span),
    #[error("Label Duplicated {0}")]
    DuplicatedLabel(Span),

    #[error("Default address space not found")]
    DefaultSpace, //TODO src
    #[error("Can only `goto` to a label")]
    InvalidLocalGoto, //TODO src
    #[error("TableConstructor have diferent return types")]
    InvalidExport,
    #[error("BitRange can't be zero")]
    BitRangeZero, //TODO src

    #[error("Can't apply op to variable due to size at {0}")]
    VarSize(Span), //TODO sub-type error

    #[error("Call user Function with invalid param numbers {0}")]
    UserFunctionParamNumber(Span),
    #[error("Call user Function with invalid return Size {0}")]
    UserFunctionReturnSize(Span),

    //TODO remove this
    #[error("Invalid amb1: {0}")]
    InvalidAmb1(Span),
}

#[derive(Clone, Debug, Error)]
pub enum PcodeMacroError {
    #[error("Invalid Ref {0}")]
    InvalidRef(Span),
    #[error("Missing Ref {0}")]
    MissingRef(Span),

    #[error("Invalid param size")]
    InvalidSpecialization(Span),

    #[error("Execution Error")]
    Execution(#[from] ExecutionError),
}

#[derive(Clone, Debug, Error)]
pub enum WithBlockError {
    #[error("Table name Error")]
    TableName,
    #[error("Pattern Error")]
    Pattern(#[from] PatternError),
    #[error("Disassembly Error")]
    Disassembly(#[from] DisassemblyError),
}

impl SleighError {
    pub fn new_table<E: Into<TableError>>(location: Span, error: E) -> Self {
        let error: TableError = error.into();
        Self::Table { location, error }
    }
    pub fn new_pcode_macro<E: Into<PcodeMacroError>>(
        location: Span,
        error: E,
    ) -> Self {
        let error: PcodeMacroError = error.into();
        Self::PacodeMacro { location, error }
    }
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
            self.start.file.to_string_lossy(),
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
            self.file.to_string_lossy(),
            self.line + 1,
            self.column + 1
        )
    }
}

/// bit in Little endian constraint
/// in 4 bytes, bit 0 => bit 0, bit 1 => bit 1, bit 8 => bit 8
fn field_in_le(value_bit: usize, _field_bits: usize) -> usize {
    value_bit
}

/// bit in Big endian constraint
/// in 4 bytes, bit 0 => bit 8, bit 1 => bit 9, bit 8 => bit 0
/// AKA invert the byte order, but bit order is bit 0 => lsb, bit N => msb
fn field_in_be(value_bit: usize, field_bits: usize) -> usize {
    let field_bytes = (field_bits + 7) / 8;
    let value_byte = value_bit / 8;
    let bit_in_byte = value_bit % 8;
    (((field_bytes - 1) - value_byte) * 8) + bit_in_byte
}

fn value_in_token(value_bit: usize, _field_bits: usize) -> usize {
    value_bit
}

// bit in context constraint
// in context, the byte order don't mater, but the bit order is bit 0 => msb
// bit N => lsb.
/// in 4 bytes, bit 0 => bit 31, bit 1 => bit 30, bit 8 => bit 23
fn value_in_context(value_bit: usize, field_bits: usize) -> usize {
    (field_bits - 1) - value_bit
}

pub fn file_to_sleigh(filename: &Path) -> Result<Sleigh, SleighError> {
    let mut pro = FilePreProcessor::new(&filename)?;
    let mut buf = vec![];
    let syntax = crate::syntax::Sleigh::parse(&mut pro, &mut buf, false)?;
    let sleigh = crate::semantic::Sleigh::new(syntax)?;
    Ok(sleigh)
}
