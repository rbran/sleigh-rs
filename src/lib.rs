use std::ops::{Bound, RangeBounds};
use std::path::Path;
#[cfg(not(feature = "thread"))]
use std::rc::Rc;
#[cfg(feature = "thread")]
use std::sync::Arc as Rc;

use thiserror::Error;

pub(crate) mod preprocessor;
pub(crate) mod semantic;
pub(crate) mod syntax;

use preprocessor::FilePreProcessor;
use preprocessor::PreprocessorError;

use syntax::{BitRangeLsbLen, BitRangeLsbMsb};

pub use semantic::inner::execution::FieldSize;

pub use semantic::disassembly;
pub use semantic::display;
pub use semantic::meaning;
pub use semantic::pattern;
pub use semantic::space;
pub use semantic::table;
pub use semantic::token;
pub use semantic::user_function;
pub use semantic::varnode;
pub use semantic::{
    AttachLiteralId, AttachNumberId, AttachVarnodeId, BitrangeId, ContextId,
    GlobalScope, PrintBase, Sleigh, SpaceId, TableId, TokenFieldId, TokenId,
    UserFunctionId, ValueFmt, VarnodeId,
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
pub(crate) const DISABLE_EXECUTION_PARSING: bool = false;

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
    PreProcessor(PreprocessorError),

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

    #[error("Unable to solve table (\n{0})")]
    TableUnsolvable(String),

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

impl nom::error::ParseError<&[preprocessor::token::Token]>
    for Box<SleighError>
{
    fn from_error_kind(
        input: &[preprocessor::token::Token],
        kind: nom::error::ErrorKind,
    ) -> Self {
        Box::new(SleighError::from_error_kind(input, kind))
    }

    fn append(
        input: &[preprocessor::token::Token],
        kind: nom::error::ErrorKind,
        other: Self,
    ) -> Self {
        Box::new(SleighError::append(input, kind, *other))
    }
}

impl From<Box<PreprocessorError>> for Box<SleighError> {
    fn from(value: Box<PreprocessorError>) -> Self {
        Box::new(SleighError::PreProcessor(*value))
    }
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

impl From<Box<PatternError>> for Box<TableError> {
    fn from(value: Box<PatternError>) -> Self {
        Box::new(TableError::Pattern(*value))
    }
}
impl From<Box<DisassemblyError>> for Box<TableError> {
    fn from(value: Box<DisassemblyError>) -> Self {
        Box::new(TableError::Disassembly(*value))
    }
}
impl From<Box<DisplayError>> for Box<TableError> {
    fn from(value: Box<DisplayError>) -> Self {
        Box::new(TableError::Display(*value))
    }
}
impl From<Box<ExecutionError>> for Box<TableError> {
    fn from(value: Box<ExecutionError>) -> Self {
        Box::new(TableError::Execution(*value))
    }
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
    #[error("Pattern in Or statement without constraint")]
    UnrestrictedOr(Span),

    #[error("Invalid assignment Error")]
    ConstraintExpr(#[from] DisassemblyError),
}
impl From<Box<DisassemblyError>> for Box<PatternError> {
    fn from(value: Box<DisassemblyError>) -> Self {
        Box::new(PatternError::ConstraintExpr(*value))
    }
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
    VarSize(#[from] VarSizeError), //TODO sub-type error

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
impl From<Box<ExecutionError>> for Box<PcodeMacroError> {
    fn from(value: Box<ExecutionError>) -> Self {
        Box::new(PcodeMacroError::Execution(*value))
    }
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

#[derive(Clone, Debug, Error)]
pub enum VarSizeError {
    #[error("Assignment Left {left:?} and Right {right:?} side are imcompatible at {location}")]
    AssignmentSides {
        left: FieldSize,
        right: FieldSize,
        location: Span,
    },
    #[error("Take {lsb}bytes from value of size {input:?} at {location}")]
    TakeLsbTooSmall {
        lsb: NumberNonZeroUnsigned,
        input: FieldSize,
        location: Span,
    },
    #[error("Trunk {lsb}bytes from value of size {input:?} at {location}")]
    TrunkLsbTooSmall {
        lsb: u64,
        output: FieldSize,
        input: FieldSize,
        location: Span,
    },
    #[error("BitRange is too big with {bits}bits but expecting {output:?} at {location}")]
    BitRangeTooBig {
        bits: NumberNonZeroUnsigned,
        output: FieldSize,
        location: Span,
    },
    #[error(
        "BitRange input is too small {input:?} taking {bits}bits at {location}"
    )]
    BitRangeInputSmall {
        bits: NumberNonZeroUnsigned,
        input: FieldSize,
        location: Span,
    },
    #[error("Unary Operator with input {input:?} and output {output:?} with diff size at {location}")]
    UnaryOpDiffSize {
        location: Span,
        input: FieldSize,
        output: FieldSize,
    },
    #[error("BitCount produces at least {bits} bits, but outputs {output:?} at {location}")]
    BitCountInvalidOutput {
        bits: NumberNonZeroUnsigned,
        output: FieldSize,
        location: Span,
    },
    #[error("Ext input {input:?} actually shrink to {output:?} at {location}")]
    ExtShrink {
        input: FieldSize,
        output: FieldSize,
        location: Span,
    },
    #[error(
        "Shift Left {left:?} and output {output:?} are not equal at {location}"
    )]
    ShiftLeftOutputDiff {
        left: FieldSize,
        output: FieldSize,
        location: Span,
    },
    #[error("BinaryOp have left {left:?} right {right:?} and output {output:?} with diferent size at {location}")]
    TriBinaryOp {
        left: FieldSize,
        right: FieldSize,
        output: FieldSize,
        location: Span,
    },
    #[error("BinaryOp with bool output left {left:?} right {right:?} with diferent size at {location}")]
    BoolBinaryOp {
        left: FieldSize,
        right: FieldSize,
        location: Span,
    },

    #[error("Address Size {address_size:?} bigger then Space bytes {space_bytes} at {location}")]
    AddressTooBig {
        address_size: FieldSize,
        space_bytes: NumberNonZeroUnsigned,
        location: Span,
    },

    #[error("Param Should be {param:?} but is {input:?} at {location}")]
    MacroParamWrongSize {
        param: FieldSize,
        input: FieldSize,
        location: Span,
    },
}

impl From<Box<VarSizeError>> for Box<ExecutionError> {
    fn from(value: Box<VarSizeError>) -> Self {
        Box::new(ExecutionError::VarSize(*value))
    }
}

impl From<VarSizeError> for Box<ExecutionError> {
    fn from(value: VarSizeError) -> Self {
        Box::new(ExecutionError::VarSize(value))
    }
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
    pub fn end_span(&self, end_line: u64, end_column: u64) -> Span {
        match self {
            Location::Macro(x) => Span::Macro(x.end_span(end_line, end_column)),
            Location::File(x) => Span::File(x.end_span(end_line, end_column)),
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
    fn end_span(&self, end_line: u64, end_column: u64) -> MacroSpan {
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
    pub(crate) fn new_start<A: AsRef<Path>>(path: A) -> Self {
        FileLocation {
            file: Rc::from(path.as_ref()),
            line: 0,
            column: 0,
        }
    }
    fn end_span(&self, end_line: u64, end_column: u64) -> FileSpan {
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

pub fn file_to_sleigh(filename: &Path) -> Result<Sleigh, Box<SleighError>> {
    let mut pro = FilePreProcessor::new(filename)?;
    tracing::trace!("preprocessor finished sucessfully");
    let mut buf = vec![];
    let syntax = crate::syntax::Sleigh::parse(&mut pro, &mut buf, false)?;
    tracing::trace!("parser finished sucessfully");
    let sleigh = crate::semantic::Sleigh::new(syntax)?;
    tracing::trace!("semantic analyzes finished sucessfully");
    Ok(sleigh)
}

#[cfg(test)]
mod test {
    use std::path::Path;

    use crate::file_to_sleigh;

    #[test]
    fn parse_all() {
        const ARCHS: &[&str] = &[
            //"RISCV/data/languages/riscv.lp64d.slaspec",
            //"RISCV/data/languages/riscv.ilp32d.slaspec",
            //"DATA/data/languages/data-le-64.slaspec",
            //"DATA/data/languages/data-be-64.slaspec",
            //"V850/data/languages/V850.slaspec",
            //"68000/data/languages/68040.slaspec",
            //"68000/data/languages/68030.slaspec",
            //"68000/data/languages/coldfire.slaspec",
            //"68000/data/languages/68020.slaspec",
            //"SuperH4/data/languages/SuperH4_le.slaspec",
            //"SuperH4/data/languages/SuperH4_be.slaspec",
            //"6502/data/languages/6502.slaspec",
            //"6502/data/languages/65c02.slaspec",
            //"CR16/data/languages/CR16B.slaspec",
            //"CR16/data/languages/CR16C.slaspec",
            //"BPF/data/languages/BPF_le.slaspec",
            //"Z80/data/languages/z80.slaspec",
            //"Z80/data/languages/z180.slaspec",
            //"M8C/data/languages/m8c.slaspec",
            //"8051/data/languages/80390.slaspec",
            //"8051/data/languages/80251.slaspec",
            //"8051/data/languages/8051.slaspec",
            //"8051/data/languages/mx51.slaspec",
            //"PIC/data/languages/pic12c5xx.slaspec",
            //"PIC/data/languages/dsPIC30F.slaspec",
            //"PIC/data/languages/pic17c7xx.slaspec",
            //"PIC/data/languages/PIC24H.slaspec",
            //"PIC/data/languages/pic16c5x.slaspec",
            //"PIC/data/languages/dsPIC33E.slaspec",
            //"PIC/data/languages/pic16.slaspec",
            //"PIC/data/languages/dsPIC33C.slaspec",
            //"PIC/data/languages/PIC24E.slaspec",
            //"PIC/data/languages/PIC24F.slaspec",
            //"PIC/data/languages/dsPIC33F.slaspec",
            //"PIC/data/languages/pic18.slaspec",
            //"PIC/data/languages/pic16f.slaspec",
            //"HCS08/data/languages/HCS08.slaspec",
            //"HCS08/data/languages/HC08.slaspec",
            //"HCS08/data/languages/HC05.slaspec",
            //"eBPF/data/languages/eBPF_le.slaspec",
            //"AARCH64/data/languages/AARCH64.slaspec",
            //"AARCH64/data/languages/AARCH64BE.slaspec",
            //"AARCH64/data/languages/AARCH64_AppleSilicon.slaspec",
            "tricore/data/languages/tricore.slaspec",
            //"PA-RISC/data/languages/pa-risc32be.slaspec",
            //"MC6800/data/languages/6809.slaspec",
            //"MC6800/data/languages/6805.slaspec",
            //"MC6800/data/languages/H6309.slaspec",
            //"TI_MSP430/data/languages/TI_MSP430X.slaspec",
            //"TI_MSP430/data/languages/TI_MSP430.slaspec",
            //"PowerPC/data/languages/ppc_32_quicciii_le.slaspec",
            //"PowerPC/data/languages/ppc_64_isa_be.slaspec",
            //"PowerPC/data/languages/ppc_64_isa_altivec_vle_be.slaspec",
            //"PowerPC/data/languages/ppc_32_e500_be.slaspec",
            //"PowerPC/data/languages/ppc_64_isa_altivec_be.slaspec",
            //"PowerPC/data/languages/ppc_32_be.slaspec",
            //"PowerPC/data/languages/ppc_64_be.slaspec",
            //"PowerPC/data/languages/ppc_32_4xx_le.slaspec",
            //"PowerPC/data/languages/ppc_32_quicciii_be.slaspec",
            //"PowerPC/data/languages/ppc_32_4xx_be.slaspec",
            //"PowerPC/data/languages/ppc_64_isa_altivec_le.slaspec",
            //"PowerPC/data/languages/ppc_32_le.slaspec",
            //"PowerPC/data/languages/ppc_64_isa_le.slaspec",
            //"PowerPC/data/languages/ppc_64_le.slaspec",
            //"PowerPC/data/languages/ppc_32_e500_le.slaspec",
            //"PowerPC/data/languages/ppc_64_isa_vle_be.slaspec",
            //"MIPS/data/languages/mips32R6be.slaspec",
            //"MIPS/data/languages/mips64be.slaspec",
            //"MIPS/data/languages/mips32R6le.slaspec",
            //"MIPS/data/languages/mips32le.slaspec",
            //"MIPS/data/languages/mips64le.slaspec",
            //"MIPS/data/languages/mips32be.slaspec",
            //"Atmel/data/languages/avr32a.slaspec",
            //"Atmel/data/languages/avr8.slaspec",
            //"Atmel/data/languages/avr8xmega.slaspec",
            //"Atmel/data/languages/avr8e.slaspec",
            //"Atmel/data/languages/avr8eind.slaspec",
            //"x86/data/languages/x86.slaspec",
            //"x86/data/languages/x86-64.slaspec",
            //"CP1600/data/languages/CP1600.slaspec",
            //"SuperH/data/languages/sh-2.slaspec",
            //"SuperH/data/languages/sh-2a.slaspec",
            //"SuperH/data/languages/sh-1.slaspec",
            //"Sparc/data/languages/SparcV9_64.slaspec",
            //"Sparc/data/languages/SparcV9_32.slaspec",
            //"MCS96/data/languages/MCS96.slaspec",
            //"Toy/data/languages/toy64_le.slaspec",
            //"Toy/data/languages/toy_builder_be_align2.slaspec",
            //"Toy/data/languages/toy_be_posStack.slaspec",
            //"Toy/data/languages/toy_builder_be.slaspec",
            //"Toy/data/languages/toy_wsz_be.slaspec",
            //"Toy/data/languages/toy_le.slaspec",
            //"Toy/data/languages/toy64_be_harvard.slaspec",
            //"Toy/data/languages/toy64_be.slaspec",
            //"Toy/data/languages/toy_wsz_le.slaspec",
            //"Toy/data/languages/toy_builder_le.slaspec",
            //"Toy/data/languages/toy_be.slaspec",
            //"Toy/data/languages/toy_builder_le_align2.slaspec",
            //"ARM/data/languages/ARM4t_le.slaspec",
            //"ARM/data/languages/ARM4_le.slaspec",
            //"ARM/data/languages/ARM7_be.slaspec",
            //"ARM/data/languages/ARM6_be.slaspec",
            //"ARM/data/languages/ARM5t_le.slaspec",
            //"ARM/data/languages/ARM5_le.slaspec",
            //"ARM/data/languages/ARM4_be.slaspec",
            //"ARM/data/languages/ARM5_be.slaspec",
            //"ARM/data/languages/ARM4t_be.slaspec",
            //"ARM/data/languages/ARM7_le.slaspec",
            //"ARM/data/languages/ARM6_le.slaspec",
            //"ARM/data/languages/ARM5t_be.slaspec",
            //"ARM/data/languages/ARM8_le.slaspec",
            //"ARM/data/languages/ARM8_be.slaspec",
            //"8085/data/languages/8085.slaspec",
            //"HCS12/data/languages/HCS12X.slaspec",
            //"HCS12/data/languages/HC12.slaspec",
            //"HCS12/data/languages/HCS12.slaspec",
            //"8048/data/languages/8048.slaspec",
            // TODO: cpool
            //"JVM/data/languages/JVM.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_Oreo.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_Android10.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_Marshmallow.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_Pie.slaspec",
            //"Dalvik/data/languages/Dalvik_ODEX_KitKat.slaspec",
            //"Dalvik/data/languages/Dalvik_Base.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_KitKat.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_Android11.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_Nougat.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_Android12.slaspec",
            //"Dalvik/data/languages/Dalvik_DEX_Lollipop.slaspec",
        ];
        const SLEIGH_PROCESSOR_PATH: &str = "Ghidra/Processors";
        let home = std::env::var("GHIDRA_SRC")
            .expect("Enviroment variable GHIDRA_SRC not found");
        for arch in ARCHS {
            let file = format!("{home}/{SLEIGH_PROCESSOR_PATH}/{arch}");
            let path = Path::new(&file);
            println!(
                "parsing: {}",
                path.file_name().unwrap().to_str().unwrap()
            );

            if let Err(err) = file_to_sleigh(path) {
                panic!("Unable to parse: {err}");
            } else {
                println!("Success");
            }
        }
    }
}
