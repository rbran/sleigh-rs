use std::ops::{Bound, RangeBounds};
use std::path::Path;
use std::rc::Rc;

mod preprocessor;

pub(crate) mod semantic;
pub(crate) mod syntax;

use preprocessor::FilePreProcessor;
pub use preprocessor::PreprocessorError;

use thiserror::Error;

pub type FloatType = f64;
pub type NumberUnsigned = u64;
pub type NumberSigned = i64;
pub type NumberSuperSigned = i128;
pub type NumberNonZeroUnsigned = std::num::NonZeroU64;
pub type NumberNonZeroSigned = std::num::NonZeroI64;
pub type NumberNonZeroSuperSigned = std::num::NonZeroI128;

//old naming convention
pub type IntTypeU = NumberUnsigned;
pub type IntTypeS = NumberSigned;
pub type NonZeroTypeU = NumberNonZeroUnsigned;
pub type NonZeroTypeS = NumberNonZeroSigned;

pub use semantic::meaning::Meaning;
pub use semantic::pattern::{Block, Pattern, PatternLen};
pub use semantic::pcode_macro::PcodeMacro;
pub use semantic::space::Space;
pub use semantic::table::{Constructor, Table};
pub use semantic::token::{Token, TokenField};
pub use semantic::user_function::UserFunction;
pub use semantic::varnode::{Bitrange, Context, Varnode};
pub use semantic::*;

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

    pub(crate) fn as_unsigned(&self) -> u64 {
        match self {
            Number::Positive(value) => *value,
            Number::Negative(value) => *value as u64,
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
            Self::Negative(value.abs() as u64)
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct RangeBits {
    pub lsb_bit: NumberUnsigned,
    pub n_bits: NumberNonZeroUnsigned,
}
impl RangeBits {
    pub fn new(lsb_bit: NumberUnsigned, n_bits: NumberNonZeroUnsigned) -> Self {
        Self { lsb_bit, n_bits }
    }
    pub fn len_bits(&self) -> NumberNonZeroUnsigned {
        self.n_bits
    }
    pub(crate) fn from_syntax(input: syntax::BitRange) -> Option<Self> {
        let lsb_bit = input.lsb_bit;
        let n_bits = NumberNonZeroUnsigned::new(input.n_bits)?;
        Some(Self { lsb_bit, n_bits })
    }
}
impl From<RangeBits> for std::ops::Range<NumberUnsigned> {
    fn from(input: RangeBits) -> Self {
        input.lsb_bit..(input.lsb_bit + input.n_bits.get())
    }
}
impl IntoIterator for RangeBits {
    type Item = NumberUnsigned;
    type IntoIter = <std::ops::Range<Self::Item> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        let range: std::ops::Range<Self::Item> = self.into();
        range.into_iter()
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

    //TODO delete this
    #[error("SemanticError {0}")]
    SemanticError(#[from] SemanticError)
}

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
    fn into_span(&self, end_line: u64, end_column: u64) -> Span {
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

#[cfg(test)]
mod test {
    use std::path::Path;

    use crate::preprocessor::FilePreProcessor;

    const GHIDRA_HOME: &str = "../ghidra/";

    fn parse_file(filename: &str) {
        let filename = Path::new(GHIDRA_HOME).join(filename);
        println!("file: {}", filename.to_string_lossy());
        let mut pro = FilePreProcessor::new(&filename).unwrap();
        let mut buf = vec![];
        let syntax =
            crate::syntax::Sleigh::parse(&mut pro, &mut buf, false).unwrap();
        let _sleigh = crate::semantic::Sleigh::new(syntax).unwrap();
    }

    macro_rules! test_file {
        ($fun_name:ident, $filename:literal) => {
            #[allow(non_snake_case)]
            #[test]
            fn $fun_name() {
                parse_file($filename)
            }
        };
    }

    //TODO What is `export 0:0;`?
    //test_file!(CPU_68000_68040, "Ghidra/Processors/68000/data/languages/68040.slaspec");
    //test_file!(CPU_68000_68030, "Ghidra/Processors/68000/data/languages/68030.slaspec");
    //test_file!(CPU_68000_coldfire, "Ghidra/Processors/68000/data/languages/coldfire.slaspec");
    //test_file!(CPU_68000_68020, "Ghidra/Processors/68000/data/languages/68020.slaspec");

    //TODO: https://github.com/NationalSecurityAgency/ghidra/pull/4016R
    test_file!(
        CPU_HCS12_HCS12,
        "Ghidra/Processors/HCS12/data/languages/HCS12.slaspec"
    );

    //TODO Varnode used in disassembly
    //test_file!(
    //    CPU_Atmel_avr32a,
    //    "Ghidra/Processors/Atmel/data/languages/avr32a.slaspec"
    //);
    //test_file!(
    //    CPU_Atmel_avr8xmega,
    //    "Ghidra/Processors/Atmel/data/languages/avr8xmega.slaspec"
    //);
    //test_file!(
    //    CPU_Atmel_avr8eind,
    //    "Ghidra/Processors/Atmel/data/languages/avr8eind.slaspec"
    //);
    //test_file!(
    //    CPU_8048_8048,
    //    "Ghidra/Processors/8048/data/languages/8048.slaspec"
    //);

    //TODO: sometimes the dst addr is 32, other time 64
    test_file!(
        CPU_PA_RISC_pa_risc32be,
        "Ghidra/Processors/PA-RISC/data/languages/pa-risc32be.slaspec"
    );

    //TODO: try to assign a 32bits value into a 64bits varnode
    test_file!(
        CPU_RISCV_riscv_ilp32d,
        "Ghidra/Processors/RISCV/data/languages/riscv.ilp32d.slaspec"
    );
    test_file!(
        CPU_RISCV_riscv_lp64d,
        "Ghidra/Processors/RISCV/data/languages/riscv.lp64d.slaspec"
    );

    //TODO: bitrange auto adapt to an arbitrary size
    //TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
    test_file!(
        CPU_V850_V850,
        "Ghidra/Processors/V850/data/languages/V850.slaspec"
    );
    test_file!(
        CPU_6502_6502,
        "Ghidra/Processors/6502/data/languages/6502.slaspec"
    );
    test_file!(
        CPU_6502_65c02,
        "Ghidra/Processors/6502/data/languages/65c02.slaspec"
    );
    test_file!(
        CPU_CR16_CR16B,
        "Ghidra/Processors/CR16/data/languages/CR16B.slaspec"
    );
    test_file!(
        CPU_CR16_CR16C,
        "Ghidra/Processors/CR16/data/languages/CR16C.slaspec"
    );
    test_file!(
        CPU_Z80_z80,
        "Ghidra/Processors/Z80/data/languages/z80.slaspec"
    );
    test_file!(
        CPU_Z80_z180,
        "Ghidra/Processors/Z80/data/languages/z180.slaspec"
    );
    test_file!(
        CPU_HCS08_HC08,
        "Ghidra/Processors/HCS08/data/languages/HC08.slaspec"
    );
    test_file!(
        CPU_HCS08_HCS08,
        "Ghidra/Processors/HCS08/data/languages/HCS08.slaspec"
    );
    test_file!(
        CPU_HCS08_HC05,
        "Ghidra/Processors/HCS08/data/languages/HC05.slaspec"
    );
    test_file!(
        CPU_tricore_tricore,
        "Ghidra/Processors/tricore/data/languages/tricore.slaspec"
    );
    test_file!(
        CPU_MC6800_6809,
        "Ghidra/Processors/MC6800/data/languages/6809.slaspec"
    );
    test_file!(
        CPU_MC6800_6805,
        "Ghidra/Processors/MC6800/data/languages/6805.slaspec"
    );
    test_file!(
        CPU_MC6800_H6309,
        "Ghidra/Processors/MC6800/data/languages/H6309.slaspec"
    );
    test_file!(
        CPU_MCS96_MCS96,
        "Ghidra/Processors/MCS96/data/languages/MCS96.slaspec"
    );

    //TODO Unrrestricted token_field in pattern or block, is (like in C)
    //an implicit `!= 0`?
    //test_file!(
    //    CPU_TI_MSP430_TI_MSP430,
    //    "Ghidra/Processors/TI_MSP430/data/languages/TI_MSP430.slaspec"
    //);
    //test_file!(
    //    CPU_TI_MSP430_TI_MSP430X,
    //    "Ghidra/Processors/TI_MSP430/data/languages/TI_MSP430X.slaspec"
    //);
    test_file!(
        CPU_Atmel_avr8,
        "Ghidra/Processors/Atmel/data/languages/avr8.slaspec"
    );
    test_file!(
        CPU_Atmel_avr8e,
        "Ghidra/Processors/Atmel/data/languages/avr8e.slaspec"
    );
    test_file!(
        CPU_CP1600_CP1600,
        "Ghidra/Processors/CP1600/data/languages/CP1600.slaspec"
    );
    test_file!(
        CPU_SuperH_sh_1,
        "Ghidra/Processors/SuperH/data/languages/sh-1.slaspec"
    );
    test_file!(
        CPU_SuperH_sh_2,
        "Ghidra/Processors/SuperH/data/languages/sh-2.slaspec"
    );
    test_file!(
        CPU_SuperH_sh_2a,
        "Ghidra/Processors/SuperH/data/languages/sh-2a.slaspec"
    );
    test_file!(
        CPU_M8C_m8c,
        "Ghidra/Processors/M8C/data/languages/m8c.slaspec"
    );
    test_file!(
        CPU_8051_80251,
        "Ghidra/Processors/8051/data/languages/80251.slaspec"
    );
    test_file!(
        CPU_8051_80390,
        "Ghidra/Processors/8051/data/languages/80390.slaspec"
    );
    test_file!(
        CPU_8051_8051,
        "Ghidra/Processors/8051/data/languages/8051.slaspec"
    );
    test_file!(
        CPU_8051_mx51,
        "Ghidra/Processors/8051/data/languages/mx51.slaspec"
    );
    test_file!(
        CPU_PIC_pic16,
        "Ghidra/Processors/PIC/data/languages/pic16.slaspec"
    );
    test_file!(
        CPU_PIC_pic16f,
        "Ghidra/Processors/PIC/data/languages/pic16f.slaspec"
    );
    test_file!(
        CPU_PIC_pic17c7xx,
        "Ghidra/Processors/PIC/data/languages/pic17c7xx.slaspec"
    );
    test_file!(
        CPU_PIC_pic18,
        "Ghidra/Processors/PIC/data/languages/pic18.slaspec"
    );
    test_file!(
        CPU_PIC_PIC24E,
        "Ghidra/Processors/PIC/data/languages/PIC24E.slaspec"
    );
    test_file!(
        CPU_PIC_PIC24F,
        "Ghidra/Processors/PIC/data/languages/PIC24F.slaspec"
    );
    test_file!(
        CPU_PIC_PIC24H,
        "Ghidra/Processors/PIC/data/languages/PIC24H.slaspec"
    );
    test_file!(
        CPU_PIC_dsPIC30F,
        "Ghidra/Processors/PIC/data/languages/dsPIC30F.slaspec"
    );
    test_file!(
        CPU_PIC_dsPIC33C,
        "Ghidra/Processors/PIC/data/languages/dsPIC33C.slaspec"
    );
    test_file!(
        CPU_PIC_dsPIC33E,
        "Ghidra/Processors/PIC/data/languages/dsPIC33E.slaspec"
    );
    test_file!(
        CPU_PIC_dsPIC33F,
        "Ghidra/Processors/PIC/data/languages/dsPIC33F.slaspec"
    );

    //TODO: use value from non export table
    test_file!(
        CPU_MIPS_mips32be,
        "Ghidra/Processors/MIPS/data/languages/mips32be.slaspec"
    );
    test_file!(
        CPU_MIPS_mips32le,
        "Ghidra/Processors/MIPS/data/languages/mips32le.slaspec"
    );
    test_file!(
        CPU_MIPS_mips32R6be,
        "Ghidra/Processors/MIPS/data/languages/mips32R6be.slaspec"
    );
    test_file!(
        CPU_MIPS_mips32R6le,
        "Ghidra/Processors/MIPS/data/languages/mips32R6le.slaspec"
    );
    test_file!(
        CPU_MIPS_mips64be,
        "Ghidra/Processors/MIPS/data/languages/mips64be.slaspec"
    );
    test_file!(
        CPU_MIPS_mips64le,
        "Ghidra/Processors/MIPS/data/languages/mips64le.slaspec"
    );

    //TODO: re-export from a table that also export const
    test_file!(
        CPU_AARCH64_AARCH64,
        "Ghidra/Processors/AARCH64/data/languages/AARCH64.slaspec"
    );
    test_file!(
        CPU_AARCH64_AARCH64BE,
        "Ghidra/Processors/AARCH64/data/languages/AARCH64BE.slaspec"
    );
    test_file!(
        CPU_AARCH64_AARCH64_AppleSilicon,
        "Ghidra/Processors/AARCH64/data/languages/AARCH64_AppleSilicon.slaspec"
    );

    //TODO: Cpool
    test_file!(
        CPU_JVM_JVM,
        "Ghidra/Processors/JVM/data/languages/JVM.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_Base,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_Base.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_ODEX_KitKat,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_ODEX_KitKat.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_DEX_KitKat,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_KitKat.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_DEX_Lollipop,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Lollipop.slaspec"
    );
    test_file!(CPU_Dalvik_Dalvik_DEX_Marshmallow, "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Marshmallow.slaspec");
    test_file!(
        CPU_Dalvik_Dalvik_DEX_Nougat,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Nougat.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_DEX_Oreo,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Oreo.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_DEX_Pie,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Pie.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_DEX_Android10,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Android10.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_DEX_Android11,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Android11.slaspec"
    );
    test_file!(
        CPU_Dalvik_Dalvik_DEX_Android12,
        "Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Android12.slaspec"
    );

    //TODO: AND-OP a 64bit value with a 32bit variable, outputing a 32bit value
    test_file!(
        CPU_PowerPC_ppc_32_be,
        "Ghidra/Processors/PowerPC/data/languages/ppc_32_be.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_32_le,
        "Ghidra/Processors/PowerPC/data/languages/ppc_32_le.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_32_quicciii_be,
        "Ghidra/Processors/PowerPC/data/languages/ppc_32_quicciii_be.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_32_quicciii_le,
        "Ghidra/Processors/PowerPC/data/languages/ppc_32_quicciii_le.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_32_4xx_be,
        "Ghidra/Processors/PowerPC/data/languages/ppc_32_4xx_be.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_32_4xx_le,
        "Ghidra/Processors/PowerPC/data/languages/ppc_32_4xx_le.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_64_be,
        "Ghidra/Processors/PowerPC/data/languages/ppc_64_be.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_64_le,
        "Ghidra/Processors/PowerPC/data/languages/ppc_64_le.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_64_isa_be,
        "Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_be.slaspec"
    );
    test_file!(
        CPU_PowerPC_ppc_64_isa_le,
        "Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_le.slaspec"
    );
    test_file!(CPU_PowerPC_ppc_64_isa_altivec_be, "Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_altivec_be.slaspec");
    test_file!(CPU_PowerPC_ppc_64_isa_altivec_le, "Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_altivec_le.slaspec");
    test_file!(CPU_PowerPC_ppc_64_isa_altivec_vle_be, "Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_altivec_vle_be.slaspec");
    test_file!(
        CPU_PowerPC_ppc_64_isa_vle_be,
        "Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_vle_be.slaspec"
    );

    //TODO: Jmp into a 16bit address
    test_file!(
        CPU_x86_x86,
        "Ghidra/Processors/x86/data/languages/x86.slaspec"
    );
    test_file!(
        CPU_x86_x86_64,
        "Ghidra/Processors/x86/data/languages/x86-64.slaspec"
    );

    //TODO: jmp into 16/8bit address
    test_file!(
        CPU_8085_8085,
        "Ghidra/Processors/8085/data/languages/8085.slaspec"
    );

    //TODO: Op 32bits value with Int greater then 32bits
    test_file!(
        CPU_Sparc_SparcV9_32,
        "Ghidra/Processors/Sparc/data/languages/SparcV9_32.slaspec"
    );
    test_file!(
        CPU_Sparc_SparcV9_64,
        "Ghidra/Processors/Sparc/data/languages/SparcV9_64.slaspec"
    );

    test_file!(
        CPU_Toy_toy_builder_be_align2,
        "Ghidra/Processors/Toy/data/languages/toy_builder_be_align2.slaspec"
    );
    test_file!(
        CPU_Toy_toy_builder_le_align2,
        "Ghidra/Processors/Toy/data/languages/toy_builder_le_align2.slaspec"
    );
    test_file!(
        CPU_Toy_toy_builder_le,
        "Ghidra/Processors/Toy/data/languages/toy_builder_le.slaspec"
    );
    test_file!(
        CPU_Toy_toy_be_posStack,
        "Ghidra/Processors/Toy/data/languages/toy_be_posStack.slaspec"
    );
    test_file!(
        CPU_Toy_toy_builder_be,
        "Ghidra/Processors/Toy/data/languages/toy_builder_be.slaspec"
    );
    test_file!(
        CPU_Toy_toy_wsz_be,
        "Ghidra/Processors/Toy/data/languages/toy_wsz_be.slaspec"
    );
    test_file!(
        CPU_Toy_toy_wsz_le,
        "Ghidra/Processors/Toy/data/languages/toy_wsz_le.slaspec"
    );
    test_file!(
        CPU_Toy_toy_be,
        "Ghidra/Processors/Toy/data/languages/toy_be.slaspec"
    );
    test_file!(
        CPU_Toy_toy_le,
        "Ghidra/Processors/Toy/data/languages/toy_le.slaspec"
    );
    test_file!(
        CPU_Toy_toy64_be,
        "Ghidra/Processors/Toy/data/languages/toy64_be.slaspec"
    );
    test_file!(
        CPU_Toy_toy64_le,
        "Ghidra/Processors/Toy/data/languages/toy64_le.slaspec"
    );
    test_file!(
        CPU_Toy_toy64_be_harvard,
        "Ghidra/Processors/Toy/data/languages/toy64_be_harvard.slaspec"
    );

    test_file!(
        CPU_ARM_ARM4_be,
        "Ghidra/Processors/ARM/data/languages/ARM4_be.slaspec"
    );
    test_file!(
        CPU_ARM_ARM4_le,
        "Ghidra/Processors/ARM/data/languages/ARM4_le.slaspec"
    );
    test_file!(
        CPU_ARM_ARM4t_be,
        "Ghidra/Processors/ARM/data/languages/ARM4t_be.slaspec"
    );
    test_file!(
        CPU_ARM_ARM4t_le,
        "Ghidra/Processors/ARM/data/languages/ARM4t_le.slaspec"
    );
    test_file!(
        CPU_ARM_ARM5_be,
        "Ghidra/Processors/ARM/data/languages/ARM5_be.slaspec"
    );
    test_file!(
        CPU_ARM_ARM5_le,
        "Ghidra/Processors/ARM/data/languages/ARM5_le.slaspec"
    );
    test_file!(
        CPU_ARM_ARM5t_be,
        "Ghidra/Processors/ARM/data/languages/ARM5t_be.slaspec"
    );
    test_file!(
        CPU_ARM_ARM5t_le,
        "Ghidra/Processors/ARM/data/languages/ARM5t_le.slaspec"
    );
    test_file!(
        CPU_ARM_ARM6_be,
        "Ghidra/Processors/ARM/data/languages/ARM6_be.slaspec"
    );
    test_file!(
        CPU_ARM_ARM6_le,
        "Ghidra/Processors/ARM/data/languages/ARM6_le.slaspec"
    );
    test_file!(
        CPU_ARM_ARM7_be,
        "Ghidra/Processors/ARM/data/languages/ARM7_be.slaspec"
    );
    test_file!(
        CPU_ARM_ARM7_le,
        "Ghidra/Processors/ARM/data/languages/ARM7_le.slaspec"
    );
    test_file!(
        CPU_ARM_ARM8_be,
        "Ghidra/Processors/ARM/data/languages/ARM8_be.slaspec"
    );
    test_file!(
        CPU_ARM_ARM8_le,
        "Ghidra/Processors/ARM/data/languages/ARM8_le.slaspec"
    );

    test_file!(
        CPU_DATA_data_be_64,
        "Ghidra/Processors/DATA/data/languages/data-be-64.slaspec"
    );
    test_file!(
        CPU_DATA_data_le_64,
        "Ghidra/Processors/DATA/data/languages/data-le-64.slaspec"
    );

    test_file!(
        CPU_SuperH4_SuperH4_be,
        "Ghidra/Processors/SuperH4/data/languages/SuperH4_be.slaspec"
    );
    test_file!(
        CPU_SuperH4_SuperH4_le,
        "Ghidra/Processors/SuperH4/data/languages/SuperH4_le.slaspec"
    );

    test_file!(
        CPU_PIC_pic12c5xx,
        "Ghidra/Processors/PIC/data/languages/pic12c5xx.slaspec"
    );
    test_file!(
        CPU_PIC_pic16c5x,
        "Ghidra/Processors/PIC/data/languages/pic16c5x.slaspec"
    );
}
