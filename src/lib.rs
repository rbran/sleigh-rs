use std::ops::{Bound, RangeBounds};
use std::path::Path;
use std::rc::Rc;

mod base;

pub mod preprocessor;
pub mod semantic;
pub mod syntax;

use preprocessor::preprocess;

use syntax::parse_syntax;

pub use semantic::assembly::{Assembly, Token};
pub use semantic::disassembly::Disassembly;
pub use semantic::pattern::{Block, Pattern, SubBlock, SubPattern};
pub use semantic::pcode_macro::PcodeMacro;
pub use semantic::space::Space;
pub use semantic::table::{Constructor, Table};
pub use semantic::user_function::UserFunction;
pub use semantic::varnode::Varnode;
pub use semantic::Sleigh;

pub use base::{IntTypeS, IntTypeU, NonZeroTypeU};

pub const IDENT_INSTRUCTION: &str = "instruction";
pub const IDENT_INST_START: &str = "inst_start";
pub const IDENT_INST_NEXT: &str = "inst_next";
pub const IDENT_EPSILON: &str = "epsilon";
pub const IDENT_CONST: &str = "const";
pub const IDENT_UNIQUE: &str = "unique";

//TODO delete this for the love of askdjfhaçsdjkfh
pub static STATE: u32 = 0;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InputSource {
    pub line: u32,
    pub column: usize,
    pub file: Rc<Path>,
}

impl InputSource {
    pub fn new_from_offset(source: &str, offset: &str, file: Rc<Path>) -> Self {
        let offset = offset.as_ptr() as usize - source.as_ptr() as usize;
        let (pre, _pos) = source.split_at(offset);
        let mut line: u32 = 0;
        //let column = 1 + pre
        //    .lines()
        //    .inspect(|_| line += 1)
        //    .last()
        //    .map(|x| x.chars().count())
        //    .unwrap_or(0);
        let column = pre
            .split('\n')
            .inspect(|_| line += 1)
            .last()
            .map(|x| 1 + x.chars().count())
            .unwrap_or(1);
        line = line.saturating_sub(1);
        Self { line, column, file }
    }
}

impl std::fmt::Display for InputSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file.to_string_lossy(),
            self.line,
            self.column
        )
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BitRange {
    pub lsb_bit: IntTypeU,
    pub n_bits: NonZeroTypeU,
}
impl BitRange {
    pub fn new(lsb_bit: IntTypeU, n_bits: NonZeroTypeU) -> Self {
        Self { lsb_bit, n_bits }
    }
    pub fn size(&self) -> NonZeroTypeU {
        self.n_bits
    }
    pub fn from_syntax(input: syntax::BitRange) -> Option<Self> {
        let lsb_bit = input.lsb_bit;
        let n_bits = NonZeroTypeU::new(input.n_bits)?;
        Some(Self { lsb_bit, n_bits })
    }
}
impl From<BitRange> for std::ops::Range<IntTypeU> {
    fn from(input: BitRange) -> Self {
        input.lsb_bit..(input.lsb_bit + input.n_bits.get())
    }
}

//TODO impl Error here
pub fn file_to_sleigh(
    filename: &str,
) -> Result<semantic::Sleigh, Box<dyn std::error::Error>> {
    let file = Path::new(filename);
    let output = preprocess(file)?;
    let syntax = parse_syntax(&output)?;
    let sleigh = Sleigh::new(syntax, &output)?;
    Ok(sleigh)
}

#[cfg(test)]
mod test {
    use crate::file_to_sleigh;
    use crate::preprocessor::preprocess;
    use crate::syntax::parse_syntax;

    const ARCH_FILES: &[&str] = &[
        //TODO What is `export 0:0;`?
        //"68040.slaspec",
        //"68030.slaspec",
        //"coldfire.slaspec",
        //"68020.slaspec",

        //TODO: https://github.com/NationalSecurityAgency/ghidra/pull/4016R
        //"HCS12.slaspec",

        //TODO: disassembly pointing to non context varnode???
        //"avr32a.slaspec",
        //"avr8xmega.slaspec",
        //"avr8eind.slaspec",
        //"8048.slaspec",
        "pa-risc32be.slaspec",
        "riscv.ilp32d.slaspec",
        "riscv.lp64d.slaspec",
        "mips32be.slaspec",
        "mips32le.slaspec",
        "mips32R6be.slaspec",
        "mips32R6le.slaspec",
        "mips64be.slaspec",
        "mips64le.slaspec",
        "data-be-64.slaspec",
        "data-le-64.slaspec",
        "V850.slaspec",
        "SuperH4_be.slaspec",
        "SuperH4_le.slaspec",
        "6502.slaspec",
        "65c02.slaspec",
        "CR16B.slaspec",
        "CR16C.slaspec",
        "z80.slaspec",
        "z180.slaspec",
        "JVM.slaspec",
        "m8c.slaspec",
        "80251.slaspec",
        "80390.slaspec",
        "8051.slaspec",
        "mx51.slaspec",
        "pic12c5xx.slaspec",
        "pic16.slaspec",
        "pic16c5x.slaspec",
        "pic16f.slaspec",
        "pic17c7xx.slaspec",
        "pic18.slaspec",
        "PIC24E.slaspec",
        "PIC24F.slaspec",
        "PIC24H.slaspec",
        "dsPIC30F.slaspec",
        "dsPIC33C.slaspec",
        "dsPIC33E.slaspec",
        "dsPIC33F.slaspec",
        "HC08.slaspec",
        "HCS08.slaspec",
        "HC05.slaspec",
        "AARCH64.slaspec",
        "AARCH64BE.slaspec",
        "AARCH64_AppleSilicon.slaspec",
        "tricore.slaspec",
        "6809.slaspec",
        "6805.slaspec",
        "H6309.slaspec",
        "TI_MSP430.slaspec",
        "TI_MSP430X.slaspec",
        "ppc_32_be.slaspec",
        "ppc_32_le.slaspec",
        "ppc_32_quicciii_be.slaspec",
        "ppc_32_quicciii_le.slaspec",
        "ppc_32_4xx_be.slaspec",
        "ppc_32_4xx_le.slaspec",
        "ppc_64_be.slaspec",
        "ppc_64_le.slaspec",
        "ppc_64_isa_be.slaspec",
        "ppc_64_isa_le.slaspec",
        "ppc_64_isa_altivec_be.slaspec",
        "ppc_64_isa_altivec_le.slaspec",
        "ppc_64_isa_altivec_vle_be.slaspec",
        "ppc_64_isa_vle_be.slaspec",
        "avr8.slaspec",
        "avr8e.slaspec",
        "x86.slaspec",
        "x86-64.slaspec",
        "CP1600.slaspec",
        "sh-1.slaspec",
        "sh-2.slaspec",
        "sh-2a.slaspec",
        "SparcV9_32.slaspec",
        "SparcV9_64.slaspec",
        "MCS96.slaspec",
        "toy_builder_be_align2.slaspec",
        "toy_builder_le_align2.slaspec",
        "toy_builder_le.slaspec",
        "toy_be_posStack.slaspec",
        "toy_builder_be.slaspec",
        "toy_wsz_be.slaspec",
        "toy_wsz_le.slaspec",
        "toy_be.slaspec",
        "toy_le.slaspec",
        "toy64_be.slaspec",
        "toy64_le.slaspec",
        "toy64_be_harvard.slaspec",
        "ARM4_be.slaspec",
        "ARM4_le.slaspec",
        "ARM4t_be.slaspec",
        "ARM4t_le.slaspec",
        "ARM5_be.slaspec",
        "ARM5_le.slaspec",
        "ARM5t_be.slaspec",
        "ARM5t_le.slaspec",
        "ARM6_be.slaspec",
        "ARM6_le.slaspec",
        "ARM7_be.slaspec",
        "ARM7_le.slaspec",
        "ARM8_be.slaspec",
        "ARM8_le.slaspec",
        "8085.slaspec",
        "Dalvik_Base.slaspec",
        "Dalvik_ODEX_KitKat.slaspec",
        "Dalvik_DEX_KitKat.slaspec",
        "Dalvik_DEX_Lollipop.slaspec",
        "Dalvik_DEX_Marshmallow.slaspec",
        "Dalvik_DEX_Nougat.slaspec",
        "Dalvik_DEX_Oreo.slaspec",
        "Dalvik_DEX_Pie.slaspec",
        "Dalvik_DEX_Android10.slaspec",
        "Dalvik_DEX_Android11.slaspec",
        "Dalvik_DEX_Android12.slaspec",
    ];

    #[test]
    fn syntax() {
        for file in ARCH_FILES.iter() {
            let file = std::path::Path::new(file);
            let output = preprocess(file).unwrap();
            let _syntax = parse_syntax(&output).unwrap();
        }
    }

    #[test]
    fn semantic() {
        for file in ARCH_FILES.iter() {
            file_to_sleigh(file).unwrap();
        }
    }
}
