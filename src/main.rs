use sleigh_rs::file_to_sleigh;

const ARCH_FILES: &[&str] = &[
////TODO What is `export 0:0;`?
//"/home/rbran/src/ghidra/Ghidra/Processors/68000/data/languages/68040.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/68000/data/languages/68030.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/68000/data/languages/coldfire.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/68000/data/languages/68020.slaspec",
//
////TODO: https://github.com/NationalSecurityAgency/ghidra/pull/4016R
//"/home/rbran/src/ghidra/Ghidra/Processors/HCS12/data/languages/HCS12.slaspec",
//
////TODO: disassembly pointing to non context varnode???
//"/home/rbran/src/ghidra/Ghidra/Processors/Atmel/data/languages/avr32a.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Atmel/data/languages/avr8xmega.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Atmel/data/languages/avr8eind.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/8048/data/languages/8048.slaspec",
//
////TODO: sometimes the dst addr is 32, other time 64
//"/home/rbran/src/ghidra/Ghidra/Processors/PA-RISC/data/languages/pa-risc32be.slaspec",
//
////TODO: try to assign a 32bits value into a 64bits varnode
//"/home/rbran/src/ghidra/Ghidra/Processors/RISCV/data/languages/riscv.ilp32d.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/RISCV/data/languages/riscv.lp64d.slaspec",
//
////TODO: bitrange auto adapt to an arbitrary size
////TODO: Assign values with diferent sizes, eg 8bit value into 16bit variable
//"/home/rbran/src/ghidra/Ghidra/Processors/V850/data/languages/V850.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/6502/data/languages/6502.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/6502/data/languages/65c02.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/CR16/data/languages/CR16B.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/CR16/data/languages/CR16C.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Z80/data/languages/z80.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Z80/data/languages/z180.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/HCS08/data/languages/HC08.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/HCS08/data/languages/HCS08.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/HCS08/data/languages/HC05.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/tricore/data/languages/tricore.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MC6800/data/languages/6809.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MC6800/data/languages/6805.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MC6800/data/languages/H6309.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MCS96/data/languages/MCS96.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/TI_MSP430/data/languages/TI_MSP430.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/TI_MSP430/data/languages/TI_MSP430X.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Atmel/data/languages/avr8.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Atmel/data/languages/avr8e.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/CP1600/data/languages/CP1600.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/SuperH/data/languages/sh-1.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/SuperH/data/languages/sh-2.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/SuperH/data/languages/sh-2a.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/M8C/data/languages/m8c.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/8051/data/languages/80251.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/8051/data/languages/80390.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/8051/data/languages/8051.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/8051/data/languages/mx51.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/pic16.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/pic16f.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/pic17c7xx.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/pic18.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/PIC24E.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/PIC24F.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/PIC24H.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/dsPIC30F.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/dsPIC33C.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/dsPIC33E.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/dsPIC33F.slaspec",
//
////TODO: use value from non export table
//"/home/rbran/src/ghidra/Ghidra/Processors/MIPS/data/languages/mips32be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MIPS/data/languages/mips32le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MIPS/data/languages/mips32R6be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MIPS/data/languages/mips32R6le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MIPS/data/languages/mips64be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/MIPS/data/languages/mips64le.slaspec",
//
////TODO: re-export from a table that also export const
//"/home/rbran/src/ghidra/Ghidra/Processors/AARCH64/data/languages/AARCH64.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/AARCH64/data/languages/AARCH64BE.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/AARCH64/data/languages/AARCH64_AppleSilicon.slaspec",
//
////TODO: Cpool
//"/home/rbran/src/ghidra/Ghidra/Processors/JVM/data/languages/JVM.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_Base.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_ODEX_KitKat.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_KitKat.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Lollipop.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Marshmallow.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Nougat.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Oreo.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Pie.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Android10.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Android11.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Dalvik/data/languages/Dalvik_DEX_Android12.slaspec",
//
////TODO: AND-OP a 64bit value with a 32bit variable, outputing a 32bit value
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_32_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_32_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_32_quicciii_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_32_quicciii_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_32_4xx_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_32_4xx_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_64_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_64_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_altivec_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_altivec_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_altivec_vle_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/PowerPC/data/languages/ppc_64_isa_vle_be.slaspec",
//
////TODO: Jmp into a 16bit address
//"/home/rbran/src/ghidra/Ghidra/Processors/x86/data/languages/x86.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/x86/data/languages/x86-64.slaspec",
//
////TODO: jmp into 16/8bit address
//"/home/rbran/src/ghidra/Ghidra/Processors/8085/data/languages/8085.slaspec",
//
////TODO: Op 32bits value with Int greater then 32bits
//"/home/rbran/src/ghidra/Ghidra/Processors/Sparc/data/languages/SparcV9_32.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Sparc/data/languages/SparcV9_64.slaspec",
//
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_builder_be_align2.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_builder_le_align2.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_builder_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_be_posStack.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_builder_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_wsz_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_wsz_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy64_be.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy64_le.slaspec",
//"/home/rbran/src/ghidra/Ghidra/Processors/Toy/data/languages/toy64_be_harvard.slaspec",

"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM4_be.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM4_le.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM4t_be.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM4t_le.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM5_be.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM5_le.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM5t_be.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM5t_le.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM6_be.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM6_le.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM7_be.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM7_le.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM8_be.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/ARM/data/languages/ARM8_le.slaspec",

"/home/rbran/src/ghidra/Ghidra/Processors/DATA/data/languages/data-be-64.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/DATA/data/languages/data-le-64.slaspec",

"/home/rbran/src/ghidra/Ghidra/Processors/SuperH4/data/languages/SuperH4_be.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/SuperH4/data/languages/SuperH4_le.slaspec",

"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/pic12c5xx.slaspec",
"/home/rbran/src/ghidra/Ghidra/Processors/PIC/data/languages/pic16c5x.slaspec",

];

fn main() -> Result<(), Box<dyn std::error::Error>> {
    for file in ARCH_FILES.iter() {
        println!("file {}", file);
        let x = file_to_sleigh(file);
        if let Err(x) = &x {
            println!("{}", &x);
        }
        x?;
    }
    Ok(())
}
