use crate::{Number, NumberNonZeroUnsigned, NumberUnsigned, Span};

use super::{
    disassembly, pcode_macro::PcodeMacroCallId, BitrangeId, ContextId,
    InstNext, InstStart, SpaceId, TableId, TokenFieldId, UserFunctionId,
    VarnodeId,
};

#[derive(Clone, Debug)]
pub struct Execution {
    pub(crate) blocks: Box<[Block]>,
    pub(crate) variables: Box<[Variable]>,

    // TODO make this a const, the first block is always the entry block
    //entry_block have no name and is not on self.labels
    pub entry_block: BlockId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub(crate) usize);

#[derive(Clone, Debug)]
pub struct Block {
    //None is entry block
    pub name: Option<Box<str>>,
    pub next: Option<BlockId>,
    pub statements: Box<[Statement]>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Delayslot(NumberUnsigned),
    Export(Export),
    CpuBranch(CpuBranch),
    LocalGoto(LocalGoto),
    UserCall(UserCall),
    MacroCall(MacroCall),
    Build(Build),
    Declare(VariableId),
    Assignment(Assignment),
    MemWrite(MemWrite),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VariableId(pub(crate) usize);

#[derive(Clone, Debug)]
pub struct Variable {
    pub(crate) name: Box<str>,
    pub len_bits: NumberNonZeroUnsigned,
    pub location: Option<Span>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Value(ExprElement),
    Op(ExprBinaryOp),
}

#[derive(Clone, Debug)]
pub struct ExprBinaryOp {
    pub location: Span,
    pub len_bits: NumberNonZeroUnsigned,
    pub op: Binary,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum ExprElement {
    Value(ExprValue),
    UserCall(UserCall),
    Reference(Reference),
    Op(ExprUnaryOp),
    New(ExprNew),
    CPool(ExprCPool),
}

#[derive(Clone, Debug)]
pub struct Reference {
    pub location: Span,
    pub len_bits: NumberNonZeroUnsigned,
    pub value: ReferencedValue,
}

#[derive(Clone, Debug)]
pub struct ExprUnaryOp {
    pub location: Span,
    pub output_bits: NumberNonZeroUnsigned,
    pub op: Unary,
    pub input: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprNew {
    pub location: Span,
    pub first: Box<Expr>,
    pub second: Option<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct ExprCPool {
    pub location: Span,
    pub params: Box<[Expr]>,
}

#[derive(Clone, Debug)]
pub struct UserCall {
    pub location: Span,
    pub function: UserFunctionId,
    pub params: Box<[Expr]>,
}

#[derive(Clone, Debug)]
pub enum ExprValue {
    Int(ExprNumber),
    TokenField(ExprTokenField),
    InstStart(ExprInstStart),
    InstNext(ExprInstNext),
    Varnode(ExprVarnode),
    Context(ExprContext),
    Bitrange(ExprBitrange),
    Table(ExprTable),
    DisVar(ExprDisVar),
    ExeVar(ExprExeVar),
}

#[derive(Clone, Debug)]
pub struct ExprNumber {
    pub location: Span,
    pub size: NumberNonZeroUnsigned,
    pub number: Number,
}

#[derive(Clone, Debug)]
pub struct ExprTokenField {
    pub location: Span,
    pub size: NumberNonZeroUnsigned,
    pub id: TokenFieldId,
}

#[derive(Clone, Debug)]
pub struct ExprInstStart {
    pub location: Span,
    pub data: InstStart,
}

#[derive(Clone, Debug)]
pub struct ExprInstNext {
    pub location: Span,
    pub data: InstNext,
}

#[derive(Clone, Debug)]
pub struct ExprVarnode {
    pub location: Span,
    pub id: VarnodeId,
}

#[derive(Clone, Debug)]
pub struct ExprContext {
    pub location: Span,
    pub size: NumberNonZeroUnsigned,
    pub id: ContextId,
}

#[derive(Clone, Debug)]
pub struct ExprBitrange {
    pub location: Span,
    pub size: NumberNonZeroUnsigned,
    pub id: BitrangeId,
}

#[derive(Clone, Debug)]
pub struct ExprTable {
    pub location: Span,
    pub id: TableId,
}

#[derive(Clone, Debug)]
pub struct ExprDisVar {
    pub location: Span,
    pub size: NumberNonZeroUnsigned,
    pub id: disassembly::VariableId,
}

#[derive(Clone, Debug)]
pub struct ExprExeVar {
    pub location: Span,
    pub id: VariableId,
}

#[derive(Clone, Debug)]
pub enum ReferencedValue {
    //only if translate into varnode
    TokenField(RefTokenField),
    InstStart(RefInstStart),
    InstNext(RefInstNext),
    Table(RefTable),
}

#[derive(Clone, Debug)]
pub struct RefTokenField {
    pub location: Span,
    pub id: TokenFieldId,
}

#[derive(Clone, Debug)]
pub struct RefInstStart {
    pub location: Span,
    pub data: InstStart,
}

#[derive(Clone, Debug)]
pub struct RefInstNext {
    pub location: Span,
    pub data: InstNext,
}

#[derive(Clone, Debug)]
pub struct RefTable {
    pub location: Span,
    pub id: TableId,
}

#[derive(Clone, Debug)]
pub struct CpuBranch {
    pub cond: Option<Expr>,
    pub call: BranchCall,
    pub direct: bool,
    pub dst: Expr,
}

#[derive(Clone, Debug, Copy)]
pub enum BranchCall {
    Goto,
    Call,
    Return,
}

#[derive(Clone, Debug)]
pub struct LocalGoto {
    pub cond: Option<Expr>,
    pub dst: BlockId,
}

#[derive(Clone, Debug)]
pub enum WriteValue {
    Varnode(WriteVarnode),
    Bitrange(WriteBitrange),
    ///only with attach variable
    TokenField(WriteTokenField),
    TableExport(WriteTable),
    Local(WriteExeVar),
}

#[derive(Clone, Debug)]
pub struct WriteVarnode {
    pub location: Span,
    pub id: VarnodeId,
}

#[derive(Clone, Debug)]
pub struct WriteBitrange {
    pub location: Span,
    pub id: BitrangeId,
}

#[derive(Clone, Debug)]
pub struct WriteTokenField {
    pub location: Span,
    pub id: TokenFieldId,
}

#[derive(Clone, Debug)]
pub struct WriteTable {
    pub location: Span,
    pub id: TableId,
}

#[derive(Clone, Debug)]
pub struct WriteExeVar {
    pub location: Span,
    pub id: VariableId,
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub location: Span,
    pub var: WriteValue,
    pub op: Option<Truncate>,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub struct MemWrite {
    pub addr: Expr,
    pub mem: MemoryLocation,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub struct Build {
    pub table: ExprTable,
}

#[derive(Clone, Debug)]
pub struct MacroCall {
    pub params: Box<[Expr]>,
    pub function: PcodeMacroCallId,
}

#[derive(Clone, Debug)]
pub enum ExportConst {
    DisVar(disassembly::VariableId),
    /// Attach values are limited
    TokenField(TokenFieldId),
    /// Attach values are limited
    Context(ContextId),
    // the instruction start addr,
    InstructionStart,
    /// only if also export Const or Context value
    Table(TableId),
    ExeVar(VariableId),
}

#[derive(Clone, Debug)]
pub enum Export {
    /// export a value that is known before the execution step.
    Const {
        /// len in bits of the exported value
        len_bits: NumberNonZeroUnsigned,
        /// location of the value exported
        location: Span,
        /// exported value
        export: ExportConst,
    },
    /// Arbitrarelly values
    Value(Expr),
    /// Reference to a memory
    Reference { addr: Expr, memory: MemoryLocation },
}

#[derive(Clone, Debug)]
pub struct MemoryLocation {
    pub space: SpaceId,
    pub len_bytes: NumberNonZeroUnsigned,
}

#[derive(Clone, Copy, Debug)]
pub struct Truncate {
    pub lsb: NumberUnsigned,
    pub len_bits: NumberNonZeroUnsigned,
}

#[derive(Clone, Debug)]
pub enum Unary {
    //NOTE: The ByteRangeMsb, ByteRangeLsb and BitRange from docs are all
    //Truncate
    Truncate(Truncate),
    Dereference(MemoryLocation),
    //Reference(AddrReference),
    Negation,
    BitNegation,
    Negative,
    FloatNegative,
    Popcount,
    Lzcount,
    Zext,
    Sext,
    FloatNan,
    FloatAbs,
    FloatSqrt,
    Int2Float,
    Float2Float,
    SignTrunc,
    FloatCeil,
    FloatFloor,
    FloatRound,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Binary {
    //Binary Arithmetic
    Mult,
    Div,
    SigDiv,
    Rem,
    FloatDiv,
    FloatMult,
    Add,
    Sub,
    FloatAdd,
    FloatSub,
    Lsl,
    Lsr,
    Asr,
    BitAnd,
    BitXor,
    BitOr,
    //Binary Logical
    SigLess,
    SigGreater,
    SigRem,
    SigLessEq,
    SigGreaterEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    FloatLess,
    FloatGreater,
    FloatLessEq,
    FloatGreaterEq,
    And,
    Xor,
    Or,
    Eq,
    Ne,
    FloatEq,
    FloatNe,
    //call functions
    Carry,
    SCarry,
    SBorrow,
}

impl Variable {
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Execution {
    pub fn variable(&self, id: VariableId) -> &Variable {
        &self.variables[id.0]
    }
    pub fn block(&self, id: BlockId) -> &Block {
        &self.blocks[id.0]
    }
    pub fn export(&self) -> impl Iterator<Item = &Export> {
        self.blocks.iter().filter_map(|block| {
            block
                .statements
                .last()
                .and_then(|statement| match statement {
                    Statement::Export(export) => Some(export),
                    _ => None,
                })
        })
    }
}
