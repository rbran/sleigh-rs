use std::ops::Range;

use crate::{
    AttachNumberId, AttachVarnodeId, Number, NumberNonZeroUnsigned,
    NumberUnsigned, Sleigh, Span,
};

use super::{
    disassembly, BitrangeId, ContextId, InstNext, InstStart, SpaceId, TableId,
    TokenFieldId, UserFunctionId, VarnodeId,
};

#[derive(Clone, Copy, Debug)]
pub enum ExportLen {
    /// value that is known at Dissassembly time
    Const(NumberNonZeroUnsigned),
    /// value that can be know at execution time
    Value(NumberNonZeroUnsigned),
    /// References/registers and other mem locations, all with the same size
    Reference(NumberNonZeroUnsigned),
    /// If each table exports a diferent type, could happen in individual
    /// constructors, if it exports a sub_table that export Multiple
    Multiple(NumberNonZeroUnsigned),
}

impl ExportLen {
    pub fn len(&self) -> NumberNonZeroUnsigned {
        match self {
            Self::Const(len)
            | Self::Value(len)
            | Self::Reference(len)
            | Self::Multiple(len) => *len,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Execution {
    pub(crate) variables: Box<[Variable]>,
    pub(crate) blocks: Box<[Block]>,
    pub(crate) export: Option<ExportLen>,

    // TODO make this a const, the first block is always the entry block
    //entry_block have no name and is not on self.labels
    pub entry_block: BlockId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, Debug)]
pub struct Block {
    //None is entry block, NOTE name may not be unique due to macro expansions
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
    Build(Build),
    Declare(VariableId),
    Assignment(Assignment),
    // TODO make MemWrite a subtype of Assignment
    MemWrite(MemWrite),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VariableId(pub usize);

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
impl Expr {
    pub fn len_bits(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> NumberNonZeroUnsigned {
        match self {
            Expr::Value(value) => value.len_bits(sleigh, execution),
            Expr::Op(op) => op.len_bits,
        }
    }
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
    Value { location: Span, value: ExprValue },
    UserCall(UserCall),
    Reference(Reference),
    Op(ExprUnaryOp),
    New(ExprNew),
    CPool(ExprCPool),
}
impl ExprElement {
    fn len_bits(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> NumberNonZeroUnsigned {
        match self {
            Self::Value { value, .. } => value.len_bits(sleigh, execution),
            Self::UserCall(_x) => unimplemented!(),
            Self::Reference(x) => x.len_bits,
            Self::Op(x) => x.output_bits,
            Self::New(_x) => unimplemented!(),
            Self::CPool(_x) => unimplemented!(),
        }
    }
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
    /// Simple Int value
    Int(ExprNumber),
    /// Context/TokenField value translated into a Int
    IntDynamic(ExprDynamicInt),
    InstStart(InstStart),
    InstNext(InstNext),
    /// simple TokenField, no attachment
    TokenField(ExprTokenField),
    /// simple Context, no attachment
    Context(ExprContext),
    /// A Varnode Value
    Varnode(VarnodeId),
    /// A Context/TokenField translated into a varnode
    VarnodeDynamic(ExprVarnodeDynamic),
    /// Dynamic Int from Context or TokenField
    Bitrange(ExprBitrange),
    Table(TableId),
    DisVar(ExprDisVar),
    ExeVar(VariableId),
}

impl ExprValue {
    pub fn len_bits(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> NumberNonZeroUnsigned {
        match self {
            Self::Int(x) => x.size,
            Self::TokenField(x) => x.size,
            Self::InstStart(_) | Self::InstNext(_) => {
                (sleigh.addr_bytes().get() * 8).try_into().unwrap()
            }
            Self::Varnode(x) => {
                (sleigh.varnode(*x).len_bytes.get() * 8).try_into().unwrap()
            }
            Self::Context(x) => sleigh.context(x.id).bitrange.bits.len(),
            Self::Bitrange(x) => sleigh.bitrange(x.id).bits.len(),
            Self::Table(x) => sleigh.table(*x).export.unwrap().len(),
            Self::DisVar(x) => x.size,
            Self::ExeVar(x) => execution.variable(*x).len_bits,
            Self::IntDynamic(ExprDynamicInt { bits, .. }) => *bits,
            Self::VarnodeDynamic(ExprVarnodeDynamic { attach_id, .. }) => {
                sleigh.attach_varnode(*attach_id).len_bytes(sleigh)
            }
        }
    }

    fn convert(&self) -> ExprValue {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct ExprNumber {
    pub size: NumberNonZeroUnsigned,
    pub number: Number,
}

#[derive(Clone, Debug)]
pub struct ExprDynamicInt {
    pub attach_id: AttachNumberId,
    pub attach_value: DynamicValueType,
    pub bits: NumberNonZeroUnsigned,
}

#[derive(Clone, Debug)]
pub struct ExprVarnodeDynamic {
    pub attach_id: AttachVarnodeId,
    pub attach_value: DynamicValueType,
}

#[derive(Clone, Debug)]
pub struct ExprTokenField {
    pub size: NumberNonZeroUnsigned,
    pub id: TokenFieldId,
}

#[derive(Clone, Debug)]
pub enum ExprVarnode {
    Static(VarnodeId),
    Dynamic {
        attach_id: AttachVarnodeId,
        attach_value: DynamicValueType,
    },
}

/// Only used for types with attachment values to Varnodes/Ints
#[derive(Clone, Debug)]
pub enum DynamicValueType {
    TokenField(TokenFieldId),
    Context(ContextId),
}

#[derive(Clone, Debug)]
pub struct ExprContext {
    pub size: NumberNonZeroUnsigned,
    pub id: ContextId,
}

#[derive(Clone, Debug)]
pub struct ExprBitrange {
    pub size: NumberNonZeroUnsigned,
    pub id: BitrangeId,
}

#[derive(Clone, Debug)]
pub struct ExprDisVar {
    pub size: NumberNonZeroUnsigned,
    pub id: disassembly::VariableId,
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
    Varnode(VarnodeId),
    Bitrange(BitrangeId),
    ///only with attach variable
    TokenField {
        token_field_id: TokenFieldId,
        attach_id: AttachVarnodeId,
    },
    // TODO Context translated into varnode
    TableExport(TableId),
    Local(VariableId),
}

#[derive(Clone, Debug)]
pub struct Assignment {
    /// assigment location
    pub location: Span,
    /// left side of the assignment location
    pub var: WriteValue,
    pub op: Option<AssignmentOp>,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub enum AssignmentOp {
    TakeLsb(NumberNonZeroUnsigned),
    TrunkLsb(NumberUnsigned),
    BitRange(Range<NumberUnsigned>),
}

#[derive(Clone, Debug)]
pub struct MemWrite {
    pub addr: Expr,
    pub mem: MemoryLocation,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub struct Build {
    pub location: Span,
    pub table: TableId,
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

impl Export {
    pub fn len_bits(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> NumberNonZeroUnsigned {
        match self {
            Export::Const { len_bits, .. } => *len_bits,
            Export::Value(value) => value.len_bits(sleigh, execution),
            Export::Reference { addr: _, memory } => {
                (memory.len_bytes.get() * 8).try_into().unwrap()
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct MemoryLocation {
    pub space: SpaceId,
    pub len_bytes: NumberNonZeroUnsigned,
}

#[derive(Clone, Debug)]
pub enum Unary {
    TakeLsb(NumberNonZeroUnsigned),
    TrunkLsb(NumberUnsigned),
    BitRange(Range<NumberUnsigned>),
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
    pub fn variables(&self) -> &[Variable] {
        &self.variables
    }

    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }

    pub fn block(&self, id: BlockId) -> &Block {
        &self.blocks[id.0]
    }

    pub fn export_len(&self) -> Option<ExportLen> {
        self.export
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

    pub fn variable(&self, var: VariableId) -> &Variable {
        &self.variables[var.0]
    }
}
