pub mod disassembly;
pub mod display;
pub mod execution;
pub mod meaning;
pub mod pattern;
pub mod space;
pub mod table;
pub mod token;
pub mod user_function;
pub mod varnode;

// internal representation used to convert from syntax to semantic
// represenatation
pub(crate) mod inner;

use std::collections::HashMap;

use crate::semantic::inner::{SolvedLocation, SolverStatus};
use crate::{syntax, Endian, NumberNonZeroUnsigned, SleighError, Span};

use self::inner::Solved;
use self::meaning::{AttachLiteral, AttachNumber, AttachVarnode};
use self::space::Space;
use self::table::Table;
use self::token::{Token, TokenField};
use self::user_function::UserFunction;
use self::varnode::{Bitrange, Context, ContextMemoryMapping, Varnode};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum PrintBase {
    Dec,
    Hex,
}
impl PrintBase {
    pub fn is_hex(&self) -> bool {
        matches!(self, Self::Hex)
    }
    pub fn is_dec(&self) -> bool {
        matches!(self, Self::Dec)
    }
}
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ValueFmt {
    pub signed: bool,
    pub base: PrintBase,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct InstStart;

#[derive(Clone, Copy, Debug, Default)]
pub struct InstNext;

#[derive(Clone, Copy, Debug, Default)]
pub struct Epsilon;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct SpaceId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TableId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TokenId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TokenFieldId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct UserFunctionId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BitrangeId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ContextId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VarnodeId(usize);
impl VarnodeId {
    pub(crate) unsafe fn from_raw(idx: usize) -> Self {
        Self(idx)
    }
    pub fn to_raw(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AttachLiteralId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AttachNumberId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AttachVarnodeId(pub usize);

#[derive(Clone, Copy, Debug)]
pub enum GlobalScope {
    Space(SpaceId),
    Varnode(VarnodeId),
    Context(ContextId),
    Bitrange(BitrangeId),
    Token(TokenId),
    TokenField(TokenFieldId),
    InstStart(InstStart),
    InstNext(InstNext),
    Epsilon(Epsilon),
    UserFunction(UserFunctionId),
    Table(TableId),
}

impl From<inner::GlobalScope> for GlobalScope {
    fn from(value: inner::GlobalScope) -> Self {
        match value {
            inner::GlobalScope::Space(x) => Self::Space(x),
            inner::GlobalScope::Varnode(x) => Self::Varnode(x),
            inner::GlobalScope::Context(x) => Self::Context(x),
            inner::GlobalScope::Bitrange(x) => Self::Bitrange(x),
            inner::GlobalScope::Token(x) => Self::Token(x),
            inner::GlobalScope::TokenField(x) => Self::TokenField(x),
            inner::GlobalScope::InstStart(x) => Self::InstStart(x),
            inner::GlobalScope::InstNext(x) => Self::InstNext(x),
            inner::GlobalScope::Epsilon(x) => Self::Epsilon(x),
            inner::GlobalScope::UserFunction(x) => Self::UserFunction(x),
            inner::GlobalScope::Table(x) => Self::Table(x),

            inner::GlobalScope::PcodeMacro(_) => unreachable!(),
        }
    }
}

impl GlobalScope {
    pub fn token_field(&self) -> Option<TokenFieldId> {
        match self {
            GlobalScope::TokenField(x) => Some(*x),
            _ => None,
        }
    }
    pub fn token(&self) -> Option<TokenId> {
        match self {
            GlobalScope::Token(x) => Some(*x),
            _ => None,
        }
    }
    pub fn space(&self) -> Option<SpaceId> {
        match self {
            GlobalScope::Space(x) => Some(*x),
            _ => None,
        }
    }
    pub fn varnode(&self) -> Option<VarnodeId> {
        match self {
            GlobalScope::Varnode(x) => Some(*x),
            _ => None,
        }
    }
    pub fn context(&self) -> Option<ContextId> {
        match self {
            GlobalScope::Context(x) => Some(*x),
            _ => None,
        }
    }
    pub fn bitrange(&self) -> Option<BitrangeId> {
        match self {
            GlobalScope::Bitrange(x) => Some(*x),
            _ => None,
        }
    }
    pub fn table(&self) -> Option<TableId> {
        match self {
            GlobalScope::Table(x) => Some(*x),
            _ => None,
        }
    }
    pub fn user_function(&self) -> Option<UserFunctionId> {
        match self {
            GlobalScope::UserFunction(x) => Some(*x),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Sleigh {
    endian: Endian,
    alignment: u8,

    default_space: SpaceId,
    // TODO make it a const value 0, first table is always the instruction table
    instruction_table: TableId,

    spaces: Box<[Space]>,
    varnodes: Box<[Varnode]>,
    contexts: Box<[Context]>,
    bitranges: Box<[Bitrange]>,
    tokens: Box<[Token]>,
    token_fields: Box<[TokenField]>,
    user_functions: Box<[UserFunction]>,
    tables: Box<[Table]>,

    attach_varnodes: Box<[AttachVarnode]>,
    attach_literals: Box<[AttachLiteral]>,
    attach_numbers: Box<[AttachNumber]>,
    //scope with all the global identifiers
    global_scope: HashMap<String, GlobalScope>,
    /// Context mapped into single and packed memory block
    context_memory: ContextMemoryMapping,
}

impl Sleigh {
    pub fn endian(&self) -> Endian {
        self.endian
    }
    pub fn alignemnt(&self) -> u8 {
        self.alignment
    }
    // TODO make it a const value 0, first table is always the instruction table
    pub fn instruction_table(&self) -> TableId {
        self.instruction_table
    }
    pub fn context_memory(&self) -> &ContextMemoryMapping {
        &self.context_memory
    }
    pub fn global_scope_by_name<'a>(
        &'a self,
        name: &str,
    ) -> Option<&'a GlobalScope> {
        self.global_scope.get(name)
    }
    pub fn space(&self, space: SpaceId) -> &Space {
        &self.spaces[space.0]
    }
    pub fn varnode(&self, varnode: VarnodeId) -> &Varnode {
        &self.varnodes[varnode.0]
    }
    pub fn context(&self, context: ContextId) -> &Context {
        &self.contexts[context.0]
    }
    pub fn bitrange(&self, bitrange: BitrangeId) -> &Bitrange {
        &self.bitranges[bitrange.0]
    }
    pub fn token(&self, token: TokenId) -> &Token {
        &self.tokens[token.0]
    }
    pub fn token_field(&self, token_field: TokenFieldId) -> &TokenField {
        &self.token_fields[token_field.0]
    }
    pub fn user_function(
        &self,
        user_function: UserFunctionId,
    ) -> &UserFunction {
        &self.user_functions[user_function.0]
    }
    pub fn table(&self, table: TableId) -> &Table {
        &self.tables[table.0]
    }
    pub fn attach_varnode(&self, id: AttachVarnodeId) -> &AttachVarnode {
        &self.attach_varnodes[id.0]
    }
    pub fn attach_number(&self, id: AttachNumberId) -> &AttachNumber {
        &self.attach_numbers[id.0]
    }
    pub fn attach_literal(&self, id: AttachLiteralId) -> &AttachLiteral {
        &self.attach_literals[id.0]
    }
    pub(crate) fn new(value: syntax::Sleigh) -> Result<Self, Box<SleighError>> {
        let inner = inner::Sleigh::new(value)?;
        tracing::trace!("semantic tree constructed sucessfully");
        //HACK: verify that indirect recursion don't happen
        //NOTE we don't need to worry about direct (self) recursion.
        //AKA `Tablea` calling itself
        let tables = inner
            .global_scope
            .values()
            .filter_map(inner::GlobalScope::table);
        for table_id in tables {
            use std::ops::ControlFlow;
            let table = inner.table(table_id);
            if let ControlFlow::Break(rec) =
                table.pattern_indirect_recursion(&inner, table_id)
            {
                unimplemented!(
                    "Indirect recursion is not implemented at the moment {:?}",
                    rec
                );
            }
        }

        //TODO: if unable to solve addr size or default space not set, error
        let _ = inner.default_space.as_ref().unwrap();
        //solve all pcodes macros inside the tables
        //solve all tables
        for i in 0.. {
            let mut solved = Solved::default();
            for table in inner.tables.iter() {
                table.solve(&inner, &mut solved)?;
            }
            if solved.we_finished() && !solved.we_did_a_thing() {
                break;
            }
            if i > 100 || !solved.we_did_a_thing() {
                let mut solved = SolvedLocation::default();
                for table in inner.tables.iter() {
                    table.solve(&inner, &mut solved)?;
                }
                return Err(Box::new(SleighError::TableUnsolvable(
                    solved.locations.iter().fold(
                        String::new(),
                        |mut acc, (location, file, line)| {
                            use std::fmt::Write;
                            writeln!(
                                &mut acc,
                                "{}:{}: {location}",
                                file,
                                line + 1
                            )
                            .unwrap();
                            acc
                        },
                    ),
                )));
            }
        }
        tracing::trace!("semantic table solving sucessfully");
        let context_memory = ContextMemoryMapping::map_all(&inner);
        let contexts: Box<[_]> = inner
            .contexts
            .into_iter()
            .map(|context| context.convert())
            .collect();

        let endian = inner.endian.ok_or(SleighError::EndianMissing)?;
        let alignment = inner.alignment.unwrap_or(0).try_into().unwrap();
        // unwrap because should be created on Sleigh::new
        let default_space = inner
            .default_space
            .ok_or(SleighError::SpaceMissingDefault)?;
        //TODO check all constructor for tables have export of the same size
        let token_fields = inner
            .token_fields
            .into_iter()
            .map(|x| x.convert())
            .collect();
        let mut sleigh = Self {
            endian,
            alignment,
            default_space,
            context_memory,
            contexts,
            token_fields,
            tokens: inner.tokens.into(),
            instruction_table: inner.instruction_table,
            spaces: inner.spaces.into(),
            varnodes: inner.varnodes.into(),
            user_functions: inner.user_functions.into(),
            bitranges: inner.bitranges.into(),
            attach_varnodes: inner.attach_varnodes.into(),
            attach_literals: inner.attach_literals.into(),
            attach_numbers: inner.attach_numbers.into(),
            tables: Box::new([]),
            global_scope: inner
                .global_scope
                .into_iter()
                .filter_map(|(k, v)| match v {
                    // PcodeMacro is fully replaced at this point
                    inner::GlobalScope::PcodeMacro(_) => None,
                    v => Some((k, v.into())),
                })
                .collect(),
        };
        let tables = inner
            .tables
            .into_iter()
            .map(|x| x.convert(&sleigh))
            .collect();
        sleigh.tables = tables;

        //TODO remove unused table/macro/pcode_macro/etc?
        Ok(sleigh)
    }
    pub fn addr_bytes(&self) -> NumberNonZeroUnsigned {
        self.space(self.default_space).addr_bytes
    }

    pub fn spaces(&self) -> &[Space] {
        &self.spaces
    }
    pub fn varnodes(&self) -> &[Varnode] {
        &self.varnodes
    }
    pub fn contexts(&self) -> &[Context] {
        &self.contexts
    }
    pub fn bitranges(&self) -> &[Bitrange] {
        &self.bitranges
    }
    pub fn user_functions(&self) -> &[UserFunction] {
        &self.user_functions
    }
    pub fn tables(&self) -> &[Table] {
        &self.tables
    }
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }
    pub fn token_fields(&self) -> &[TokenField] {
        &self.token_fields
    }
    pub fn attach_varnodes(&self) -> &[AttachVarnode] {
        &self.attach_varnodes
    }
    pub fn attach_numbers(&self) -> &[AttachNumber] {
        &self.attach_numbers
    }
    pub fn attach_literals(&self) -> &[AttachLiteral] {
        &self.attach_literals
    }
    pub fn default_space(&self) -> SpaceId {
        self.default_space
    }

    pub fn attach_varnodes_len_bytes(
        &self,
        id: AttachVarnodeId,
    ) -> NumberNonZeroUnsigned {
        self.varnode(self.attach_varnode(id).0[0].1).len_bytes
    }
}
