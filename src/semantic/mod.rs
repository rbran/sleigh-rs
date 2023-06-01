pub mod disassembly;
pub mod display;
pub mod execution;
pub mod meaning;
pub mod pattern;
pub mod pcode_macro;
pub mod space;
pub mod table;
pub mod token;
pub mod user_function;
pub mod varnode;

// internal representation used to convert from syntax to semantic
// represenatation
mod inner;

use crate::semantic::inner::{SolvedLocation, SolverStatus};
use crate::{syntax, Endian, NumberNonZeroUnsigned, SleighError, Span};

use self::inner::Solved;
use self::meaning::{AttachLiteral, AttachNumber, AttachVarnode};
use self::pcode_macro::PcodeMacro;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PcodeMacroId(pub usize);

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
pub struct VarnodeId(pub usize);

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
    PcodeMacro(PcodeMacroId),
    Table(TableId),
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
    pub fn pcode_macro(&self) -> Option<PcodeMacroId> {
        match self {
            GlobalScope::PcodeMacro(x) => Some(*x),
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
    pub endian: Endian,
    pub alignment: u8,

    pub default_space: SpaceId,
    pub instruction_table: TableId,

    spaces: Box<[Space]>,
    varnodes: Box<[Varnode]>,
    contexts: Box<[Context]>,
    bitranges: Box<[Bitrange]>,
    tokens: Box<[Token]>,
    token_fields: Box<[TokenField]>,
    user_functions: Box<[UserFunction]>,
    pcode_macros: Box<[PcodeMacro]>,
    tables: Box<[Table]>,

    attach_varnodes: Box<[AttachVarnode]>,
    attach_literals: Box<[AttachLiteral]>,
    attach_numbers: Box<[AttachNumber]>,
    //scope with all the global identifiers
    //pub global_scope: HashMap<String, GlobalScope>,
    /// Context mapped into single and packed memory block
    pub context_memory: ContextMemoryMapping,
}

impl Sleigh {
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
    pub fn pcode_macro(&self, pcode_macro: PcodeMacroId) -> &PcodeMacro {
        &self.pcode_macros[pcode_macro.0]
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
    pub(crate) fn new(value: syntax::Sleigh) -> Result<Self, SleighError> {
        let inner = inner::Sleigh::new(value)?;
        //HACK: verify that indirect recursion don't happen
        //NOTE we don't need to worry about direct (self) recursion.
        //AKA `Tablea` calling itself
        for table_id in
            inner.global_scope.values().filter_map(GlobalScope::table)
        {
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
            if i > 100 {
                //TODO return an error, but for now force the conversion,
                //so we can find where the error occour
                //break;
                panic!("Too many tries, unable to solve tables")
            }
            if !solved.we_did_a_thing() {
                //print the location that where unable to solve
                //TODO change solve functions to use <T: SolverStatus + ?Sized>
                let mut solved = SolvedLocation::default();
                for table in inner.tables.iter() {
                    table.solve(&inner, &mut solved)?;
                }
                //TODO return an error, but for now force the conversion,
                //so we can find where the error occour
                //break;
                panic!("Unable to solve the table")
            }
        }
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
            .ok_or_else(|| SleighError::SpaceMissingDefault)?;
        //TODO check all constructor for tables have export of the same size
        let token_fields = inner
            .token_fields
            .into_iter()
            .map(|x| x.convert())
            .collect();
        let pcode_macros = inner
            .pcode_macros
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
            pcode_macros,
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
            //global_scope: inner.global_scope,
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
    pub fn pcode_macros(&self) -> &[PcodeMacro] {
        &self.pcode_macros
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
}
