use std::collections::HashMap;

use crate::semantic::meaning::{AttachLiteral, AttachNumber, AttachVarnode};
use crate::semantic::{
    AttachLiteralId, AttachNumberId, AttachVarnodeId, BitrangeId, ContextId,
    GlobalScope, PcodeMacroId, SpaceId, TableId, TokenFieldId, TokenId,
    UserFunctionId, VarnodeId,
};
use crate::syntax::define::TokenFieldAttribute;
use crate::{
    syntax, Endian, NumberNonZeroUnsigned, NumberUnsigned, SleighError, Span,
    IDENT_EPSILON, IDENT_INSTRUCTION, IDENT_INST_NEXT, IDENT_INST_START,
};

use super::space::Space;
use super::token::Token;
use super::user_function::UserFunction;
use super::varnode::{Bitrange, Varnode};
use super::{Epsilon, InstNext, InstStart, PrintBase, ValueFmt};

pub mod attach;
pub mod disassembly;
pub mod display;
pub mod execution;
pub mod pattern;
pub mod pcode_macro;
pub mod space;
pub mod table;
pub mod token;
pub mod varnode;
pub mod with_block;

use self::execution::FieldSize;
use self::pattern::Pattern;
use self::pcode_macro::PcodeMacro;
use self::table::Table;
use self::token::TokenField;
use self::varnode::Context;
use self::with_block::WithBlockCurrent;

#[derive(Copy, Clone, Debug)]
pub struct PrintFlags {
    ///flag if signed was set
    pub signed_set: bool,
    ///flag if hex or dec was set
    pub base: Option<PrintBase>,
}

impl PrintFlags {
    pub fn from_token_att<'a>(
        src: &Span,
        att: impl Iterator<Item = &'a TokenFieldAttribute>,
    ) -> Result<Self, Box<SleighError>> {
        let (mut signed_set, mut base) = (false, None);
        for att in att {
            use syntax::define::TokenFieldAttribute::*;
            match att {
                Hex if base.is_none() => base = Some(PrintBase::Hex),
                Dec if base.is_none() => base = Some(PrintBase::Dec),
                Hex | Dec => {
                    return Err(Box::new(SleighError::TokenFieldAttachDup(
                        src.clone(),
                    )))
                }
                Signed if !signed_set => signed_set = true,
                Signed => {
                    return Err(Box::new(SleighError::TokenFieldAttDup(
                        src.clone(),
                    )))
                }
            }
        }
        Ok(Self { signed_set, base })
    }
    pub fn is_set(&self) -> bool {
        self.signed_set || self.base.is_some()
    }
}

impl From<PrintFlags> for ValueFmt {
    fn from(flags: PrintFlags) -> Self {
        //if signed is set, this is signed, otherwise is unsigned
        let signed = flags.signed_set;
        //use the set base, if unset, use the default: hex
        let base = flags.base.unwrap_or(PrintBase::Hex);
        ValueFmt { signed, base }
    }
}

pub trait SolverStatus<T: SolverStatus = Self> {
    fn iam_not_finished(
        &mut self,
        location: &Span,
        file: &'static str,
        line: u32,
    );
    fn i_did_a_thing(&mut self);
    fn we_finished(&self) -> bool;
    fn we_did_a_thing(&self) -> bool;
    fn unfinished_locations(&self) -> &[(Span, &'static str, u32)];
    fn combine(&mut self, other: &Self);
}

#[derive(Clone, Copy, Debug)]
pub struct Solved {
    did_a_thing: bool,
    finished: bool,
}

impl SolverStatus for Solved {
    fn iam_not_finished(
        &mut self,
        _location: &Span,
        _file: &'static str,
        _line: u32,
    ) {
        self.finished = false;
    }
    fn i_did_a_thing(&mut self) {
        self.did_a_thing = true;
    }
    fn we_finished(&self) -> bool {
        self.finished
    }
    fn we_did_a_thing(&self) -> bool {
        self.did_a_thing
    }
    fn unfinished_locations(&self) -> &[(Span, &'static str, u32)] {
        &[]
    }
    fn combine(&mut self, other: &Self) {
        self.did_a_thing |= other.we_did_a_thing();
        self.finished &= other.we_finished();
    }
}

impl Default for Solved {
    fn default() -> Self {
        Self {
            did_a_thing: false,
            finished: true,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct SolvedLocation {
    solved: Solved,
    locations: Vec<(Span, &'static str, u32)>,
}

impl SolverStatus for SolvedLocation {
    fn iam_not_finished(
        &mut self,
        location: &Span,
        file: &'static str,
        line: u32,
    ) {
        self.solved.iam_not_finished(location, file, line);
        self.locations.push((location.clone(), file, line));
    }
    fn i_did_a_thing(&mut self) {
        self.solved.i_did_a_thing();
    }
    fn we_finished(&self) -> bool {
        self.solved.we_finished()
    }
    fn we_did_a_thing(&self) -> bool {
        self.solved.we_did_a_thing()
    }
    fn unfinished_locations(&self) -> &[(Span, &'static str, u32)] {
        &self.locations
    }
    fn combine(&mut self, other: &Self) {
        self.solved.combine(&other.solved);
        self.locations
            .extend(other.unfinished_locations().iter().cloned());
    }
}

#[derive(Debug)]
pub struct Sleigh {
    /// the default address space
    pub default_space: Option<SpaceId>,
    pub instruction_table: TableId,

    //data that will be passed to the final struct
    /// processor endian
    pub endian: Option<Endian>,
    /// memory access alignemnt
    pub alignment: Option<NumberUnsigned>,
    /// all the unique ident types, such Tables, Macros, Varnodes, etc.
    pub global_scope: HashMap<String, GlobalScope>,

    pub spaces: Vec<Space>,
    pub varnodes: Vec<Varnode>,
    pub contexts: Vec<Context>,
    pub bitranges: Vec<Bitrange>,
    pub tokens: Vec<Token>,
    pub token_fields: Vec<TokenField>,
    pub user_functions: Vec<UserFunction>,
    pub pcode_macros: Vec<PcodeMacro>,
    pub tables: Vec<Table>,

    pub attach_varnodes: Vec<AttachVarnode>,
    pub attach_literals: Vec<AttachLiteral>,
    pub attach_numbers: Vec<AttachNumber>,
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
    pub fn context_mut(&mut self, context: ContextId) -> &mut Context {
        &mut self.contexts[context.0]
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
    pub fn token_field_mut(
        &mut self,
        token_field: TokenFieldId,
    ) -> &mut TokenField {
        &mut self.token_fields[token_field.0]
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
    pub fn table_mut(&mut self, table: TableId) -> &mut Table {
        &mut self.tables[table.0]
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
    pub fn default_space(&self) -> Option<SpaceId> {
        self.default_space
    }
    pub fn get_global(&self, name: &str) -> Option<GlobalScope> {
        self.global_scope.get(name).copied()
    }
    pub fn set_endian(
        &mut self,
        endian: Endian,
    ) -> Result<(), Box<SleighError>> {
        self.endian
            .replace(endian)
            .map(|_old| Err(Box::new(SleighError::EndianMultiple)))
            .unwrap_or(Ok(()))
    }
    pub fn set_alignment(
        &mut self,
        align: syntax::define::Alignment,
    ) -> Result<(), Box<SleighError>> {
        self.alignment
            .replace(align.0)
            .map(|_| Err(Box::new(SleighError::AlignmentMultiple)))
            .unwrap_or(Ok(()))
    }
    fn process(
        &mut self,
        with_block_current: &mut WithBlockCurrent,
        syntax: syntax::Sleigh,
    ) -> Result<(), Box<SleighError>> {
        for assertation in syntax.assertations.into_iter() {
            use syntax::define::Define::*;
            use syntax::Assertation::*;
            match assertation {
                Define(Endian(endian)) => self.set_endian(endian)?,
                Define(Alignment(x)) => self.set_alignment(x)?,
                Define(Space(x)) => self.create_space(x)?,
                Define(Varnode(x)) => self.create_memory(x)?,
                Define(Bitrange(x)) => self.create_bitrange(x)?,
                Define(UserFunction(x)) => self.create_user_function(x)?,
                Define(Context(x)) => self.create_context(x)?,
                Define(Token(x)) => self.create_token(x)?,
                Attach(x) => self.attach_meaning(x)?,
                TableConstructor(x) => {
                    self.insert_table_constructor(with_block_current, x)?
                }
                PcodeMacro(x) => self.create_pcode_macro(x)?,
                WithBlock(with_block) => {
                    //TODO remove this clone
                    let body = with_block_current.push(with_block);
                    self.process(with_block_current, body)?;
                    with_block_current.pop();
                }
            }
        }
        Ok(())
    }

    pub fn addr_bytes(&self) -> Option<NumberNonZeroUnsigned> {
        let space_id = self.default_space?;
        let space = self.space(space_id);
        Some(space.addr_bytes)
    }

    pub fn new(syntax: syntax::Sleigh) -> Result<Self, Box<SleighError>> {
        let instruction_table =
            Table::new_empty(true, IDENT_INSTRUCTION.to_owned());
        let instruction_table_id = TableId(0);
        let mut sleigh = Sleigh {
            tables: vec![instruction_table],
            global_scope: HashMap::from([
                (
                    IDENT_INST_START.to_string(),
                    GlobalScope::InstStart(InstStart),
                ),
                (IDENT_INST_NEXT.to_string(), GlobalScope::InstNext(InstNext)),
                (IDENT_EPSILON.to_string(), GlobalScope::Epsilon(Epsilon)),
                (
                    IDENT_INSTRUCTION.to_string(),
                    GlobalScope::Table(instruction_table_id),
                ),
            ]),
            instruction_table: instruction_table_id,
            default_space: None,
            endian: None,
            alignment: None,
            spaces: vec![],
            varnodes: vec![],
            contexts: vec![],
            bitranges: vec![],
            tokens: vec![],
            token_fields: vec![],
            user_functions: vec![],
            pcode_macros: vec![],
            attach_varnodes: vec![],
            attach_literals: vec![],
            attach_numbers: vec![],
        };

        sleigh.process(&mut WithBlockCurrent::default(), syntax)?;

        Ok(sleigh)
    }
}
