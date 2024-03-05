use crate::semantic::meaning::Meaning;
use crate::semantic::token::{TokenField as FinalTokenField, TokenFieldAttach};
use crate::semantic::{
    NumberNonZeroUnsigned, PrintBase, SleighError, Span, Token, TokenFieldId,
    TokenId,
};
use crate::{syntax, FieldBits};

use super::{FieldSize, GlobalScope, PrintFlags, Sleigh};

pub struct TokenField {
    pub name: String,
    pub location: Span,
    pub bits: FieldBits,
    pub token: TokenId,
    pub print_flags: PrintFlags,
    pub attach: Option<TokenFieldAttach>,
}
impl std::fmt::Debug for TokenField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TokenField bit_range: ({:?})", self.bits,)
    }
}

impl TokenField {
    pub fn attach(&mut self, meaning: Meaning) -> Result<(), Box<SleighError>> {
        if self.attach.is_some() {
            //once attached, can't attach again
            return Err(Box::new(SleighError::AttachMultiple(
                self.location.clone(),
            )));
        }
        if self.print_flags.signed_set {
            todo!("Is allowed to attach to signed value?");
        }
        if meaning.is_number() && self.print_flags.base.is_some() {
            todo!("Is allowed to attach varnode/literal if base is set?");
        }
        let attach = match meaning {
            Meaning::NoAttach(_) => unreachable!(),
            Meaning::Varnode(x) => TokenFieldAttach::Varnode(x),
            Meaning::Literal(x) => TokenFieldAttach::Literal(x),
            // use the default print base for TokenField with attach number: Hex
            Meaning::Number(_, x) => TokenFieldAttach::Number(
                self.print_flags.base.unwrap_or(PrintBase::Hex),
                x,
            ),
        };
        //TODO what if print_flags are set?
        self.attach = Some(attach);
        Ok(())
    }

    pub fn exec_value_len(&self, sleigh: &Sleigh) -> FieldSize {
        // return the meaning len, or if not meaning len, the raw field len
        match self.attach {
            // is only created after the finalization if attach is None
            Some(TokenFieldAttach::NoAttach(_)) => unreachable!(),
            // does not affect the token_field len
            Some(TokenFieldAttach::Literal(_)) | None => {
                FieldSize::default().set_min_bits(self.bits.len()).unwrap()
            }
            Some(TokenFieldAttach::Varnode(attach_id)) => {
                let varnodes = sleigh.attach_varnode(attach_id);
                //all varnodes have the same len
                varnodes.execution_len(sleigh)
            }
            Some(TokenFieldAttach::Number(_, attach_id)) => {
                let attach = sleigh.attach_number(attach_id);
                attach.execution_len()
            }
        }
    }

    pub fn convert(self) -> FinalTokenField {
        //default to literal if unset
        let attach = self
            .attach
            .unwrap_or(TokenFieldAttach::NoAttach(self.print_flags.into()));
        FinalTokenField {
            name: self.name.into(),
            location: self.location,
            bits: self.bits,
            token: self.token,
            attach,
        }
    }
}

impl Sleigh {
    pub fn create_token(
        &mut self,
        input: syntax::define::Token,
    ) -> Result<(), Box<SleighError>> {
        let size = input
            .size
            .checked_div(8)
            .and_then(NumberNonZeroUnsigned::new)
            .ok_or_else(|| {
                SleighError::TokenFieldSizeInvalid(input.src.clone())
            })?;
        let endian = input
            .endian
            .or(self.endian)
            .expect("Global endian undefined at this point is a logical error");
        let token = Token {
            location: input.src.clone(),
            len_bytes: size,
            endian,
            name: input.name.clone().into(),
        };
        self.tokens.push(token);
        let token_id = TokenId(self.tokens.len() - 1);
        self.global_scope
            .insert(input.name, GlobalScope::Token(token_id))
            .map(|_| Err(SleighError::NameDuplicated))
            .unwrap_or(Ok(()))?;

        for field in input.token_fields.into_iter() {
            let range: FieldBits = field.range.try_into()?;
            let print_flags = PrintFlags::from_token_att(
                &input.src,
                field.attributes.iter(),
            )?;
            //default into print in hex format
            let token_field = TokenField {
                name: field.name.clone(),
                location: field.name_span,
                bits: range,
                token: token_id,
                print_flags,
                attach: None,
            };
            self.token_fields.push(token_field);
            let token_field_id = TokenFieldId(self.token_fields.len() - 1);
            self.global_scope
                .insert(field.name, GlobalScope::TokenField(token_field_id))
                .map(|_| Err(SleighError::NameDuplicated))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
}
