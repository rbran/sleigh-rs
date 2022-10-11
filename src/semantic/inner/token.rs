use std::rc::Rc;

use crate::base::NonZeroTypeU;
use crate::semantic::{assembly, inner, PrintFmt, SemanticError};
use crate::syntax::define;

use super::Sleigh;

impl<'a> Sleigh<'a> {
    pub fn create_token(
        &mut self,
        mut token: define::Token<'a>,
    ) -> Result<(), SemanticError> {
        let token_name = Rc::from(token.name);
        let size = NonZeroTypeU::new(token.size)
            .ok_or(SemanticError::TokenInvalidSize)?;
        let endian = token
            .endian
            .or(self.endian)
            .expect("Global endian undefined at this point is a logical error");
        let token_ref = Rc::new(assembly::Token {
            name: Rc::clone(&token_name),
            size,
            endian,
        });
        self.idents
            .insert(
                Rc::clone(&token_name),
                inner::GlobalScope::Token(Rc::clone(&token_ref)),
            )
            .map(|_| Err(SemanticError::NameDuplicated))
            .unwrap_or(Ok(()))?;

        for mut field in token.token_fields.drain(..) {
            if field.start > field.end || field.end >= size.get() {
                return Err(SemanticError::TokenFieldInvalidSize);
            }
            let (mut signed, mut fmt) = (false, None);
            for att in field.attributes.drain(..) {
                use define::TokenFieldAttribute::*;
                match att {
                    PrintFmt(x) if fmt.is_none() => fmt = Some(x),
                    Signed if !signed => signed = true,
                    _ => return Err(SemanticError::TokenFieldAttDup),
                }
            }
            let fmt = fmt.unwrap_or(PrintFmt::Hex);
            let assembly = assembly::Assembly::new_field(
                &field.name,
                field.start,
                field.end,
                signed,
                fmt,
                &token_ref,
            );
            self.idents
                .insert(
                    Rc::clone(assembly.name()),
                    inner::GlobalScope::Assembly(assembly),
                )
                .map(|_| Err(SemanticError::NameDuplicated))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
}
