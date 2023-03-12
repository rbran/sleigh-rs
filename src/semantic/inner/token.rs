use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::semantic::meaning::Meaning;
use crate::semantic::token::Token;
use crate::semantic::{GlobalConvert, SemanticError};
use crate::syntax::define;
use crate::{NumberNonZeroUnsigned, BitRange, Span, SleighError};

use super::{
    Endian, FieldSize, GlobalElement, GlobalScope, PrintFlags, Sleigh,
};

//#[derive(Clone, Debug, Error)]
//pub enum TokenFieldError {
//    #[error("Attach value/register/name to TokenField multiple times")]
//    AttachMultiple(InputSource),
//    #[error("Attach variable/name to tokenField with a print flag set")]
//    AttachPrintFlag(InputSource),
//}

type FinalTokenField = crate::semantic::token::TokenField;
pub struct TokenField {
    pub src: Span,
    pub range: BitRange,
    pub token: GlobalElement<Token>,
    //start with false, if set to true, unable to modify
    pub attach_finish: Cell<bool>,
    pub print_flags: PrintFlags,
    meaning: RefCell<Option<Meaning>>,
    result: RefCell<Option<Rc<<TokenField as GlobalConvert>::FinalType>>>,
}
impl std::fmt::Debug for TokenField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TokenField bit_range: ({:?})", self.range,)
    }
}

impl GlobalElement<Token> {
    pub fn new_token(
        name: &str,
        src: Span,
        len_bytes: NumberNonZeroUnsigned,
        endian: Endian,
    ) -> Self {
        let token = Token::new(src, len_bytes, endian);
        GlobalElement::new_from(name, token)
    }
}

impl GlobalElement<TokenField> {
    pub fn new_token_field(
        name: &str,
        src: Span,
        range: BitRange,
        print_flags: PrintFlags,
        token: GlobalElement<Token>,
    ) -> Self {
        let token = TokenField {
            src,
            range,
            token,
            print_flags,
            attach_finish: Cell::new(false),
            meaning: RefCell::new(None),
            result: RefCell::new(None),
        };
        GlobalElement::new_from(name, token)
    }
}

impl TokenField {
    pub fn meaning<'a>(&'a self) -> std::cell::Ref<Option<Meaning>> {
        self.attach_finish.set(true);
        self.meaning.borrow()
    }
    pub fn meaning_mut<'a>(&'a self) -> std::cell::RefMut<Option<Meaning>> {
        assert!(!self.attach_finish.get());
        self.meaning.borrow_mut()
    }
    pub fn range(&self) -> &BitRange {
        &self.range
    }
    pub fn exec_value_len(&self) -> FieldSize {
        match self.meaning().as_ref().map(Meaning::exec_len_bytes) {
            //have special meaning, and the meaning have a defined len
            Some(Some(len)) => FieldSize::new_bytes(len),
            //don't have speacial meaning, or the meaning just use the raw value
            Some(None) | None => {
                FieldSize::default().set_min(self.range().len()).unwrap()
            }
        }
    }
}
impl GlobalConvert for TokenField {
    type FinalType = FinalTokenField;
    fn convert(&self) -> Rc<FinalTokenField> {
        let result = self.result.borrow();
        match result.as_ref() {
            Some(result) => Rc::clone(result),
            None => {
                drop(result);
                let mut result = self.result.borrow_mut();
                //default to literal if unset
                let meaning = self.meaning.borrow();
                let meaning = meaning
                    .clone()
                    .unwrap_or(Meaning::Literal(self.print_flags.into()));
                let final_value = Self::FinalType::new(
                    self.src.clone(),
                    self.range.clone(),
                    self.token.clone(),
                    meaning,
                );
                let final_element = Rc::new(final_value);
                *result = Some(Rc::clone(&final_element));
                final_element
            }
        }
    }
}

impl Sleigh {
    pub fn create_token(
        &mut self,
        input: define::Token,
    ) -> Result<(), SleighError> {
        let size = input
            .size
            .checked_div(8)
            .and_then(NumberNonZeroUnsigned::new)
            .ok_or(SemanticError::TokenInvalidSize)?;
        let endian = input
            .endian
            .or(self.endian)
            .expect("Global endian undefined at this point is a logical error");
        let token = GlobalElement::new_token(
            &input.name,
            input.src.clone(),
            size,
            endian,
        );
        self.idents
            .insert(Rc::clone(&token.name), GlobalScope::Token(token.clone()))
            .map(|_| Err(SemanticError::NameDuplicated))
            .unwrap_or(Ok(()))?;

        for field in input.token_fields.into_iter() {
            let range: BitRange = field.range.try_into()?;
            let print_flags = PrintFlags::from_token_att(
                &input.src,
                field.attributes.iter(),
            )?;
            //default into print in hex format
            let field_ele = GlobalElement::new_token_field(
                &field.name,
                input.src.clone(),
                range,
                print_flags,
                token.clone(),
            );
            self.idents
                .insert(
                    Rc::clone(&field_ele.name),
                    GlobalScope::TokenField(field_ele),
                )
                .map(|_| Err(SemanticError::NameDuplicated))
                .unwrap_or(Ok(()))?;
        }
        Ok(())
    }
}
