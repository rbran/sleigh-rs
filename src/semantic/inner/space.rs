use std::rc::Rc;

use crate::semantic::inner;
use crate::semantic::space::SpaceType;
use crate::semantic::GlobalElement;
use crate::semantic::SemanticError;
use crate::syntax::define;

use super::FieldSize;
use super::Sleigh;

impl Sleigh {
    pub fn create_space(
        &mut self,
        space: define::Space,
    ) -> Result<(), SemanticError> {
        let src = space.src;
        let (mut default, mut addr_size, mut wordsize, mut space_type) =
            (false, None, None, None);
        for att in space.attributes.into_iter() {
            use define::Attribute::*;
            match att {
                Type(x) if space_type.is_none() => space_type = Some(x),
                Size(x) if addr_size.is_none() => addr_size = Some(x),
                WordSize(x) if wordsize.is_none() => wordsize = Some(x),
                Default if !default => default = true,
                _ => return Err(SemanticError::SpaceInvalidAtt),
            }
        }
        let word_size = wordsize
            .unwrap_or(1)
            .try_into()
            .map_err(|_| SemanticError::SpaceMissingSize)?;
        let addr_size = match addr_size
            .ok_or(SemanticError::SpaceMissingSize)?
        {
            0 => return Err(SemanticError::SpaceInvalidSize),
            x => x.try_into().map_err(|_| SemanticError::SpaceMissingSize)?,
        };
        let space_type = space_type.unwrap_or(SpaceType::Register);
        let space = GlobalElement::new_space(
            &space.name,
            src,
            space_type,
            word_size,
            addr_size,
        );
        if default {
            self.default_space
                .replace(space.clone())
                .map(|_| Err(SemanticError::SpaceMultipleDefault))
                .unwrap_or(Ok(()))?;
            //set the exec addr size
            self.exec_addr_size =
                Some(FieldSize::new_bytes(space.addr_bytes()));
        }
        self.idents
            .insert(Rc::clone(&space.name), inner::GlobalScope::Space(space))
            .map(|_| Err(SemanticError::NameDuplicated))
            .unwrap_or(Ok(()))
    }
}
