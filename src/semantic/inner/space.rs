use std::rc::Rc;

use crate::semantic::inner;
use crate::semantic::space;
use crate::semantic::SemanticError;
use crate::syntax::define;

use super::Sleigh;

impl<'a> Sleigh<'a> {
    pub fn create_space(
        &mut self,
        mut space: define::Space<'a>,
    ) -> Result<(), SemanticError> {
        let name: Rc<str> = Rc::from(space.name);
        let (mut default, mut addr_size, mut wordsize, mut space_type) =
            (false, None, None, None);
        for att in space.attributes.drain(..) {
            use define::Attribute::*;
            match att {
                Type(x) if space_type.is_none() => space_type = Some(x),
                Size(x) if addr_size.is_none() => addr_size = Some(x),
                WordSize(x) if wordsize.is_none() => wordsize = Some(x),
                Default if !default => default = true,
                _ => return Err(SemanticError::SpaceInvalidAtt),
            }
        }
        let wordsize = wordsize
            .unwrap_or(1)
            .try_into()
            .map_err(|_| SemanticError::SpaceMissingSize)?;
        let addr_size = match addr_size
            .ok_or(SemanticError::SpaceMissingSize)?
        {
            0 => return Err(SemanticError::SpaceInvalidSize),
            x => x.try_into().map_err(|_| SemanticError::SpaceMissingSize)?,
        };
        let mem = space::Memory {
            wordsize,
            addr_size,
        };
        use define::SpaceType::*;
        let space = match space_type.unwrap_or(define::SpaceType::Register) {
            Register => space::SpaceType::Register(mem),
            Rom => space::SpaceType::Rom(mem),
            Ram => space::SpaceType::Ram(mem),
        };
        let space = Rc::new(space::Space {
            name,
            space_type: space,
        });
        if default {
            self.default_space
                .replace(Rc::clone(&space))
                .map(|_| Err(SemanticError::SpaceMultipleDefault))
                .unwrap_or(Ok(()))?;
        }
        self.idents
            .insert(Rc::clone(&space.name), inner::GlobalScope::Space(space))
            .map(|_| Err(SemanticError::NameDuplicated))
            .unwrap_or(Ok(()))
    }
}
