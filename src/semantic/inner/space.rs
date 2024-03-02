use crate::semantic::space::SpaceType;
use crate::semantic::{GlobalScope, Space, SpaceId};
use crate::{syntax, SleighError};

use super::Sleigh;

impl Sleigh {
    pub fn create_space(
        &mut self,
        input: syntax::define::Space,
    ) -> Result<(), Box<SleighError>> {
        let src = input.src;
        let (mut default, mut addr_size, mut wordsize, mut space_type) =
            (false, None, None, None);
        for att in input.attributes.into_iter() {
            use syntax::define::Attribute::*;
            match att {
                Type(x) if space_type.is_none() => space_type = Some(x),
                Size(x) if addr_size.is_none() => addr_size = Some(x),
                WordSize(x) if wordsize.is_none() => wordsize = Some(x),
                Default if !default => default = true,
                _ => return Err(Box::new(SleighError::SpaceInvalidAtt(src))),
            }
        }
        let word_size = wordsize.unwrap_or(1).try_into().map_err(|_| {
            Box::new(SleighError::SpaceMissingSize(src.clone()))
        })?;
        let addr_size = match addr_size.ok_or_else(|| {
            Box::new(SleighError::SpaceMissingSize(src.clone()))
        })? {
            0 => {
                return Err(Box::new(SleighError::SpaceInvalidSize(
                    src.clone(),
                )))
            }
            x => x.try_into().map_err(|_| {
                Box::new(SleighError::SpaceInvalidSize(src.clone()))
            })?,
        };
        let space_type = space_type.unwrap_or(SpaceType::Register);
        let space = Space {
            src,
            space_type,
            wordsize: word_size,
            addr_bytes: addr_size,
        };
        self.spaces.push(space);
        let space_id = SpaceId(self.spaces.len() - 1);
        if default {
            self.default_space
                .replace(space_id)
                .map(|_| Err(Box::new(SleighError::SpaceMultipleDefault)))
                .unwrap_or(Ok(()))?;
        }
        self.global_scope
            .insert(input.name, GlobalScope::Space(space_id))
            .map(|_| Err(Box::new(SleighError::NameDuplicated)))
            .unwrap_or(Ok(()))
    }
}
