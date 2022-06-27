use std::rc::Rc;

use crate::semantic::WithBlockError;
use crate::syntax;

use super::disassembly::{Disassembly, DisassemblyBuilder};
use super::table::disassembly::Builder;
use super::{Pattern, Sleigh, Table};

#[derive(Clone, Debug)]
pub struct WithBlock {
    pub table: Rc<Table>,
    pub pattern: Pattern,
    pub disassembly: Disassembly,
}

impl WithBlock {
    pub(crate) fn new<'a>(
        sleigh: &mut Sleigh<'a>,
        with_block: &Option<WithBlock>,
        input: syntax::block::with_block::WithBlock<'a>,
    ) -> Result<Self, WithBlockError> {
        let table = sleigh
            .get_table_or_create_empty(input.table_name())
            .ok_or(WithBlockError::TableName)?;

        // use the with_block pattern or a default one
        let mut pattern = with_block
            .as_ref()
            .map(|with_block| with_block.pattern.clone())
            .unwrap_or_default();
        pattern.extend(sleigh, input.pattern)?;

        let disassembly = {
            // use the with_block disassembly or a default one
            let mut disassembly = with_block
                .as_ref()
                .map(|with_block| with_block.disassembly.clone())
                .unwrap_or_default();
            let mut builder = Builder::new(sleigh, &mut disassembly);
            if let Some(input) = input.dissasembly {
                DisassemblyBuilder::extend(&mut builder, input)?;
            }
            disassembly
        };

        Ok(Self {
            table,
            pattern,
            disassembly,
        })
    }
}
