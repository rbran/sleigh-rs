use crate::{syntax, IDENT_INSTRUCTION, semantic::table::PatternError};

//multiple with_blocks are combined into this one during the processing
#[derive(Clone, Debug, Default)]
pub(crate) struct WithBlockCurrent<'a>(
    Vec<(
        &'a str,
        syntax::block::pattern::Pattern<'a>,
        Option<syntax::block::disassembly::Disassembly<'a>>,
    )>,
);

impl<'a> WithBlockCurrent<'a> {
    pub fn push(
        &mut self,
        with_block: syntax::block::with_block::WithBlock<'a>,
    ) -> syntax::Syntax<'a> {
        //Inside a with block that has a table header, a nested with block may
        //specify the instruction table by name, as in
        //"with instruction : {...}". Inside such a block, the rule regarding
        //mnemonic literals is restored (see Section 7.3.1, “Mnemonic”).
        self.0.push((
            with_block.table_name(),
            with_block.pattern,
            with_block.dissasembly,
        ));
        with_block.body
    }
    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn table_name(&self, current_table_name: &'a str) -> &'a str {
        //From ghidra docs:
        //Note that when a with block has a table header specifying a table
        //that does not yet exist, the table is created immediately. Inside a
        //with block that has a table header, a nested with block may specify
        //the instruction table by name, as in "with instruction : {...}".
        //Inside such a block, the rule regarding mnemonic literals is restored.

        //if the name is specified, use it.
        if current_table_name != "" {
            return current_table_name;
        }

        //otherwise, if the table name is not specified, the table name will
        //depent on the block last table name, there are two possibilities:
        let with_block_table_name = self.0.last().map(|x| x.0);
        match with_block_table_name {
            //there is a table name, so use it
            Some(name) => name,
            //there is no block, so the table default into the instruction
            //table
            None => IDENT_INSTRUCTION,
        }
    }
    pub fn pattern(
        &self,
        current_pattern: syntax::block::pattern::Pattern<'a>,
    ) -> Result<syntax::block::pattern::Pattern<'a>, PatternError> {
        //FUTURE use try_reduce instead
        let mut patterns = self
            .0
            .iter()
            .map(|x| &x.1)
            .cloned()
            .chain([current_pattern]);
        let first = patterns.next().unwrap();

        //Merge multiple patterns into a single one, obs: merge, not concat.
        //the last block of the first pattern is fuzed with the first block of
        //the second patter.
        patterns.try_fold(first, |mut x, mut y| {
            use syntax::block::pattern::{Block, Op};
            //if one of the patterns are empty, just return the other
            if y.blocks.len() == 0 {
                return Ok(x);
            }
            if x.blocks.len() == 0 {
                return Ok(y);
            }

            let mut y = y.blocks.drain(..);
            let first_block = y.next().unwrap();
            let last_block = x.blocks.pop().unwrap();

            //the output operation, need to be `&` or `|`, combine patterns with
            //diferent operations is invalid
            let op = match (first_block.op, last_block.op) {
                (Some(Op::And), Some(Op::Or))
                | (Some(Op::Or), Some(Op::And)) => {
                    //TODO error
                    todo!("Unable to combine patterns");
                }
                //If we have two blocks with only on element, consequently no
                //operation (op == None), default to `and`.
                (x, y) => x.or(y).unwrap_or(Op::And),
            };
            let mut elements = first_block.elements;
            elements.extend(last_block.elements);

            let merged_block = Block { op: Some(op), elements };
            x.blocks.push(merged_block);
            x.blocks.extend(y);
            Ok(x)
        })
    }
    pub fn disassembly(
        &self,
        current_pattern: Option<syntax::block::disassembly::Disassembly<'a>>,
    ) -> Option<syntax::block::disassembly::Disassembly<'a>> {
        self.0
            .iter()
            .filter_map(|x| x.2.as_ref())
            .cloned()
            .chain([current_pattern].into_iter().filter_map(|x| x))
            .reduce(|mut acc, x| {
                acc.assertations.extend(x.assertations);
                acc
            })
    }
}
