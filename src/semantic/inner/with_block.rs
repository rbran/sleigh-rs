use crate::{syntax, IDENT_INSTRUCTION};

//multiple with_blocks are combined into this one during the processing
#[derive(Clone, Debug, Default)]
pub(crate) struct WithBlockCurrent(
    Vec<(
        String,
        syntax::block::pattern::Pattern,
        Option<syntax::block::disassembly::Disassembly>,
    )>,
);

impl WithBlockCurrent {
    pub fn push(
        &mut self,
        with_block: syntax::block::with_block::WithBlock,
    ) -> syntax::Sleigh {
        //Inside a with block that has a table header, a nested with block may
        //specify the instruction table by name, as in
        //"with instruction : {...}". Inside such a block, the rule regarding
        //mnemonic literals is restored (see Section 7.3.1, “Mnemonic”).
        self.0.push((
            with_block.table_name().to_owned(),
            with_block.pattern,
            with_block.disassembly,
        ));
        with_block.body
    }
    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn table_name<'a>(&'a self, current_table_name: &'a str) -> &'a str {
        //From ghidra docs:
        //Note that when a with block has a table header specifying a table
        //that does not yet exist, the table is created immediately. Inside a
        //with block that has a table header, a nested with block may specify
        //the instruction table by name, as in "with instruction : {...}".
        //Inside such a block, the rule regarding mnemonic literals is restored.

        //if the name is specified, use it.
        if !current_table_name.is_empty() {
            return current_table_name;
        }

        //otherwise, if the table name is not specified, the table name will
        //depent on the block last table name, there are two possibilities:
        let with_block_table_name = self.0.last().map(|x| &x.0);
        match with_block_table_name {
            //there is a table name, so use it
            Some(name) => name,
            //there is no block, so the table default into the instruction
            //table
            None => IDENT_INSTRUCTION,
        }
    }
    fn pattern_iter(
        &self,
    ) -> impl ExactSizeIterator<Item = &syntax::block::pattern::Pattern> {
        self.0
            .iter()
            .map(|(_table_name, pattern, _disassembly)| pattern)
    }
    //Merge multiple patterns into a single one, obs: merge, not concat.
    //the last block of the first pattern is merged with the first block of
    //the second patter using the `And` operation.
    pub fn pattern(
        &self,
        constructor_pattern: &syntax::block::pattern::Pattern,
    ) -> syntax::block::pattern::Pattern {
        use syntax::block::pattern::Op;
        let patterns = self.pattern_iter().chain([constructor_pattern]);
        let block_number =
            self.pattern_iter().map(|p| p.blocks.len()).sum::<usize>()
                + constructor_pattern.blocks.len();
        let mut final_pattern = syntax::block::pattern::Pattern {
            blocks: Vec::with_capacity(block_number),
            src: constructor_pattern.src.clone(),
        };
        for pattern in patterns {
            let mut blocks = pattern.blocks.iter();
            let first_block = blocks.next();
            let last_block = final_pattern.blocks.last_mut();
            //if there is a last_block on the final_pattern and a first_block
            //on the new_pattern, merge both into a single block
            match (first_block, last_block) {
                //no blocks to merge into the final pattern, do nothing
                (None, _) => continue,
                //there is no last block to be merged with the new blocks.
                //so just add this new block into the final pattern.
                (Some(first_block), None) => {
                    final_pattern.blocks.extend([first_block.clone()]);
                }
                //merge the first_block (final_pattern) with the last_block
                //(new_pattern)
                (Some(first_block), Some(last_block)) => {
                    //if the first and second blocks have diferent OPs, make the
                    //second block a subpattern to avoid conflict.
                    if first_block.op() != last_block.op() {
                        last_block.elements.push((
                            Op::And,
                            syntax::block::pattern::Element {
                                field:
                                    syntax::block::pattern::Field::SubPattern(
                                        pattern.clone(),
                                    ),
                                ellipsis: None,
                            },
                        ));
                        continue;
                    } else {
                        //last block will include all the elements from the first block of
                        //the new pattern
                        last_block
                            .elements
                            .extend([(Op::And, first_block.first.clone())]);
                        last_block
                            .elements
                            .extend(first_block.elements.iter().cloned());
                    }
                }
            }
            //add all the other blocks without merging
            final_pattern.blocks.extend(blocks.cloned());
        }
        final_pattern
    }
    pub fn disassembly(
        &self,
        current_pattern: Option<syntax::block::disassembly::Disassembly>,
    ) -> Option<syntax::block::disassembly::Disassembly> {
        self.0
            .iter()
            .filter_map(|x| x.2.as_ref())
            .cloned()
            .chain([current_pattern].into_iter().flatten())
            .reduce(|mut acc, x| {
                acc.assertations.extend(x.assertations);
                acc
            })
    }
}
