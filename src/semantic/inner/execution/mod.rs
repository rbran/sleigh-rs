use crate::semantic::execution::{
    BlockId, BranchCall, Build, Execution as FinalExecution,
    Statement as FinalStatement, VariableId,
};
use crate::{ExecutionError, NumberUnsigned, Span};

use super::{Sleigh, SolverStatus};

mod builder;
pub use builder::*;
mod expr;
pub use expr::*;
mod op;
pub use op::*;
mod len;
pub use len::*;
mod export;
pub use export::*;
mod mem_write;
pub use mem_write::*;
mod block;
pub use block::*;
mod assignment;
pub use assignment::*;
mod macro_call;
pub use macro_call::*;
mod user_call;
pub use user_call::*;
mod local_goto;
pub use local_goto::*;
mod cpu_branch;
pub use cpu_branch::*;
mod variables;
pub use variables::*;
mod write_value;
//pub use write_value::*;

#[derive(Clone, Debug)]
pub struct Execution {
    pub src: Span,
    pub blocks: Vec<Block>,
    pub vars: Vec<Variable>,

    pub return_value: ExportLen,

    //entry_block have no name and is not on self.labels
    pub entry_block: BlockId,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Delayslot(NumberUnsigned),
    Export(Export),
    CpuBranch(CpuBranch),
    LocalGoto(LocalGoto),
    MacroCall(MacroCall),
    UserCall(UserCall),
    Build(Build),
    Declare(VariableId),
    Assignment(Assignment),
    MemWrite(MemWrite),
}

impl Statement {
    pub fn solve<T>(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut T,
    ) -> Result<(), Box<ExecutionError>>
    where
        T: SolverStatus + Default,
    {
        match self {
            Self::Build(_x) => (),
            Self::Delayslot(_) => (),
            Self::Export(x) => x.solve(sleigh, variables, solved)?,
            Self::Declare(_x) => (),
            Self::CpuBranch(x) => x.solve(sleigh, variables, solved)?,
            Self::LocalGoto(x) => x.solve(sleigh, variables, solved)?,
            Self::MacroCall(x) => x.solve(sleigh, variables, solved)?,
            Self::UserCall(x) => x.solve(sleigh, variables, solved)?,
            Self::Assignment(x) => x.solve(sleigh, variables, solved)?,
            Self::MemWrite(x) => x.solve(sleigh, variables, solved)?,
        }
        Ok(())
    }
    pub fn convert(self) -> FinalStatement {
        use FinalStatement as New;
        match self {
            Self::Delayslot(x) => New::Delayslot(x),
            Self::Export(x) => New::Export(x.convert()),
            Self::CpuBranch(x) => New::CpuBranch(x.convert()),
            Self::LocalGoto(x) => New::LocalGoto(x.convert()),
            Self::MacroCall(x) => New::MacroCall(x.convert()),
            Self::UserCall(x) => New::UserCall(x.convert()),
            Self::Build(x) => New::Build(x),
            Self::Declare(x) => New::Declare(x),
            Self::Assignment(x) => New::Assignment(x.convert()),
            Self::MemWrite(x) => New::MemWrite(x.convert()),
        }
    }
}

impl Execution {
    pub fn src(&self) -> &Span {
        &self.src
    }
    pub fn new_empty(src: Span) -> Self {
        let entry_block = Block::new_empty(None);
        Execution {
            src,
            blocks: vec![entry_block],
            vars: vec![],
            return_value: ExportLen::default(),
            entry_block: BlockId(0),
        }
    }
    pub fn solve<T>(
        &mut self,
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<(), Box<ExecutionError>>
    where
        T: SolverStatus + Default,
    {
        self.blocks
            .iter_mut()
            .try_for_each(|block| block.solve(sleigh, &self.vars, solved))?;

        //get the export sizes, otherwise we are finished
        let mut return_size =
            if let Some(size) = self.return_value.size().cloned() {
                size
            } else {
                return Ok(());
            };
        //find and combine all the output sizes
        let mut modified = self
            .blocks
            .iter()
            .filter(|block| block.next.is_none())
            .filter_map(|block| match block.statements.last()? {
                Statement::Export(exp) => Some(exp.output_size(sleigh, self)),
                _ => None,
            })
            .try_fold(false, |acc, out_size| {
                return_size
                    .update_action(|size| size.intersection(out_size))
                    .map(|modified| acc | modified)
                    .ok_or_else(|| Box::new(ExecutionError::InvalidExport))
            })?;
        //update all the export output sizes
        self.blocks
            .iter_mut()
            .filter(|block| block.next.is_none())
            .for_each(|block| {
                let statements = &mut block.statements;
                if let Some(Statement::Export(export)) = statements.last_mut() {
                    modified |= export
                        .output_size_mut(sleigh, &self.vars)
                        .update_action(|size| size.intersection(return_size))
                        .unwrap();
                }
            });
        modified |= self
            .return_value
            .size_mut()
            .unwrap()
            .update_action(|size| size.intersection(return_size))
            .unwrap();
        if modified {
            solved.i_did_a_thing();
        }
        if return_size.is_undefined() {
            solved.iam_not_finished(&self.src, file!(), line!());
        }
        Ok(())
    }
    pub fn convert(self) -> FinalExecution {
        FinalExecution {
            blocks: self
                .blocks
                .into_iter()
                .map(|block| block.convert())
                .collect(),
            variables: self.vars.into_iter().map(|var| var.convert()).collect(),
            entry_block: self.entry_block,
        }
    }
    pub fn block(&self, id: BlockId) -> &Block {
        &self.blocks[id.0]
    }
    pub fn block_mut(&mut self, id: BlockId) -> &mut Block {
        &mut self.blocks[id.0]
    }
    pub fn block_by_name(&self, name: &str) -> Option<BlockId> {
        self.blocks
            .iter()
            .position(|block| {
                block
                    .name
                    .as_ref()
                    .map(|block_name| block_name.as_ref() == name)
                    .unwrap_or(false)
            })
            .map(BlockId)
    }
    pub fn new_block(&mut self, name: String) -> Option<()> {
        if self.block_by_name(&name).is_some() {
            return None;
        }
        let block = Block::new_empty(Some(name.into()));
        self.blocks.push(block);
        Some(())
    }
    pub fn variable(&self, id: VariableId) -> &Variable {
        &self.vars[id.0]
    }
    pub fn variable_mut(&mut self, id: VariableId) -> &mut Variable {
        &mut self.vars[id.0]
    }
    pub fn variable_by_name(&self, name: &str) -> Option<VariableId> {
        self.vars
            .iter()
            .position(|vars| vars.name == name)
            .map(VariableId)
    }
    pub fn create_variable(
        &mut self,
        name: String,
        src: Span,
        explicit: bool,
    ) -> Result<VariableId, Box<ExecutionError>> {
        // don't allow duplicated name
        if self.variable_by_name(&name).is_some() {
            return Err(Box::new(ExecutionError::InvalidRef(src)));
        }
        //TODO src
        let var = Variable::new(name, explicit.then_some(src));
        self.vars.push(var);
        Ok(VariableId(self.vars.len() - 1))
    }
}
