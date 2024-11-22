use std::cell::{Ref, RefMut};

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
pub(crate) mod len;
pub use len::*;
mod export;
pub use export::*;
mod block;
pub use block::*;
mod assignment;
pub use assignment::*;
mod user_call;
pub use user_call::*;
mod local_goto;
pub use local_goto::*;
mod cpu_branch;
pub use cpu_branch::*;
mod variables;
pub use variables::*;

#[derive(Clone, Debug)]
pub struct Execution {
    pub src: Span,
    pub variables: Vec<Variable>,
    pub blocks: Vec<Block>,

    pub return_value: TableExportType,

    //entry_block have no name and is not on self.labels
    pub entry_block: BlockId,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Delayslot(NumberUnsigned),
    Export(Export),
    CpuBranch(CpuBranch),
    LocalGoto(LocalGoto),
    UserCall(UserCall),
    Build(Build),
    Declare(VariableId),
    Assignment(Assignment),
    // NOTE this only exits to facilitate the inlining of macros
    MacroParamAssignment(MacroParamAssignment),
}

impl Statement {
    pub fn solve<T>(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut T,
    ) -> Result<(), Box<ExecutionError>>
    where
        T: SolverStatus,
    {
        match self {
            Self::Build(_x) => (),
            Self::Delayslot(_) => (),
            Self::Export(x) => x.solve(sleigh, execution, solved)?,
            Self::Declare(_x) => (),
            Self::CpuBranch(x) => x.solve(sleigh, execution, solved)?,
            Self::LocalGoto(x) => x.solve(sleigh, execution, solved)?,
            Self::UserCall(x) => x.solve(sleigh, execution, solved)?,
            Self::Assignment(x) => x.solve(sleigh, execution, solved)?,
            Self::MacroParamAssignment(x) => {
                x.solve(sleigh, execution, solved)?
            }
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
            Self::UserCall(x) => New::UserCall(x.convert()),
            Self::Build(x) => New::Build(x),
            Self::Declare(x) => New::Declare(x),
            Self::Assignment(x) => New::Assignment(x.convert()),
            Self::MacroParamAssignment(x) => New::Assignment(x.convert()),
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
            variables: vec![],
            return_value: TableExportType::None,
            entry_block: BlockId(0),
        }
    }
    pub fn solve<T: SolverStatus>(
        &mut self,
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<(), Box<ExecutionError>> {
        self.blocks
            .iter()
            .try_for_each(|block| block.solve(sleigh, self, solved))?;

        //get the export sizes, otherwise we are finished
        let Some(mut return_size) = self.return_value.size().cloned() else {
            return Ok(());
        };

        // calculate the new exported value
        let mut inputs: Vec<FieldSize> = self
            .export_statements_mut()
            .map(|x| x.output_size(sleigh, self))
            .collect();
        let modified =
            len::n_generate_a(inputs.as_mut_slice(), &mut return_size)
                .ok_or_else(|| Box::new(ExecutionError::InvalidExport))?;

        if modified {
            solved.i_did_a_thing();
            for (new_size, mut old_size) in
                inputs.into_iter().zip(self.export_statements_mut())
            {
                old_size.output_size_mut(sleigh, self).set(new_size);
            }
            *self.return_value.size_mut().unwrap() = return_size;
        }

        if return_size.is_undefined() {
            solved.iam_not_finished(&self.src, file!(), line!());
        }

        Ok(())
    }
    pub fn convert(self) -> FinalExecution {
        FinalExecution {
            variables: self
                .variables
                .into_iter()
                .map(|x| x.convert())
                .collect(),
            blocks: self
                .blocks
                .into_iter()
                .map(|block| block.convert())
                .collect(),
            entry_block: self.entry_block,
            export: self.return_value.convert(),
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
        &self.variables[id.0]
    }
    pub fn variable_mut(&mut self, id: VariableId) -> &mut Variable {
        &mut self.variables[id.0]
    }
    pub fn variable_by_name(&self, name: &str) -> Option<VariableId> {
        for (var_id, var) in self.variables.iter().enumerate() {
            if var.name == name {
                return Some(VariableId(var_id));
            }
        }
        None
    }
    pub fn create_variable(
        &mut self,
        name: String,
        src: Span,
        size: Option<FieldSize>,
        explicit: bool,
    ) -> Result<VariableId, Box<ExecutionError>> {
        //// don't allow duplicated name
        //if self.variable_by_name(&name).is_some() {
        //    return Err(Box::new(ExecutionError::InvalidRef(src)));
        //}
        //TODO src
        let var_id = self.variables.len();
        let var = Variable::new(name, src, size, explicit);
        self.variables.push(var);
        Ok(VariableId(var_id))
    }

    pub fn export_statements(&self) -> impl Iterator<Item = Ref<Export>> {
        self.blocks
            .iter()
            .filter(|block| block.next.is_none())
            .filter_map(|block| {
                let last = block.statements.last()?.borrow();
                if !matches!(&*last, Statement::Export(_)) {
                    return None;
                }
                Some(Ref::map(last, |last| {
                    let Statement::Export(exp) = last else {
                        unreachable!();
                    };
                    exp
                }))
            })
    }

    pub fn export_statements_mut(
        &self,
    ) -> impl Iterator<Item = RefMut<Export>> {
        self.blocks
            .iter()
            .filter(|block| block.next.is_none())
            .filter_map(|block| {
                let last = block.statements.last()?.borrow_mut();
                if !matches!(&*last, Statement::Export(_)) {
                    return None;
                }
                Some(RefMut::map(last, |last| {
                    let Statement::Export(exp) = last else {
                        unreachable!();
                    };
                    exp
                }))
            })
    }
}
