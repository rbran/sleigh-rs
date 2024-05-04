use crate::semantic::execution::{Block as FinalBlock, BlockId};
use crate::semantic::inner::{Sleigh, SolverStatus};
use crate::ExecutionError;

use super::{Statement, Variable};

#[derive(Clone, Debug)]
pub struct Block {
    //None is entry block
    pub name: Option<Box<str>>,
    // empty means the entry point
    //parent: RefCell<Vec<Weak<Block>>>,
    // None means the block is an return
    pub next: Option<BlockId>,
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn new_empty(name: Option<Box<str>>) -> Self {
        Block {
            name,
            next: None,
            statements: vec![],
        }
    }

    pub fn solve<T: SolverStatus>(
        &mut self,
        sleigh: &Sleigh,
        variables: &[Variable],
        solved: &mut T,
    ) -> Result<(), Box<ExecutionError>> {
        self.statements.iter_mut().try_for_each(|statements| {
            statements.solve(sleigh, variables, solved)
        })
    }

    pub fn convert(self) -> FinalBlock {
        let statements = self
            .statements
            .into_iter()
            .map(|statement| statement.convert())
            .collect();
        let statements = statements;
        FinalBlock {
            name: self.name,
            next: self.next,
            statements,
        }
    }
}
