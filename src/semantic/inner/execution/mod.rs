use std::cell::{Cell, Ref, RefCell, RefMut};

use crate::semantic::execution::{
    Assignment as FinalAssignment, Block as FinalBlock, BlockId, BranchCall,
    Build, CpuBranch as FinalCpuBranch, Execution as FinalExecution,
    Export as FinalExport, ExportConst, LocalGoto as FinalLocalGoto,
    MacroCall as FinalMacroCall, MemWrite as FinalMemWrite,
    Statement as FinalStatement, Unary, UserCall as FinalUserCall,
    Variable as FinalVariable, VariableId, WriteValue,
};
use crate::semantic::table::ExecutionExport;
use crate::semantic::{PcodeMacroId, UserFunctionId};
use crate::{
    ExecutionError, Number, NumberNonZeroUnsigned, NumberUnsigned, Span,
};

use super::pcode_macro::PcodeMacroCallId;
use super::{Sleigh, SolverStatus};

mod builder;
pub use builder::*;
mod expr;
pub use expr::*;
mod op;
pub use op::*;
mod len;
pub use len::*;

#[derive(Clone, Debug)]
pub struct Execution {
    pub src: Span,
    pub blocks: RefCell<Vec<Block>>,
    pub vars: Vec<Variable>,

    pub return_value: ExportLen,

    //entry_block have no name and is not on self.labels
    pub entry_block: BlockId,
}

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

#[derive(Clone, Debug)]
pub enum Export {
    Const {
        len_bits: FieldSize,
        location: Span,
        export: ExportConst,
    },
    Value(Expr),
    Reference {
        addr: Expr,
        memory: MemoryLocation,
    },
}

#[derive(Clone, Debug)]
pub struct MemWrite {
    pub addr: Expr,
    mem: MemoryLocation,
    src: Span,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub var: WriteValue,
    op: Option<Truncate>,
    src: Span,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub struct MacroCall {
    instance: PcodeMacroCallId,
    pub params: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct UserCall {
    pub location: Span,
    pub output_size: FieldSize,
    pub function: UserFunctionId,
    pub params: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct LocalGoto {
    pub cond: Option<Expr>,
    pub dst: BlockId,
}
#[derive(Clone, Debug)]
pub struct CpuBranch {
    pub cond: Option<Expr>,
    pub call: BranchCall,
    //TODO delete direct?
    direct: bool,
    pub dst: Expr,
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

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    //pub scope: RefCell<VariableScope>,
    pub size: Cell<FieldSize>,
    ///location of the variable declaration, if declared explicitly
    /// NOTE: PcodeMacro Params src is located in `Parameter`
    pub src: Option<Span>,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ExportLen {
    //don't return
    #[default]
    None,
    //type can't be defined yet.
    //Undefined(FieldSize),
    //value that is known at Dissassembly time
    Const(FieldSize),
    //value that can be know at execution time
    Value(FieldSize),
    //References/registers and other mem locations, all with the same size
    Reference(FieldSize),
    //multiple source, can by any kind of return, value or address,
    //but all with the same size
    Multiple(FieldSize),
}

impl Export {
    pub fn new_value(expr: Expr) -> Self {
        Self::Value(expr)
    }
    pub fn new_const(size: FieldSize, src: Span, value: ExportConst) -> Self {
        Self::Const {
            len_bits: size,
            location: src,
            export: value,
        }
    }
    pub fn new_reference(
        sleigh: &Sleigh,
        execution: &Execution,
        mut addr: Expr,
        space_deref: MemoryLocation,
    ) -> Self {
        //addr expr is the addr to access the space, so it need to be space
        //addr size
        let space = sleigh.space(space_deref.space);
        addr.size_mut(sleigh, execution)
            .update_action(|size| {
                size.intersection(
                    FieldSize::new_bytes(space.addr_bytes)
                )
            })
            .unwrap(/*TODO*/);
        Self::Reference {
            addr,
            memory: space_deref,
        }
    }
    pub fn return_type(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> ExportLen {
        match self {
            Export::Value(value) => {
                ExportLen::Value(value.size(sleigh, execution))
            }
            Export::Reference { addr: _, memory } => {
                ExportLen::Reference(memory.size)
            }
            Export::Const {
                len_bits: len,
                location: _,
                export: _,
            } => ExportLen::Const(*len),
        }
    }
    pub fn src(&self) -> &Span {
        match self {
            Self::Value(expr) | Export::Reference { addr: expr, .. } => {
                expr.src()
            }
            Self::Const { location, .. } => location,
        }
    }
    pub fn output_size(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> FieldSize {
        match self {
            Self::Value(expr) => expr.size(sleigh, execution),
            //TODO verify this
            Self::Reference { addr: _, memory } => memory.size,
            Self::Const { len_bits: len, .. } => *len,
        }
    }
    pub fn output_size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Value(expr) => expr.size_mut(sleigh, execution),
            //TODO verify this
            Self::Reference { addr: _, memory } => {
                Box::new(FieldSizeMutRef::from(&mut memory.size))
            }
            Self::Const { len_bits, .. } => {
                Box::new(FieldSizeMutRef::from(len_bits))
            }
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        match self {
            Self::Const { .. } => Ok(()),
            Self::Value(expr) => expr.solve(sleigh, execution, solved),
            Self::Reference { addr, memory } => {
                addr.solve(sleigh, execution, solved)?;
                memory.solve(solved);
                if addr.size(sleigh, execution).is_undefined() {
                    solved.iam_not_finished_location(
                        addr.src(),
                        file!(),
                        line!(),
                    );
                }
                Ok(())
            }
        }
    }
    pub fn convert(self) -> FinalExport {
        match self {
            Self::Const {
                len_bits,
                location,
                export,
            } => FinalExport::Const {
                len_bits: len_bits.possible_value().unwrap(),
                location,
                export,
            },
            Self::Value(expr) => FinalExport::Value(expr.convert()),
            Self::Reference { addr, memory } => FinalExport::Reference {
                addr: addr.convert(),
                memory: memory.convert(),
            },
        }
    }
}

impl MemWrite {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut addr: Expr,
        mem: MemoryLocation,
        src: Span,
        mut right: Expr,
    ) -> Self {
        //HACK: unexplained exeptions:
        //if the mem size is 1byte and value produced by right is 1bit, auto
        //zext it
        if mem
            .size
            .final_value()
            .map(|size| size.get() == 8)
            .unwrap_or(false)
            && right
                .size(sleigh, execution)
                .final_value()
                .map(|size| size.get() == 1)
                .unwrap_or(false)
        {
            right = Expr::Value(ExprElement::new_op(
                right.src().clone(),
                Unary::Zext,
                right,
            ))
        }

        //addr expr is the addr to access the space, so it need to be space
        //addr size
        let space = sleigh.space(mem.space);
        addr.size_mut(sleigh, execution)
            .set(FieldSize::new_bytes(space.addr_bytes));
        Self {
            addr,
            mem,
            src,
            right,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        self.right.solve(sleigh, execution, solved)?;
        //exception in case the right size is smaller then the left size,
        //truncate the right size with a msb(0)
        let left_size = self.mem.size.final_value();
        let right_size = self.right.size(sleigh, execution).final_value();
        if left_size
            .zip(right_size)
            .map(|(left, right)| left.get() < right.get())
            .unwrap_or(false)
        {
            //dummy value
            let dummy_src = Span::File(crate::FileSpan {
                start: crate::FileLocation {
                    file: std::rc::Rc::from(std::path::Path::new("")),
                    line: 0,
                    column: 0,
                },
                end_line: 0,
                end_column: 0,
            });
            let mut taken =
                Expr::Value(ExprElement::Value(ReadValue::Int(ExprNumber {
                    location: dummy_src,
                    size: FieldSize::default(),
                    number: Number::Positive(0),
                })));
            std::mem::swap(&mut self.right, &mut taken);
            self.right = Expr::Value(ExprElement::Truncate(
                taken.src().clone(),
                Truncate::new(0, left_size.unwrap()),
                Box::new(taken),
            ));
            self.right.solve(sleigh, execution, solved)?;
            solved.i_did_a_thing()
        }

        //if the left side is WriteAddr without a size, the only option is to
        //use the left side size
        if let Some(write_size) = self.mem.size.final_value() {
            //if left side size is known, right side will need to produce a
            //a value with size equal or smaller then that
            let modified = self
                .right
                .size_mut(sleigh, execution)
                .update_action(|size| {
                    size.set_max(write_size)?.set_possible_value(write_size)
                });
            if modified
                .ok_or_else(|| ExecutionError::VarSize(self.mem.src.clone()))?
            {
                solved.i_did_a_thing();
            }
        } else {
            //if the left side is WriteAddr without a size, the only option is
            //to use the right side size
            let modified = self.mem.size.update_action(|size| {
                size.intersection(self.right.size(sleigh, execution))
            });
            if modified
                .ok_or_else(|| ExecutionError::VarSize(self.src.clone()))?
            {
                solved.i_did_a_thing();
            }
        }

        if self.addr.size(sleigh, execution).is_undefined()
            || self.right.size(sleigh, execution).is_undefined()
        {
            solved.iam_not_finished_location(self.right.src(), file!(), line!())
        }

        Ok(())
    }
    pub fn convert(self) -> FinalMemWrite {
        FinalMemWrite {
            addr: self.addr.convert(),
            mem: self.mem.convert(),
            right: self.right.convert(),
        }
    }
}

impl Assignment {
    pub fn new(
        var: WriteValue,
        op: Option<Truncate>,
        src: Span,
        right: Expr,
    ) -> Self {
        Self {
            var,
            op,
            src,
            right,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let error_src = self.right.src().clone();
        let error = || ExecutionError::VarSize(error_src.clone());

        self.right.solve(sleigh, execution, solved)?;

        //exception in case the right result is 1 bit and the left is
        //less then 1 byte, auto add zext the right side
        let left_size = self
            .op
            .as_ref()
            .map(|op| op.output_size())
            .unwrap_or_else(|| self.var.size(sleigh, execution))
            .final_value();
        let right_size = self.right.size(sleigh, execution).final_value();
        if left_size
            .zip(right_size)
            .map(|(left, right)| left.get() == 8 && right.get() < 8)
            .unwrap_or(false)
        {
            //dummy value
            let dummy_src = Span::File(crate::FileSpan {
                start: crate::FileLocation {
                    file: std::rc::Rc::from(std::path::Path::new("")),
                    line: 0,
                    column: 0,
                },
                end_line: 0,
                end_column: 0,
            });
            let mut taken =
                Expr::Value(ExprElement::Value(ReadValue::Int(ExprNumber {
                    location: dummy_src,
                    size: FieldSize::default(),
                    number: Number::Positive(0),
                })));
            std::mem::swap(&mut self.right, &mut taken);
            self.right = Expr::Value(ExprElement::Op(ExprUnaryOp {
                location: taken.src().clone(),
                output_size: FieldSize::new_bits(left_size.unwrap()),
                op: Unary::Zext,
                input: Box::new(taken),
            }));
            self.right.solve(sleigh, execution, solved)?;
            solved.i_did_a_thing()
        }

        //left and right sizes are the same
        if let Some(trunc) = &mut self.op {
            let modified = [
                &mut self.right.size_mut(sleigh, execution)
                    as &mut dyn FieldSizeMut,
                &mut FieldSizeMutRef::from(trunc.output_size_mut()),
            ]
            .all_same_lenght();
            if modified.ok_or_else(error)? {
                solved.i_did_a_thing()
            }
        } else {
            let modified = [
                &mut self.right.size_mut(sleigh, execution)
                    as &mut dyn FieldSizeMut,
                &mut self.var.size_mut(sleigh, execution)
                    as &mut dyn FieldSizeMut,
            ]
            .all_same_lenght();

            //if right size is possible min, so does left if size is not defined
            if self.var.size(sleigh, execution).is_undefined()
                && self.right.size(sleigh, execution).possible_min()
            {
                if self
                    .var
                    .size_mut(sleigh, execution)
                    .update_action(|size| Some(size.set_possible_min()))
                    .unwrap()
                {
                    solved.i_did_a_thing();
                }
            }
            if modified.ok_or_else(error)? {
                solved.i_did_a_thing()
            }
        }

        if self.var.size(sleigh, execution).is_undefined()
            || self.right.size(sleigh, execution).is_undefined()
        {
            solved.iam_not_finished_location(self.right.src(), file!(), line!())
        }

        Ok(())
    }
    pub fn convert(self) -> FinalAssignment {
        FinalAssignment {
            location: self.src,
            var: self.var,
            op: self.op.map(|op| op.convert()),
            right: self.right.convert(),
        }
    }
}

impl WriteValue {
    pub(crate) fn size(
        &self,
        sleigh: &Sleigh,
        execution: &Execution,
    ) -> FieldSize {
        match self {
            Self::Varnode(var) => {
                FieldSize::new_bytes(sleigh.varnode(var.id).len_bytes)
            }
            Self::Bitrange(var) => {
                FieldSize::new_bits(sleigh.bitrange(var.id).bits.len())
            }
            Self::TableExport(value) => {
                let export = sleigh.table(value.id).export.borrow();
                *export.as_ref().unwrap().size().unwrap()
            }
            Self::TokenField(ass) => {
                sleigh.token_field(ass.id).exec_value_len(sleigh)
            }
            Self::Local(var) => execution.variable(var.id).size.get(),
        }
    }
    pub fn size_mut<'a>(
        &'a mut self,
        sleigh: &'a Sleigh,
        execution: &'a Execution,
    ) -> Box<dyn FieldSizeMut + 'a> {
        match self {
            Self::Varnode(var) => Box::new(FieldSizeMutOwned::from(
                FieldSize::new_bytes(sleigh.varnode(var.id).len_bytes),
            )),
            Self::Bitrange(var) => Box::new(FieldSizeMutOwned::from(
                FieldSize::new_bits(sleigh.bitrange(var.id).bits.len()),
            )),
            Self::TokenField(ass) => Box::new(FieldSizeMutOwned::from(
                sleigh.token_field(ass.id).exec_value_len(sleigh),
            )),
            Self::TableExport(table) => Box::new(sleigh.table(table.id)),
            Self::Local(local) => Box::new(&execution.variable(local.id).size),
        }
    }
}

impl MacroCall {
    pub fn new(params: Vec<Expr>, macro_id: PcodeMacroId) -> Self {
        Self {
            params,
            instance: PcodeMacroCallId {
                macro_id,
                instance_id: None,
            },
        }
    }
    pub fn solve<T>(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut T,
    ) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        // if already specialized, then finished
        if self.instance.instance_id.is_some() {
            return Ok(());
        }

        //otherwise try to specialize the macro call
        //update the param expected size with the macro expected param size
        //and solved it
        let params_iter = sleigh
            .pcode_macro(self.instance.macro_id)
            .params
            .iter()
            .map(|param| execution.variable(param.variable_id));
        for (param, macro_param) in self.params.iter_mut().zip(params_iter) {
            let src = param.src().clone();
            if param
                .size_mut(sleigh, execution)
                .update_action(|size| size.intersection(macro_param.size.get()))
                .ok_or(ExecutionError::VarSize(src))?
            {
                solved.we_did_a_thing();
            }
            param.solve(sleigh, execution, solved)?;
        }

        //try to specialize the macro call
        let params_size = self
            .params
            .iter()
            .map(|x| x.size(sleigh, execution).final_value())
            .collect::<Option<Vec<_>>>();
        if let Some(params_size) = params_size {
            let pcode_macro = sleigh.pcode_macro(self.instance.macro_id);
            self.instance.instance_id = Some(
                pcode_macro.specialize(&params_size).map_err(|_| todo!())?,
            );
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(self) -> FinalMacroCall {
        let params = self.params.into_iter().map(|x| x.convert()).collect();
        FinalMacroCall {
            params,
            function: self.instance.convert(),
        }
    }
}

impl UserCall {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut params: Vec<Expr>,
        function: UserFunctionId,
        location: Span,
    ) -> Self {
        //TODO how to handle user functions with variable number of parameter???
        //function.set_param_num(params.len()).unwrap(/*TODO*/);

        //params size is not very relevant
        params.iter_mut().for_each(|param| {
            //TODO improve the size speculation
            param
                .size_mut(sleigh, execution)
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        Self {
            params,
            output_size: FieldSize::new_unsized(),
            function,
            location,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        self.params
            .iter_mut()
            .map(|x| x.solve(sleigh, execution, solved))
            .collect::<Result<_, _>>()
    }
    pub fn convert(self) -> FinalUserCall {
        let params = self.params.into_iter().map(|x| x.convert()).collect();
        FinalUserCall {
            location: self.location,
            function: self.function,
            params,
        }
    }
}

impl LocalGoto {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut cond: Option<Expr>,
        dst: BlockId,
    ) -> Result<Self, ExecutionError> {
        //condition can have any size, preferencially 1 bit for true/false
        cond.iter_mut().for_each(|cond| {
            cond.size_mut(sleigh, execution)
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        Ok(LocalGoto { cond, dst })
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        if let Some(cond) = self.cond.as_mut() {
            cond.solve(sleigh, execution, solved)?;
            if cond.size(sleigh, execution).is_undefined() {
                solved.iam_not_finished_location(cond.src(), file!(), line!());
            }
        }
        Ok(())
    }

    pub fn convert(self) -> FinalLocalGoto {
        let cond = self.cond.map(|x| x.convert());
        let dst = self.dst;
        FinalLocalGoto { cond, dst }
    }
}

impl CpuBranch {
    pub fn new(
        sleigh: &Sleigh,
        execution: &Execution,
        mut cond: Option<Expr>,
        call: BranchCall,
        direct: bool,
        dst: Expr,
    ) -> Self {
        //condition can have any size, preferencially 1 bit for true/false
        cond.iter_mut().for_each(|cond| {
            cond.size_mut(sleigh, execution)
                .update_action(|size| Some(size.set_possible_min()))
                .unwrap();
        });
        CpuBranch {
            cond,
            call,
            direct,
            dst,
        }
    }
    pub fn solve(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut impl SolverStatus,
    ) -> Result<(), ExecutionError> {
        let mut modified = false;
        if let Some(cond) = self.cond.as_mut() {
            cond.solve(sleigh, execution, solved)?;
            if cond.size(sleigh, execution).is_undefined() {
                solved.iam_not_finished_location(cond.src(), file!(), line!());
            }
        }
        //correlate the addr execution size and the jmp dest addr size
        let error = ExecutionError::VarSize(self.dst.src().clone());
        modified |= [
            &mut self.dst.size_mut(sleigh, execution) as &mut dyn FieldSizeMut,
            &mut FieldSizeMutOwned::from(FieldSize::new_bytes(
                sleigh.addr_bytes().unwrap(/*todo*/),
            )),
        ]
        .all_same_lenght()
        .ok_or_else(|| error)?;

        self.dst.solve(sleigh, execution, solved)?;
        if self.dst.size(sleigh, execution).is_undefined() {
            solved.iam_not_finished_location(self.dst.src(), file!(), line!());
        }

        if modified {
            solved.i_did_a_thing();
        }
        Ok(())
    }
    pub fn convert(self) -> FinalCpuBranch {
        let cond = self.cond.map(|cond| cond.convert());
        let dst = self.dst.convert();
        FinalCpuBranch {
            cond,
            call: self.call,
            direct: self.direct,
            dst,
        }
    }
}

impl Statement {
    pub fn solve<T>(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut T,
    ) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        match self {
            Self::Build(_x) => (),
            Self::Delayslot(_) => (),
            Self::Export(x) => x.solve(sleigh, execution, solved)?,
            Self::Declare(_x) => (),
            Self::CpuBranch(x) => x.solve(sleigh, execution, solved)?,
            Self::LocalGoto(x) => x.solve(sleigh, execution, solved)?,
            Self::MacroCall(x) => x.solve(sleigh, execution, solved)?,
            Self::UserCall(x) => x.solve(sleigh, execution, solved)?,
            Self::Assignment(x) => x.solve(sleigh, execution, solved)?,
            Self::MemWrite(x) => x.solve(sleigh, execution, solved)?,
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

impl Block {
    pub fn new_empty(name: Option<Box<str>>) -> Self {
        Block {
            name,
            next: None,
            statements: vec![],
        }
    }

    pub fn solve<T>(
        &mut self,
        sleigh: &Sleigh,
        execution: &Execution,
        solved: &mut T,
    ) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        self.statements
            .iter_mut()
            .map(|statements| statements.solve(sleigh, execution, solved))
            .collect::<Result<(), _>>()
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

impl Variable {
    pub fn new(name: String, src: Option<Span>) -> Self {
        Self {
            name,
            //scope,
            size: Cell::new(FieldSize::new_unsized()),
            src,
        }
    }
    fn convert(self) -> FinalVariable {
        FinalVariable {
            name: self.name.into(),
            len_bits: self.size.get().possible_value().unwrap(),
            location: self.src,
        }
    }
}

impl ExportLen {
    pub fn export_nothing(&self) -> bool {
        matches!(self, Self::None)
    }
    pub fn size(&self) -> Option<&FieldSize> {
        match self {
            Self::None => None,
            Self::Const(size)
            | Self::Value(size)
            | Self::Reference(size)
            | Self::Multiple(size) => Some(size),
        }
    }
    pub fn size_mut(&mut self) -> Option<&mut FieldSize> {
        match self {
            Self::None => None,
            Self::Const(size)
            | Self::Value(size)
            | Self::Reference(size)
            | Self::Multiple(size) => Some(size),
        }
    }
    pub fn combine(self, other: Self) -> Option<Self> {
        match (self, other) {
            //if both return nothing, the result is to return nothing
            (Self::None, Self::None) => Some(Self::None),
            //two const values, keep the type
            (Self::Const(one), Self::Const(other)) => {
                one.intersection(other).map(Self::Const)
            }
            //const and value, become value
            (Self::Const(con), Self::Value(value))
            | (Self::Value(value), Self::Const(con)) => {
                con.intersection(value).map(Self::Value)
            }
            //const can't be combined with anything else
            (Self::Const(_), _) | (_, Self::Const(_)) => None,
            //one return nothing and the other return something is invalid
            (Self::None, _) | (_, Self::None) => None,
            //if one is multiple, it consumes any other value type
            (Self::Multiple(one), other) | (other, Self::Multiple(one)) => {
                one.intersection(*other.size().unwrap()).map(Self::Multiple)
            }
            //two values, keep the same type
            (Self::Value(one), Self::Value(other)) => {
                one.intersection(other).map(Self::Value)
            }
            //two references from the same address space, keep the same type
            (Self::Reference(one), Self::Reference(other)) => {
                one.intersection(other).map(Self::Reference)
            }
            //any other combination is is just multiple
            (one, other) => one
                .size()
                .unwrap()
                .intersection(*other.size().unwrap())
                .map(Self::Multiple),
        }
    }
    //TODO from Option into a Result
    pub fn final_size(&self) -> Option<NumberNonZeroUnsigned> {
        match self {
            Self::None => None,
            Self::Const(len) => Some(len.possible_value().unwrap()),
            Self::Value(len) => Some(len.possible_value().unwrap()),
            Self::Reference(len) => Some(len.possible_value().unwrap()),
            Self::Multiple(len) => Some(len.possible_value().unwrap()),
        }
    }
    pub fn convert(self) -> ExecutionExport {
        match self {
            ExportLen::None => ExecutionExport::None,
            ExportLen::Const(x) => {
                ExecutionExport::Const(x.possible_value().unwrap())
            }
            ExportLen::Value(x) => {
                ExecutionExport::Value(x.possible_value().unwrap())
            }
            ExportLen::Reference(x) => {
                ExecutionExport::Reference(x.possible_value().unwrap())
            }
            ExportLen::Multiple(x) => {
                ExecutionExport::Multiple(x.possible_value().unwrap())
            }
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
            blocks: RefCell::new(vec![entry_block]),
            vars: vec![],
            return_value: ExportLen::default(),
            entry_block: BlockId(0),
        }
    }
    pub fn solve<T>(
        &mut self,
        sleigh: &Sleigh,
        solved: &mut T,
    ) -> Result<(), ExecutionError>
    where
        T: SolverStatus + Default,
    {
        self.blocks
            .borrow_mut()
            .iter_mut()
            .map(|blocks| blocks.solve(sleigh, self, solved))
            .collect::<Result<(), _>>()?;

        //get the export sizes, otherwise we are finished
        let mut return_size = if let Some(size) = self.return_size().cloned() {
            size
        } else {
            return Ok(());
        };
        //find and combine all the output sizes
        let mut modified = self
            .blocks
            .borrow()
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
                    .ok_or_else(|| ExecutionError::InvalidExport)
            })?;
        //update all the export output sizes
        self.blocks
            .borrow_mut()
            .iter_mut()
            .filter(|block| block.next.is_none())
            .for_each(|block| {
                let statements = &mut block.statements;
                match statements.last_mut() {
                    Some(Statement::Export(export)) => {
                        modified |= export
                            .output_size_mut(sleigh, self)
                            .update_action(|size| {
                                size.intersection(return_size)
                            })
                            .unwrap();
                    }
                    _ => (),
                }
            });
        modified |= self
            .return_size_mut()
            .unwrap()
            .update_action(|size| size.intersection(return_size))
            .unwrap();
        if modified {
            solved.i_did_a_thing();
        }
        if return_size.is_undefined() {
            solved.iam_not_finished_location(&self.src, file!(), line!());
        }
        Ok(())
    }
    pub fn convert(self) -> FinalExecution {
        let blocks = self.blocks.take();
        FinalExecution {
            blocks: blocks.into_iter().map(|block| block.convert()).collect(),
            variables: self.vars.into_iter().map(|var| var.convert()).collect(),
            entry_block: self.entry_block,
        }
    }
    pub fn return_size(&self) -> Option<&FieldSize> {
        self.return_value.size()
    }
    pub fn return_size_mut(&mut self) -> Option<&mut FieldSize> {
        self.return_value.size_mut()
    }
    pub fn return_type(&self) -> &ExportLen {
        &self.return_value
    }
    pub fn return_type_mut(&mut self) -> &mut ExportLen {
        &mut self.return_value
    }
    pub fn block(&self, id: BlockId) -> Ref<Block> {
        Ref::map(self.blocks.borrow(), |x| &x[id.0])
    }
    pub fn block_mut(&self, id: BlockId) -> RefMut<Block> {
        RefMut::map(self.blocks.borrow_mut(), |x| &mut x[id.0])
    }
    pub fn block_by_name(&self, name: &str) -> Option<BlockId> {
        self.blocks
            .borrow()
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
        self.blocks.borrow_mut().push(block);
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
            .position(|vars| &vars.name == name)
            .map(VariableId)
    }
    pub fn create_variable(
        &mut self,
        name: String,
        src: Span,
        explicit: bool,
    ) -> Result<VariableId, ExecutionError> {
        // don't allow duplicated name
        if self.variable_by_name(&name).is_some() {
            return Err(ExecutionError::InvalidRef(src));
        }
        //TODO src
        let var = Variable::new(name, explicit.then_some(src));
        self.vars.push(var);
        Ok(VariableId(self.vars.len() - 1))
    }
}
