
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::decl::DefId;
use crate::ir::expr::ExprId;
use crate::ir::module::ModuleId;
use crate::ir::sort::SortId;

use std::fmt::{Debug, Formatter};

/// A process in the intermediate representation.
#[derive(Debug)]
pub struct IrProc {
    pub value: IrProcEnum,
    pub loc: SourceRange,
}

#[derive(Debug)]
pub enum IrProcEnum {
    /// This can be either a process name (defined by `proc A(params) = ...`)
    /// or a multi-action (`a(args) | b(args)`, defined by `act a: Sorts`).
    MultiAction {
        actions: Vec<IrAction>,
    },
    Delta,
    // TODO: block, allow, hide, rename, comm
    Binary {
        op: BinaryProcOp,
        lhs: ProcId,
        rhs: ProcId,
    },
    Sum {
        def_id: DefId,
        identifier: Identifier,
        identifier_loc: SourceRange,
        sort: SortId,
        proc: ProcId,
    },
    IfThenElse {
        condition: ExprId,
        then_proc: ProcId,
        else_proc: ProcId,
    },
    // Time {},
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryProcOp {
    Sum,
    Parallel,
    RightParallel,
    Concat,
    LeftShift,
}

#[derive(Debug)]
pub struct IrAction {
    pub identifier: Identifier,
    pub identifier_loc: SourceRange,
    pub args: Vec<ExprId>,
    pub loc: SourceRange,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ProcId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl ProcId {
    pub fn get_module_id(&self) -> ModuleId {
        self.module
    }
}

impl Debug for ProcId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.proc.{}", self.module, self.value)
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ActionId {
    pub(crate) proc: ProcId,
    pub(crate) index: usize,
}

impl ActionId {
    pub fn get_module_id(&self) -> ModuleId {
        self.proc.module
    }
}

impl Debug for ActionId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.action.{}", self.proc, self.index)
    }
}
