
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
        actions: Vec<(Identifier, Vec<ExprId>)>,
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

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ProcId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl Debug for ProcId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.proc.{}", self.module, self.value)
    }
}
