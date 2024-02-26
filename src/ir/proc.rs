
use crate::core::syntax::SourceLocation;
use crate::ir::decl::DeclId;
use crate::ir::expr::ExprId;
use crate::ir::module::ModuleId;
use crate::ir::sort::SortId;

/// A process in the intermediate representation.
#[derive(Debug)]
pub struct IrProc {
    pub value: IrProcEnum,
    pub loc: SourceLocation,
}

#[derive(Debug)]
pub enum IrProcEnum {
    MultiAction {
        actions: Vec<(DeclId, Vec<ExprId>)>,
    },
    Name {
        id: DeclId,
        args: Vec<ExprId>,
    },
    Delta,
    // TODO: block, allow, hide, rename, comm
    Add {
        lhs: ProcId,
        rhs: ProcId,
    },
    Sum {
        variable: DeclId,
        sort: SortId,
        proc: ProcId,
    },
    Parallel {
        lhs: ProcId,
        rhs: ProcId,
    },
    RightParallel {
        lhs: ProcId,
        rhs: ProcId,
    },
    IfThenElse {
        condition: ExprId,
        then_proc: ProcId,
        else_proc: ProcId,
    },
    // LeftShift {},
    Concat {
        lhs: ProcId,
        rhs: ProcId,
    },
    // Time {},
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProcId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}
