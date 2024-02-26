
use crate::ir::module::ModuleId;
use crate::ir::proc::ProcId;
use crate::ir::sort::SortId;

#[derive(Debug)]
pub struct IrDecl {
    pub value: IrDeclEnum,
}

#[derive(Debug)]
pub enum IrDeclEnum {
    Action {
        sort: SortId,
    },
    Constructor {
        sort: SortId,
    },
    GlobalVariable {
        sort: SortId,
    },
    LocalVariable {
        sort: SortId,
    },
    Map {
        sort: SortId,
    },
    Process {
        params: Vec<DeclId>,
        proc: ProcId,
    },
    Sort {
        sort: SortId,
    },
}

/// A (nameless) identifier that, within a given analysis context, refers to a
/// specific declaration in a module.
/// 
/// This means that it has an id for the module, and an id for the declaration.
/// 
/// Note that if you remove declarations from the module, this will invalidate
/// existing IDs.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct DeclId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}
