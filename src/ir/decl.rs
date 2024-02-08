
use crate::core::syntax::Identifier;
use crate::ir::module::ModuleId;
use crate::ir::sort::SortId;

pub struct IrDecl {
    pub id: Identifier,
    pub value: IrDeclEnum,
}

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
    Map {
        sort: SortId,
    },
    Process {

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
