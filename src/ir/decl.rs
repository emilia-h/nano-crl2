
use crate::core::syntax::Identifier;
use crate::ir::module::ModuleId;
use crate::ir::proc::ProcId;
use crate::ir::sort::SortId;

use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub struct IrDecl {
    pub def_id: DefId,
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
        params: Vec<(DefId, Identifier, SortId)>,
        proc: ProcId,
    },
    /// A sort declaration of the form `sort A;`.
    Sort,
    /// A sort declaration of the form `sort A = Nat;` or `sort A = struct a(A)
    /// | b;`.
    SortAlias {
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
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct DeclId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl Debug for DeclId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}.decl.{}", self.module.index, self.value)
    }
}

/// A (nameless) identifier that identifies a fresh declaration, for instance a
/// variable.
/// 
/// Note that this is different from a `DeclId`; a `DeclId` refers to the
/// IR declaration object such as `proc a = ...;`, while `DefId` identifies
/// the named object `a` that is created by a declaration (`proc a = ...;`), an
/// expression (`exists a : Nat . ...`), a process (`sum a : Nat . ...`) or a
/// sort (`struct a | ...`).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct DefId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl Debug for DefId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.def.{}", self.module, self.value)
    }
}
