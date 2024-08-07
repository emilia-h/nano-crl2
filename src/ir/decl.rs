
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::module::ModuleId;
use crate::ir::proc::ProcId;
use crate::ir::sort::SortId;

use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub struct IrDecl {
    pub def_id: DefId,
    pub identifier: Identifier,
    pub value: IrDeclEnum,
    pub loc: SourceRange,
}

#[derive(Debug)]
pub enum IrDeclEnum {
    Action {
        sorts: Vec<SortId>,
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
        params: Vec<IrParam>,
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

/// A parameter node in the IR.
/// 
/// This is used in `proc` declarations. The reason a parameter is a node of
/// its own in the IR graph with its own ID type is that it is useful to be
/// able to refer to it using a [`NodeId`]. In particular, it is used when one
/// wants to find the source node of a `DefId`, which could be a parameter.
/// 
/// [`NodeId`]: ../module/struct.NodeId.html
#[derive(Debug)]
pub struct IrParam {
    pub def_id: DefId,
    pub identifier: Identifier,
    pub sort: SortId,
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

impl DeclId {
    pub fn get_module_id(&self) -> ModuleId {
        self.module
    }
}

impl Debug for DeclId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.decl.{}", self.module, self.value)
    }
}

/// A (nameless) identifier that identifies a specific parameter of a
/// declaration.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ParamId {
    pub(crate) decl: DeclId,
    pub(crate) index: usize,
}

impl Debug for ParamId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.param.{}", self.decl, self.index)
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
