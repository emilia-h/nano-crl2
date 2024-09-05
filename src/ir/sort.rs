
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::decl::DefId;
use crate::ir::module::ModuleId;
use crate::util::caching::Interned;

use std::fmt::{Debug, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IrSort {
    pub value: IrSortEnum,
    pub loc: SourceRange,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IrSortEnum {
    Primitive {
        sort: PrimitiveSort,
    },
    Generic {
        op: GenericSortOp,
        subsort: SortId,
    },
    Carthesian {
        lhs: SortId,
        rhs: SortId,
    },
    Function {
        lhs: SortId,
        rhs: SortId,
    },
    Name {
        identifier: Identifier,
    },
    Struct {
        // TODO
    },
}

#[derive(Debug, Eq, PartialEq)]
pub enum ResolvedSort {
    Primitive {
        sort: PrimitiveSort,
    },
    Generic {
        op: GenericSortOp,
        subsort: Interned<ResolvedSort>,
    },
    Carthesian {
        lhs: Interned<ResolvedSort>,
        rhs: Interned<ResolvedSort>,
    },
    Function {
        lhs: Interned<ResolvedSort>,
        rhs: Interned<ResolvedSort>,
    },
    Def {
        id: DefId,
    },
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum PrimitiveSort {
    Unit,
    Bool,
    Pos,
    Nat,
    Int,
    Real,
}

impl PrimitiveSort {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Unit => "__Unit",
            Self::Bool => "Bool",
            Self::Pos => "Pos",
            Self::Nat => "Nat",
            Self::Int => "Int",
            Self::Real => "Real",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum GenericSortOp {
    List,
    Set,
    FSet,
    Bag,
    FBag,
}

impl GenericSortOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::List => "List",
            Self::Set => "Set",
            Self::FSet => "FSet",
            Self::Bag => "Bag",
            Self::FBag => "FBag",
        }
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct SortId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl SortId {
    pub fn get_module_id(&self) -> ModuleId {
        self.module
    }
}

impl Debug for SortId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.sort.{}", self.module, self.value)
    }
}
