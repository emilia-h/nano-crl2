
use crate::core::syntax::{Identifier, ModuleId, SourceRange};
use crate::ir::decl::DefId;
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
    Function {
        lhs: Vec<SortId>,
        rhs: SortId,
    },
    Name {
        identifier: Identifier,
    },
    Def {
        def_id: DefId,
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
    Function {
        lhs: Vec<Interned<ResolvedSort>>,
        rhs: Interned<ResolvedSort>,
    },
    Def {
        id: DefId,
    },
}

impl ResolvedSort {
    pub fn is_unit(&self) -> bool {
        matches!(self, ResolvedSort::Primitive { sort: PrimitiveSort::Unit })
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, ResolvedSort::Primitive { sort: PrimitiveSort::Bool })
    }

    pub fn is_pos(&self) -> bool {
        matches!(self, ResolvedSort::Primitive { sort: PrimitiveSort::Pos })
    }

    pub fn is_nat(&self) -> bool {
        matches!(self, ResolvedSort::Primitive { sort: PrimitiveSort::Nat })
    }

    pub fn is_int(&self) -> bool {
        matches!(self, ResolvedSort::Primitive { sort: PrimitiveSort::Int })
    }

    pub fn is_real(&self) -> bool {
        matches!(self, ResolvedSort::Primitive { sort: PrimitiveSort::Real })
    }

    /// Returns whether this is a `Generic` variant with op == `List`.
    pub fn is_list(&self) -> bool {
        matches!(self, ResolvedSort::Generic { op: GenericSortOp::List, .. })
    }

    /// Returns whether this is a `Generic` variant with op == `Set`.
    pub fn is_set(&self) -> bool {
        matches!(self, ResolvedSort::Generic { op: GenericSortOp::Set, .. })
    }

    /// Returns whether this is a `Generic` variant with op == `FSet`.
    pub fn is_fset(&self) -> bool {
        matches!(self, ResolvedSort::Generic { op: GenericSortOp::FSet, .. })
    }

    /// Returns whether this is a `Generic` variant with op == `Bag`.
    pub fn is_bag(&self) -> bool {
        matches!(self, ResolvedSort::Generic { op: GenericSortOp::Bag, .. })
    }

    /// Returns whether this is a `Generic` variant with op == `FBag`.
    pub fn is_fbag(&self) -> bool {
        matches!(self, ResolvedSort::Generic { op: GenericSortOp::FBag, .. })
    }

    /// Returns a number that represents the generality of a number sort, or `None`
    /// if the sort is not a number sort.
    /// 
    /// It returns the following values:
    /// - `Pos`: 0
    /// - `Nat`: 1
    /// - `Int`: 2
    /// - `Real`: 3
    pub fn get_number_sort_generality(&self) -> Option<u32> {
        match self {
            ResolvedSort::Primitive { sort } => sort.get_number_sort_generality(),
            _ => None,
        }
    }
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

    /// Returns whether this is a `Pos`, `Nat`, `Int` or `Real` variant
    pub fn is_any_number(self) -> bool {
        matches!(self, Self::Pos | Self::Nat | Self::Int | Self::Real)
    }

    /// Returns a number that represents the generality of a number sort, or `None`
    /// if the sort is not a number sort.
    /// 
    /// It returns the following values:
    /// - `Pos`: 0
    /// - `Nat`: 1
    /// - `Int`: 2
    /// - `Real`: 3
    pub fn get_number_sort_generality(&self) -> Option<u32> {
        match self {
            Self::Pos => Some(0),
            Self::Nat => Some(1),
            Self::Int => Some(2),
            Self::Real => Some(3),
            _ => None,
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

    /// Returns whether this is a `Set` or `FSet` variant.
    pub fn is_any_set(self) -> bool {
        matches!(self, Self::Set | Self::FSet)
    }

    /// Returns whether this is a `Bag` or `FBag` variant.
    pub fn is_any_bag(self) -> bool {
        matches!(self, Self::Bag | Self::FBag)
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
