
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::module::ModuleId;

use std::fmt::{Debug, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IrSort {
    pub value: IrSortEnum,
    pub loc: SourceRange,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IrSortEnum {
    Unit,
    Bool,
    Pos,
    Nat,
    Int,
    Real,
    Generic {
        op: GenericSortOp,
        subsort: SortId,
    },
    Name {
        identifier: Identifier,
    },
    Struct {
        // TODO
    },
    Carthesian {
        lhs: SortId,
        rhs: SortId,
    },
    Function {
        lhs: SortId,
        rhs: SortId,
    },
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum GenericSortOp {
    List,
    Set,
    FSet,
    Bag,
    FBag,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct SortId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl Debug for SortId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.sort.{}", self.module, self.value)
    }
}
