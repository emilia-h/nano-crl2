
use crate::core::syntax::Identifier;
use crate::ir::module::ModuleId;

use std::fmt::{Debug, Formatter};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IrSort {
    pub value: IrSortEnum,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IrSortEnum {
    Unit,
    Bool,
    Pos,
    Nat,
    Int,
    Real,
    List {
        subsort: SortId,
    },
    Set {
        subsort: SortId,
    },
    FSet {
        subsort: SortId,
    },
    Bag {
        subsort: SortId,
    },
    FBag {
        subsort: SortId,
    },
    Name {
        id: Identifier,
    },
    Struct {

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
