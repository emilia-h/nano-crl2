
use crate::ir::module::ModuleId;
use crate::util::hashing::HashByAddress;

use std::rc::Rc;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IrSort {
    pub value: HashByAddress<Rc<IrSortEnum>>,
}

#[derive(Clone, Debug)]
pub enum IrSortEnum {
    Unit,
    Bool,
    Pos,
    Nat,
    Int,
    Real,
    List {
        subsort: IrSort,
    },
    Set {
        subsort: IrSort,
    },
    FSet {
        subsort: IrSort,
    },
    Bag {
        subsort: IrSort,
    },
    FBag {
        subsort: IrSort,
    },
    Struct,
    Carthesian {
        lhs: IrSort,
        rhs: IrSort,
    },
    Function {
        lhs: IrSort,
        rhs: IrSort,
    },
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SortId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}
