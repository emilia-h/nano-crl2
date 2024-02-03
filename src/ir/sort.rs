
use crate::util::interning::Interned;

use std::rc::Rc;

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct IrSort {
    pub value: Interned<Rc<IrSortEnum>>,
}

#[derive(Clone)]
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
