
#[derive(Clone, Eq, PartialEq)]
pub struct IrSort {
    pub index: usize,
}

pub struct IrSortDefinition {
    pub value: IrSortDefinitionEnum,
}

pub enum IrSortDefinitionEnum {
    Unit,
    Bool,
    Pos,
    Nat,
    Int,
    Real,
    List {
        subsort: usize,
    },
    Set {
        subsort: usize,
    },
    FSet {
        subsort: usize,
    },
    Bag {
        subsort: usize,
    },
    FBag {
        subsort: usize,
    },
    Struct,
    Carthesian {
        lhs: usize,
        rhs: usize,
    },
    Function {
        lhs: usize,
        rhs: usize,
    },
}
