
use crate::core::syntax::Identifier;
use crate::ir::sort::IrSort;

pub struct IrDecl {
    pub id: Identifier,
    pub value: IrDeclEnum,
}

pub enum IrDeclEnum {
    Action {
        sort: IrSort,
    },
    Constructor {
        sort: IrSort,
    },
    GlobalVariable {
        sort: IrSort,
    },
    Map {
        sort: IrSort,
    },
    Process {

    },
    Sort {
        sort: IrSort,
    },
}
