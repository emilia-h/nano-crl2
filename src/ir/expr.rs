
use crate::core::syntax::{Identifier, SourceLocation};
use crate::ir::decl::DeclId;
use crate::ir::module::ModuleId;
use crate::ir::sort::IrSort;

#[derive(Debug)]
pub struct IrExpr {
    pub value: IrExprEnum,
    pub loc: SourceLocation,
}

#[derive(Debug)]
pub enum IrExprEnum {
    Name {
        id: DeclId,
    },
    Number {
        // TODO should actually allow arbitrary-size integers
        value: u64,
    },
    // TODO many more
    Apply {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    // TODO many more
    Forall {

    },
    Exists {

    },
    // TODO many more
}

#[derive(Debug)]
pub struct IrRewriteRule {
    pub variables: Vec<(Identifier, IrSort)>,
    pub condition: ExprId,
    pub lhs: ExprId,
    pub rhs: ExprId,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ExprId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}
