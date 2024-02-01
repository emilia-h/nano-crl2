
use crate::core::syntax::Identifier;
use crate::ir::sort::IrSort;

pub struct IrExpr {

}

pub struct IrRewriteRule {
    pub variables: Vec<(Identifier, IrSort)>,
    pub condition: IrExpr,
    pub lhs: IrExpr,
    pub rhs: IrExpr,
}
