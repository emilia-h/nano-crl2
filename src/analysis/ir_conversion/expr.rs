
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::{ModuleIrMapping, SemanticError};
use crate::core::syntax::Identifier;
use crate::model::expr::Expr;
use crate::ir::decl::DeclId;
use crate::ir::expr::ExprId;

use std::collections::hash_map::HashMap;
use std::rc::Rc;

pub fn convert_ir_expr(
    context: &AnalysisContext,
    id_map: &mut HashMap<Identifier, DeclId>,
    expr: &Rc<Expr>,
    result_id: ExprId,
    ir_mapping: &mut ModuleIrMapping,
) -> Result<ExprId, SemanticError> {
    todo!()
}
