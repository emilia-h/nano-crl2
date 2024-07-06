
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::{ModuleIrMapping, SemanticError};
use crate::core::syntax::Identifier;
use crate::model::expr::{Expr, ExprEnum};
use crate::ir::decl::DeclId;
use crate::ir::expr::{ExprId, IrExpr, IrExprEnum};

use std::collections::hash_map::HashMap;
use std::sync::Arc;

/// Constructs the intermediate representation of a data expression.
/// 
/// Note that this also maps the given ID to the constructed IR of `expr`
/// (stored in `ir_mapping`) and that it generates IDs for its child nodes and
/// converts those recursively.
/// 
/// Returns `result_id` for convenience.
pub fn convert_ir_expr(
    context: &AnalysisContext,
    id_map: &mut HashMap<Identifier, DeclId>,
    expr: &Arc<Expr>,
    result_id: ExprId,
    ir_mapping: &mut ModuleIrMapping,
) -> Result<ExprId, SemanticError> {
    match &expr.value {
        ExprEnum::Id { id } => {
            let Some(&decl_id) = id_map.get(id) else {
                return Err(SemanticError::IdentifierError {
                    message: format!("Unknown identifier {}", id),
                    id: id.clone(),
                })
            };
            ir_mapping.exprs.insert(result_id, IrExpr {
                value: IrExprEnum::Name { id: decl_id },
                loc: expr.loc,
            });
        },
        ExprEnum::Number { value } => {
            ir_mapping.exprs.insert(result_id, IrExpr {
                value: IrExprEnum::Number { value: *value },
                loc: expr.loc,
            });
        },
        ExprEnum::Bool { value } => {
            todo!()
        },
        ExprEnum::List { values } => {
            todo!()
        },
        ExprEnum::Set { values } => {
            todo!()
        },
        ExprEnum::Bag { values } => {
            todo!()
        },
        ExprEnum::SetComprehension { id, sort, expr } => {
            todo!()
        },
        ExprEnum::FunctionUpdate { function, lhs, rhs } => {
            todo!()
        },
        ExprEnum::Apply { callee, args } => {
            let callee_id = convert_ir_expr(
                context, id_map,
                callee, context.generate_expr_id(ir_mapping.module),
                ir_mapping,
            )?;
            let mut arg_ids = Vec::new();
            for arg in args {
                arg_ids.push(convert_ir_expr(
                    context, id_map,
                    arg, context.generate_expr_id(ir_mapping.module),
                    ir_mapping,
                )?);
            }
            ir_mapping.exprs.insert(result_id, IrExpr {
                value: IrExprEnum::Apply { callee: callee_id, args: arg_ids },
                loc: expr.loc,
            });
        },
        ExprEnum::LogicalNot { value } => {
            todo!()
        },
        ExprEnum::Negate { value } => {
            todo!()
        },
        ExprEnum::Count { value } => {
            todo!()
        },
        ExprEnum::Forall { variables, expr } => {
            todo!()
        },
        ExprEnum::Exists { variables, expr } => {
            todo!()
        },
        ExprEnum::Lambda { variables, expr } => {
            todo!()
        },
        ExprEnum::Implies { lhs, rhs } => {
            todo!()
        },
        ExprEnum::LogicalOr { lhs, rhs } => {
            todo!()
        },
        ExprEnum::LogicalAnd { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Equals { lhs, rhs } => {
            todo!()
        },
        ExprEnum::NotEquals { lhs, rhs } => {
            todo!()
        },
        ExprEnum::LessThan { lhs, rhs } => {
            todo!()
        },
        ExprEnum::LessThanEquals { lhs, rhs } => {
            todo!()
        },
        ExprEnum::GreaterThan { lhs, rhs } => {
            todo!()
        },
        ExprEnum::GreaterThanEquals { lhs, rhs } => {
            todo!()
        },
        ExprEnum::In { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Cons { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Snoc { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Concat { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Add { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Subtract { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Divide { lhs, rhs } => {
            todo!()
        },
        ExprEnum::IntegerDivide { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Mod { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Multiply { lhs, rhs } => {
            todo!()
        },
        ExprEnum::Index { lhs, rhs } => {
            todo!()
        },
        ExprEnum::If { condition, then_expr, else_expr } => {
            todo!()
        },
        ExprEnum::Where { expr, assignments } => {
            todo!()
        },
    }

    Ok(result_id)
}
