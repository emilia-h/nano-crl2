
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::sort::convert_ir_sort;
use crate::ir::decl::{IrParam, ParamId};
use crate::model::decl::VariableDecl;
use crate::model::expr::{Expr, ExprEnum};
use crate::ir::expr::{BinaryExprOp, BinderExprOp, ExprId, IrExpr, IrExprEnum, UnaryExprOp};
use crate::ir::module::IrModule;

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
    expr: &Arc<Expr>,
    module: &mut IrModule,
) -> Result<ExprId, ()> {
    let add_expr = |module: &mut IrModule, value: IrExprEnum| {
        let expr_id = context.generate_expr_id(module.id);
        module.exprs.insert(expr_id, IrExpr {
            value,
            loc: expr.loc,
        });
        expr_id
    };

    let convert_multi_binder_expr = |
        module: &mut IrModule,
        op: BinderExprOp,
        variables: &Vec<VariableDecl>,
        sub_expr: &Arc<Expr>,
    | -> Result<ExprId, ()> {
        // similar to the conversion of `ProcEnum::Sum`
        let mut current_id = convert_ir_expr(context, sub_expr, module)?;
        for variable_decl in variables.iter().rev() {
            for (identifier, identifier_loc) in variable_decl.ids.iter().rev() {
                let sort_id = convert_ir_sort(context, &variable_decl.sort, module)?;
                let def_id = context.generate_def_id(module.id);
                let next_id = add_expr(module, IrExprEnum::Binder {
                    op,
                    def_id,
                    identifier: identifier.clone(),
                    identifier_loc: *identifier_loc,
                    sort: sort_id,
                    value: current_id,
                });
                module.add_parent(sort_id.into(), next_id.into());
                module.add_parent(current_id.into(), next_id.into());
                module.add_def_source(def_id, next_id.into());
                current_id = next_id;
            }
        }
        Ok(current_id)
    };

    let convert_unary_expr = |
        module: &mut IrModule,
        op: UnaryExprOp,
        value: &Arc<Expr>,
    | -> Result<ExprId, ()> {
        let value_id = convert_ir_expr(context, value, module)?;
        let expr_id = add_expr(module, IrExprEnum::Unary {
            op,
            value: value_id,
        });
        module.add_parent(value_id.into(), expr_id.into());
        Ok(expr_id)
    };

    let convert_binary_expr = |
        module: &mut IrModule,
        op: BinaryExprOp,
        lhs: &Arc<Expr>,
        rhs: &Arc<Expr>
    | -> Result<ExprId, ()> {
        let lhs_id = convert_ir_expr(context, lhs, module)?;
        let rhs_id = convert_ir_expr(context, rhs, module)?;
        let expr_id = add_expr(module, IrExprEnum::Binary {
            op,
            lhs: lhs_id,
            rhs: rhs_id,
        });
        module.add_parent(lhs_id.into(), expr_id.into());
        Ok(expr_id)
    };

    Ok(match &expr.value {
        ExprEnum::Id { id } => {
            add_expr(module, IrExprEnum::Name { identifier: id.clone() })
        },
        ExprEnum::Number { value } => {
            add_expr(module, IrExprEnum::NumberLiteral { value: *value })
        },
        ExprEnum::Bool { value } => {
            add_expr(module, IrExprEnum::BoolLiteral { value: *value })
        },
        ExprEnum::List { values } => {
            let mut current_id = add_expr(module, IrExprEnum::EmptyListLiteral);
            for value in values.iter().rev() {
                let expr_id = convert_ir_expr(context, value, module)?;
                let next_id = add_expr(module, IrExprEnum::Binary {
                    op: BinaryExprOp::Cons,
                    lhs: expr_id,
                    rhs: current_id,
                });
                module.add_parent(expr_id.into(), next_id.into());
                module.add_parent(current_id.into(), next_id.into());
                current_id = next_id;
            }
            current_id
        },
        ExprEnum::Set { values } => {
            let mut value_ids = Vec::new();
            for value in values {
                value_ids.push(convert_ir_expr(context, value, module)?);
            }
            let expr_id = context.generate_expr_id(module.id);
            for &value_id in &value_ids {
                module.add_parent(value_id.into(), expr_id.into());
            }
            module.exprs.insert(expr_id, IrExpr {
                value: IrExprEnum::SetLiteral { values: value_ids },
                loc: expr.loc,
            });
            expr_id
        },
        ExprEnum::Bag { values } => {
            let mut value_pairs = Vec::new();
            for (lhs, rhs) in values {
                value_pairs.push((
                    convert_ir_expr(context, lhs, module)?,
                    convert_ir_expr(context, rhs, module)?,
                ));
            }
            let expr_id = context.generate_expr_id(module.id);
            for &(lhs, rhs) in &value_pairs {
                module.add_parent(lhs.into(), expr_id.into());
                module.add_parent(rhs.into(), expr_id.into());
            }
            module.exprs.insert(expr_id, IrExpr {
                value: IrExprEnum::BagLiteral { values: value_pairs },
                loc: expr.loc,
            });
            expr_id
        },
        ExprEnum::SetComprehension { id, id_loc, sort, condition } => {
            let sort_id = convert_ir_sort(context, sort, module)?;
            let condition_id = convert_ir_expr(context, condition, module)?;
            let def_id = context.generate_def_id(module.id);
            let expr_id = add_expr(module, IrExprEnum::Binder {
                op: BinderExprOp::SetComprehension,
                def_id,
                identifier: id.clone(),
                identifier_loc: *id_loc,
                sort: sort_id,
                value: condition_id,
            });
            module.add_parent(sort_id.into(), expr_id.into());
            module.add_parent(condition_id.into(), expr_id.into());
            module.add_def_source(def_id, expr_id.into());
            expr_id
        },
        ExprEnum::FunctionUpdate { function, lhs, rhs } => {
            let function_id = convert_ir_expr(context, function, module)?;
            let lhs_id = convert_ir_expr(context, lhs, module)?;
            let rhs_id = convert_ir_expr(context, rhs, module)?;
            let expr_id = add_expr(module, IrExprEnum::FunctionUpdate {
                function: function_id,
                lhs: lhs_id,
                rhs: rhs_id,
            });
            module.add_parent(function_id.into(), expr_id.into());
            module.add_parent(lhs_id.into(), expr_id.into());
            module.add_parent(rhs_id.into(), expr_id.into());
            expr_id
        },
        ExprEnum::Apply { callee, args } => {
            let callee_id = convert_ir_expr(context, callee, module)?;
            let mut arg_ids = Vec::new();
            for arg in args {
                arg_ids.push(convert_ir_expr(context, arg, module)?);
            }
            let expr_id = context.generate_expr_id(module.id);
            for &arg_id in &arg_ids {
                module.add_parent(arg_id.into(), expr_id.into());
            }
            module.exprs.insert(expr_id, IrExpr {
                value: IrExprEnum::Apply {
                    callee: callee_id,
                    args: arg_ids,
                },
                loc: expr.loc,
            });
            module.add_parent(callee_id.into(), expr_id.into());
            expr_id
        },
        ExprEnum::LogicalNot { value } => {
            convert_unary_expr(module, UnaryExprOp::LogicalNot, value)?
        },
        ExprEnum::Negate { value } => {
            convert_unary_expr(module, UnaryExprOp::Negate, value)?
        },
        ExprEnum::Count { value } => {
            convert_unary_expr(module, UnaryExprOp::Count, value)?
        },
        ExprEnum::Forall { variables, condition } => {
            convert_multi_binder_expr(module, BinderExprOp::Forall, variables, condition)?
        },
        ExprEnum::Exists { variables, condition } => {
            convert_multi_binder_expr(module, BinderExprOp::Exists, variables, condition)?
        },
        ExprEnum::Lambda { variables, expr } => {
            // NOTE: for lambdas, `lambda x, y: Nat . something` (uncurried) is
            // NOT `lambda x: Nat . lambda y: Nat . something` (curried)
            let mut ir_params = Vec::new();
            for variable_decl in variables {
                for (identifier, identifier_loc) in &variable_decl.ids {
                    let sort_id = convert_ir_sort(context, &variable_decl.sort, module)?;
                    let def_id = context.generate_def_id(module.id);
                    ir_params.push(IrParam {
                        def_id,
                        identifier: identifier.clone(),
                        identifier_loc: *identifier_loc,
                        sort: sort_id,
                        loc: variable_decl.loc,
                    });
                }
            }

            let value_id = convert_ir_expr(context, expr, module)?;

            let expr_id = context.generate_expr_id(module.id);
            for (index, param) in ir_params.iter().enumerate() {
                let param_id = ParamId {
                    parent: expr_id.into(),
                    index,
                };
                module.add_parent(param.sort.into(), param_id.into());
                module.add_parent(param_id.into(), expr_id.into());
                module.add_def_source(param.def_id, param_id.into());
            }
            module.exprs.insert(expr_id, IrExpr {
                value: IrExprEnum::Lambda {
                    params: ir_params,
                    value: value_id
                },
                loc: expr.loc,
            });
            module.add_parent(value_id.into(), expr_id.into());

            expr_id
        },
        ExprEnum::Implies { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Implies, lhs, rhs)?
        },
        ExprEnum::LogicalOr { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::LogicalOr, lhs, rhs)?
        },
        ExprEnum::LogicalAnd { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::LogicalAnd, lhs, rhs)?
        },
        ExprEnum::Equals { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Equals, lhs, rhs)?
        },
        ExprEnum::NotEquals { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::NotEquals, lhs, rhs)?
        },
        ExprEnum::LessThan { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::LessThan, lhs, rhs)?
        },
        ExprEnum::LessThanEquals { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::LessThanEquals, lhs, rhs)?
        },
        ExprEnum::GreaterThan { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::GreaterThan, lhs, rhs)?
        },
        ExprEnum::GreaterThanEquals { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::GreaterThanEquals, lhs, rhs)?
        },
        ExprEnum::In { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::In, lhs, rhs)?
        },
        ExprEnum::Cons { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Cons, lhs, rhs)?
        },
        ExprEnum::Snoc { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Snoc, lhs, rhs)?
        },
        ExprEnum::Concat { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Concat, lhs, rhs)?
        },
        ExprEnum::Add { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Add, lhs, rhs)?
        },
        ExprEnum::Subtract { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Subtract, lhs, rhs)?
        },
        ExprEnum::Divide { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Divide, lhs, rhs)?
        },
        ExprEnum::IntegerDivide { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::IntegerDivide, lhs, rhs)?
        },
        ExprEnum::Mod { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Mod, lhs, rhs)?
        },
        ExprEnum::Multiply { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Multiply, lhs, rhs)?
        },
        ExprEnum::Index { lhs, rhs } => {
            convert_binary_expr(module, BinaryExprOp::Index, lhs, rhs)?
        },
        ExprEnum::If { condition, then_expr, else_expr } => {
            let condition_id = convert_ir_expr(context, condition, module)?;
            let then_id = convert_ir_expr(context, then_expr, module)?;
            let else_id = convert_ir_expr(context, else_expr, module)?;
            let expr_id = add_expr(module, IrExprEnum::If {
                condition: condition_id,
                then_expr: then_id,
                else_expr: else_id,
            });
            module.add_parent(condition_id.into(), expr_id.into());
            module.add_parent(then_id.into(), expr_id.into());
            module.add_parent(else_id.into(), expr_id.into());
            expr_id
        },
        ExprEnum::Where { expr, assignments } => {
            let mut current_id = convert_ir_expr(context, expr, module)?;
            // rewrite to a bunch of nested `whr` expressions, where each has
            // only one variable
            for (identifier, identifier_loc, assigned) in assignments {
                let expr_id = convert_ir_expr(context, assigned, module)?;
                let def_id = context.generate_def_id(module.id);
                let next_id = add_expr(module, IrExprEnum::Where {
                    def_id,
                    identifier: identifier.clone(),
                    identifier_loc: *identifier_loc,
                    inner: current_id,
                    assigned: expr_id,
                });
                module.add_parent(expr_id.into(), next_id.into());
                module.add_parent(current_id.into(), next_id.into());
                module.add_def_source(def_id, next_id.into());
                current_id = next_id;
            }
            current_id
        },
    })
}
