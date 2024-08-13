
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::query_ir_module;
use crate::analysis::semantic::name_resolution::query_def_of_name;
use crate::ir::decl::{DefId, IrDeclEnum};
use crate::ir::expr::{BinaryExprOp, BinderExprOp, ExprId, IrExprEnum, UnaryExprOp};
use crate::ir::module::NodeId;
use crate::ir::sort::{PrimitiveSort, ResolvedSort, SortId};
use crate::util::caching::Interned;

/// Returns the (resolved) sort of an expression.
/// 
/// Note that this query does not necessarily typecheck the expression.
/// Sometimes it is possible to find the sort of an expression without it being
/// typed correctly, and then this query will not return an error.
pub fn query_sort_of_expr(
    context: &AnalysisContext,
    expr: ExprId,
) -> Result<Interned<ResolvedSort>, ()> {
    if let Some(result) = context.sorts_of_expr.get_or_lock(&expr)? {
        return result;
    }

    let ir_module = query_ir_module(&context, expr.module)?;
    let ir_expr = ir_module.get_expr(expr);
    let sort_context = context.get_resolved_sort_context();

    let result = match &ir_expr.value {
        IrExprEnum::Name { identifier } => {
            let def_id = query_def_of_name(context, expr.into())?;
            query_sort_of_def(context, def_id)
        },

        IrExprEnum::NumberLiteral { value } => {
            todo!()
        },
        IrExprEnum::BoolLiteral { value } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Bool))
        },
        IrExprEnum::ListLiteral { values } => {
            todo!()
        },
        IrExprEnum::SetLiteral { values } => {
            todo!()
        },
        IrExprEnum::BagLiteral { values } => {
            todo!()
        },

        IrExprEnum::FunctionUpdate { function, lhs, rhs } => {
            query_sort_of_expr(context, *function)
        },
        IrExprEnum::Apply { callee, args } => {
            todo!()
        },

        IrExprEnum::Binder { op: BinderExprOp::Forall, .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Bool))
        },
        IrExprEnum::Binder { op: BinderExprOp::Exists, .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Bool))
        },
        IrExprEnum::Binder { op: BinderExprOp::Lambda, sort, expr, .. } => {
            let variable_sort = query_resolved_sort(context, *sort)?;
            let expr_sort = query_sort_of_expr(context, *expr)?;
            Ok(sort_context.get_function_sort(&variable_sort, &expr_sort))
        },
        IrExprEnum::Binder { op: BinderExprOp::SetComprehension, sort, .. } => {
            query_resolved_sort(context, *sort)
        },

        IrExprEnum::Unary { op: UnaryExprOp::LogicalNot, .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Bool))
        },
        IrExprEnum::Unary { op: UnaryExprOp::Negate, value } => {
            let value_sort = query_sort_of_expr(context, *value)?;
            match *value_sort {
                ResolvedSort::Primitive { sort: PrimitiveSort::Pos } |
                ResolvedSort::Primitive { sort: PrimitiveSort::Nat } |
                ResolvedSort::Primitive { sort: PrimitiveSort::Int } => {
                    Ok(sort_context.get_primitive_sort(PrimitiveSort::Int))
                },
                ResolvedSort::Primitive { sort: PrimitiveSort::Real } => {
                    Ok(sort_context.get_primitive_sort(PrimitiveSort::Real))
                },
                _ => {
                    // cannot determine sort of negation, since the
                    // expression being negated is not a number
                    context.error();
                    return Err(());
                },
            }
        },
        IrExprEnum::Unary { op: UnaryExprOp::Count, .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Nat))
        },

        IrExprEnum::Binary {
            op: BinaryExprOp::Implies |
                BinaryExprOp::LogicalOr |
                BinaryExprOp::LogicalAnd |
                BinaryExprOp::Equals |
                BinaryExprOp::NotEquals |
                BinaryExprOp::LessThan |
                BinaryExprOp::LessThanEquals |
                BinaryExprOp::GreaterThan |
                BinaryExprOp::GreaterThanEquals |
                BinaryExprOp::In,
            ..
        } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Bool))
        },
        IrExprEnum::Binary { op: BinaryExprOp::Cons, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::Snoc, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::Concat, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::Add, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::Subtract, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::Divide, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::IntegerDivide, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::Mod, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::Multiply, lhs, rhs } => {
            todo!()
        },
        IrExprEnum::Binary { op: BinaryExprOp::Index, lhs, rhs } => {
            todo!()
        },

        IrExprEnum::If { then_expr, else_expr, .. } => {
            let then_sort = query_sort_of_expr(context, *then_expr)?;
            let else_sort = query_sort_of_expr(context, *else_expr)?;
            find_common_sort(context, &then_sort, &else_sort)
        },
        IrExprEnum::Where {  } => {
            todo!()
        },
    };

    context.sorts_of_expr.unlock(&expr, result.clone());
    result
}

/// Returns the sort of the variable/map/constructor that is denoted by `def`.
/// 
/// For instance, if there is a declaration `map x: Nat -> Nat` with ID
/// `0.def.0`, then `query_sort_of_def(0.def.0)` will return `Nat -> Nat`.
/// 
/// Another example: if there is a process `sum x: S . a(x)` with ID `0.proc.0`
/// and where the `def_id` is `0.def.1`, then `query_sort_of_def(0.def.1)` will
/// return the resolved sort of `S`.
/// 
/// However, this function does not make sense for definitions that do not
/// define expressions; for instance, if there is a sort declaration `sort X =
/// struct a | b` with def ID `0.def.2`, then `query_sort_of_def(0.def.2)` will
/// just give an error.
pub fn query_sort_of_def(
    context: &AnalysisContext,
    def: DefId,
) -> Result<Interned<ResolvedSort>, ()> {
    // TODO cache

    let ir_module = query_ir_module(context, def.module)?;
    let def_source = ir_module.get_def_source(def);
    match def_source {
        NodeId::Decl(id) => {
            let decl = ir_module.get_decl(id);
            match &decl.value {
                IrDeclEnum::Constructor { sort } |
                IrDeclEnum::GlobalVariable { sort } |
                IrDeclEnum::Map { sort } => {
                    query_resolved_sort(context, *sort)
                },
                _ => {
                    // declaration ... does not have a sort
                    context.error();
                    return Err(());
                },
            }
        },
        NodeId::Expr(_) => todo!(),
        NodeId::Module(_) => todo!(),
        NodeId::Param(_) => todo!(),
        NodeId::Proc(_) => todo!(),
        NodeId::RewriteRule(_) => todo!(),
        NodeId::Sort(_) => todo!(),
    }
}

pub fn query_resolved_sort(
    context: &AnalysisContext,
    sort: SortId,
) -> Result<Interned<ResolvedSort>, ()> {
    todo!()
}

/// Finds the least common sort of two given sorts (the smallest sort that is a
/// supersort of both), or returns an error if this sort does not exist.
/// 
/// This operation is commutative, associative, and idempotent, meaning that:
/// - common(S1, S2) = common(S2, S1)
/// - common(S1, common(S2, S3)) = common(common(S1, S2), S3)
/// - common(S, S) = S
fn find_common_sort(
    context: &AnalysisContext,
    sort1: &Interned<ResolvedSort>,
    sort2: &Interned<ResolvedSort>,
) -> Result<Interned<ResolvedSort>, ()> {
    todo!()
}
