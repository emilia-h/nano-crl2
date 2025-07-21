
use crate::analysis::context::{AnalysisContext, ResolvedSortContext};
use crate::analysis::ir_conversion::module::query_ir_module;
use crate::analysis::semantic::name_resolution::query_def_of_name;
use crate::ir::decl::{DefId, IrDeclEnum};
use crate::ir::display::ResolvedSortDisplay;
use crate::ir::expr::{BinaryExprOp, BinderExprOp, ExprId, IrExpr, IrExprEnum, UnaryExprOp};
use crate::ir::iterator::get_node_loc;
use crate::ir::module::{IrModule, NodeId};
use crate::ir::proc::IrProcEnum;
use crate::ir::sort::{GenericSortOp, IrSortEnum, PrimitiveSort, ResolvedSort, SortId};
use crate::util::caching::Interned;
use crate::util::error::{chain_option, chain_result};

/// Returns the (resolved) sort of an expression.
/// 
/// Note that this query does not necessarily typecheck the expression.
/// Sometimes it is possible to find the sort of an expression without it being
/// typed correctly, and then this query will not return an error.
/// 
/// This query is cached.
pub fn query_sort_of_expr(
    context: &AnalysisContext,
    expr: ExprId,
) -> Result<Interned<ResolvedSort>, ()> {
    match context.sorts_of_expr.get_or_lock(&expr) {
        Ok(Some(value)) => value,
        Ok(None) => {
            let result = calculate_sort_of_expr(context, expr);
            context.sorts_of_expr.unlock(&expr, result.clone());
            result
        },
        Err(()) => {
            let loc = query_ir_module(context, expr.get_module_id())?
                .get_expr(expr)
                .loc;
            context.error_cyclic_dependency(loc, expr.into())
        },
    }
}

fn calculate_sort_of_expr(
    context: &AnalysisContext,
    expr: ExprId,
) -> Result<Interned<ResolvedSort>, ()> {
    let module = query_ir_module(&context, expr.get_module_id())?;
    let ir_expr = module.get_expr(expr);
    let sort_context = context.get_resolved_sort_context();

    match &ir_expr.value {
        IrExprEnum::Name { .. } => {
            let def_id = query_def_of_name(context, expr.into())?;
            query_sort_of_def(context, def_id)
        },

        IrExprEnum::NumberLiteral { value } => {
            todo!()
        },
        IrExprEnum::BoolLiteral { .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Bool))
        },
        IrExprEnum::EmptyListLiteral => {
            todo!()
        },
        IrExprEnum::SetLiteral { values } => {
            todo!()
        },
        IrExprEnum::BagLiteral { values } => {
            todo!()
        },

        IrExprEnum::FunctionUpdate { function, .. } => {
            query_sort_of_expr(context, *function)
        },
        IrExprEnum::Apply { callee, .. } => {
            let callee_sort = query_sort_of_expr(context, *callee)?;
            match &*callee_sort {
                ResolvedSort::Function { rhs, .. } => {
                    Ok(Interned::clone(rhs))
                },
                _ => {
                    let error = format!(
                        "cannot call a value with a sort `{}` that is not a function",
                        ResolvedSortDisplay::new(&callee_sort, &module),
                    );
                    return context.error(expr.get_module_id(), ir_expr.loc, error);
                },
            }
        },

        IrExprEnum::Binder { op: BinderExprOp::Forall, .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Bool))
        },
        IrExprEnum::Binder { op: BinderExprOp::Exists, .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Bool))
        },
        IrExprEnum::Binder { op: BinderExprOp::SetComprehension, sort, .. } => {
            let result = sort_context.get_generic_sort(
                GenericSortOp::Set,
                &query_resolved_sort(context, *sort)?,
            );
            Ok(result)
        },

        IrExprEnum::Lambda { params, value, .. } => {
            let mut param_sorts = Vec::new();
            for param in params {
                if let Ok(sort) = query_resolved_sort(context, param.sort) {
                    param_sorts.push(sort);
                }
            }
            if param_sorts.len() != params.len() {
                return Err(());
            }
            let expr_sort = query_sort_of_expr(context, *value)?;
            Ok(sort_context.get_function_sort(&param_sorts, &expr_sort))
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
                    let error = format!(
                        "cannot negate a value when its sort `{}` is not a number sort",
                        ResolvedSortDisplay::new(&value_sort, &module),
                    );
                    return context.error(module.id, ir_expr.loc, error);
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
        IrExprEnum::Binary { op: BinaryExprOp::Cons, rhs, .. } => {
            query_sort_of_expr(context, *rhs)
        },
        IrExprEnum::Binary { op: BinaryExprOp::Snoc, lhs, .. } => {
            query_sort_of_expr(context, *lhs)
        },
        IrExprEnum::Binary { op: BinaryExprOp::Concat, lhs, rhs } => {
            let (lhs_sort, rhs_sort) = chain_result(
                query_sort_of_expr(context, *lhs),
                query_sort_of_expr(context, *rhs),
            )?;
            if !lhs_sort.is_list() {
                let error = format!(
                    "cannot concatenate when the left-hand side sort `{}` is not a `List` sort",
                    ResolvedSortDisplay::new(&lhs_sort, &module),
                );
                return context.error(module.id, ir_expr.loc, error);
            } else if !rhs_sort.is_list() {
                let error = format!(
                    "cannot concatenate when the right-hand side sort `{}` is not a `List` sort",
                    ResolvedSortDisplay::new(&rhs_sort, &module),
                );
                return context.error(module.id, ir_expr.loc, error);
            } else if lhs_sort != rhs_sort {
                let error = format!(
                    "cannot concatenate two lists of incompatible sorts `{}` and `{}`",
                    ResolvedSortDisplay::new(&lhs_sort, &module),
                    ResolvedSortDisplay::new(&rhs_sort, &module),
                );
                return context.error(module.id, ir_expr.loc, error);
            }
            Ok(lhs_sort)
        },
        IrExprEnum::Binary { op: BinaryExprOp::Add, lhs, rhs } => {
            let (lhs_sort, rhs_sort) = chain_result(
                query_sort_of_expr(context, *lhs),
                query_sort_of_expr(context, *rhs),
            )?;
            match (&*lhs_sort, &*rhs_sort) {
                (
                    ResolvedSort::Generic { op: op1, subsort: subsort1 },
                    ResolvedSort::Generic { op: op2, subsort: subsort2 },
                ) if op1.is_any_set() && op2.is_any_set() => {
                    let Some(common) = find_common_sort(context, subsort1, subsort2) else {
                        let error = format!(
                            "cannot add two sets of incompatible sorts `{}` and `{}`",
                            ResolvedSortDisplay::new(subsort1, &module),
                            ResolvedSortDisplay::new(subsort2, &module),
                        );
                        return context.error(module.id, ir_expr.loc, error);
                    };
                    if *op1 == GenericSortOp::FSet && *op2 == GenericSortOp::FSet {
                        Ok(sort_context.get_generic_sort(GenericSortOp::FSet, &common))
                    } else {
                        Ok(sort_context.get_generic_sort(GenericSortOp::Set, &common))
                    }
                },
                (
                    ResolvedSort::Generic { op: op1, subsort: subsort1 },
                    ResolvedSort::Generic { op: op2, subsort: subsort2 },
                ) if op1.is_any_bag() && op2.is_any_bag() => {
                    // almost the same code as in the previous code
                    let Some(common) = find_common_sort(context, subsort1, subsort2) else {
                        let error = format!(
                            "cannot add two bags of incompatible sorts `{}` and `{}`",
                            ResolvedSortDisplay::new(subsort1, &module),
                            ResolvedSortDisplay::new(subsort2, &module),
                        );
                        return context.error(module.id, ir_expr.loc, error);
                    };
                    if *op1 == GenericSortOp::FBag && *op2 == GenericSortOp::FBag {
                        Ok(sort_context.get_generic_sort(GenericSortOp::FBag, &common))
                    } else {
                        Ok(sort_context.get_generic_sort(GenericSortOp::Bag, &common))
                    }
                },
                (
                    ResolvedSort::Primitive { sort: sort1 },
                    ResolvedSort::Primitive { sort: sort2 },
                ) if sort1.is_any_number() && sort2.is_any_number() => {
                    let g1 = lhs_sort.get_number_sort_generality().unwrap();
                    let g2 = rhs_sort.get_number_sort_generality().unwrap();
                    Ok(get_number_sort_from_generality(sort_context, g1.max(g2)))
                },
                _ => {
                    let error = format!(
                        "can only add numbers, sets and bags, not `{}` and `{}`",
                        ResolvedSortDisplay::new(&lhs_sort, &module),
                        ResolvedSortDisplay::new(&rhs_sort, &module),
                    );
                    return context.error(module.id, ir_expr.loc, error);
                },
            }
        },
        IrExprEnum::Binary { op: BinaryExprOp::Subtract, .. } => {
            let (g1, g2) = get_binary_op_number_generalities(context, &module, ir_expr)?;
            if g1 == 3 || g2 == 3 {
                Ok(sort_context.get_primitive_sort(PrimitiveSort::Real))
            } else {
                Ok(sort_context.get_primitive_sort(PrimitiveSort::Int))
            }
        },
        IrExprEnum::Binary { op: BinaryExprOp::Divide, .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Real))
        },
        IrExprEnum::Binary { op: BinaryExprOp::IntegerDivide, .. } => {
            let (g1, g2) = get_binary_op_number_generalities(context, &module, ir_expr)?;
            if g2 != 0 {
                let error = format!(
                    "can only perform integer division with dividend of type `Pos`, not `{}",
                    display_number_sort_generality(g2),
                );
                return context.error(module.id, ir_expr.loc, error);
            }
            if g1 <= 1 {
                Ok(sort_context.get_primitive_sort(PrimitiveSort::Nat))
            } else if g1 == 2 {
                Ok(sort_context.get_primitive_sort(PrimitiveSort::Int))
            } else {
                let error = "cannot perform integer division with divisor of type `Real`".to_owned();
                return context.error(module.id, ir_expr.loc, error);
            }
        },
        IrExprEnum::Binary { op: BinaryExprOp::Mod, .. } => {
            Ok(sort_context.get_primitive_sort(PrimitiveSort::Nat))
        },
        IrExprEnum::Binary { op: BinaryExprOp::Multiply, .. } => {
            let (g1, g2) = get_binary_op_number_generalities(context, &module, ir_expr)?;
            Ok(get_number_sort_from_generality(sort_context, g1.max(g2)))
        },
        IrExprEnum::Binary { op: BinaryExprOp::Index, lhs, .. } => {
            let lhs_sort = query_sort_of_expr(context, *lhs)?;
            let ResolvedSort::Generic { op: GenericSortOp::List, subsort } = &*lhs_sort else {
                let error = format!(
                    "cannot index expression of which sort `{}` is not a `List` sort",
                    ResolvedSortDisplay::new(&lhs_sort, &module)
                );
                return context.error(module.id, ir_expr.loc, error);
            };
            Ok(Interned::clone(subsort))
        },

        IrExprEnum::If { then_expr, else_expr, .. } => {
            let (then_sort, else_sort) = chain_result(
                query_sort_of_expr(context, *then_expr),
                query_sort_of_expr(context, *else_expr),
            )?;
            let Some(value) = find_common_sort(context, &then_sort, &else_sort) else {
                let error = format!(
                    "incompatible expressions in `if`, `{}` and `{}` do not have a common sort",
                    ResolvedSortDisplay::new(&then_sort, &module),
                    ResolvedSortDisplay::new(&else_sort, &module),
                );
                return context.error(expr.get_module_id(), ir_expr.loc, error);
            };
            Ok(value)
        },
        IrExprEnum::Where { inner, .. } => {
            query_sort_of_expr(context, *inner)
        },
    }
}

/// For a binary expression of the form `a + b`, `a - b` etc., returns the
/// number sort generalities of its operands.
/// 
/// # Panics
/// `expr.value` must be of the variant `IrExprEnum::Binary`, or this function
/// will panic.
fn get_binary_op_number_generalities(
    context: &AnalysisContext,
    module: &IrModule,
    expr: &IrExpr,
) -> Result<(u32, u32), ()> {
    let IrExprEnum::Binary { lhs, rhs, .. } = &expr.value else {
        panic!();
    };
    let (lhs_sort, rhs_sort) = chain_result(
        query_sort_of_expr(context, *lhs),
        query_sort_of_expr(context, *rhs),
    )?;
    let g1 = lhs_sort.get_number_sort_generality();
    let g2 = rhs_sort.get_number_sort_generality();
    let Some(result) = chain_option(g1, g2) else {
        let error = format!(
            "cannot add expressions of sorts `{}` and `{}`",
            ResolvedSortDisplay::new(&lhs_sort, &module),
            ResolvedSortDisplay::new(&rhs_sort, &module),
        );
        return context.error(module.id, expr.loc, error);
    };
    Ok(result)
}

/// Returns the sort that `expr` is desired to have in the context of the IR,
/// or `None` if this is not implied by the context.
/// 
/// For instance, if we have `map x: Set(Int); eqn x = {1}` then the `1`
/// expression has desired sort `Int`, even though the sort of `1` without
/// context is `Pos`. Its actual sort is then the common type of `Int` and
/// `Pos`, which is `Int`, which will typecheck correctly.
fn find_desired_sort(
    context: &AnalysisContext,
    module: &IrModule,
    expr: ExprId,
) -> Result<Option<Interned<ResolvedSort>>, ()> {
    let Some(parent) = module.get_parent(expr.into()) else {
        return Ok(None);
    };

    match parent {
        NodeId::Action(action_id) => {
            // find index
            let action = module.get_action(action_id);
            let (i, _) = action.args.iter().enumerate()
                .find(|&(i, &arg)| arg == expr)
                .unwrap();

            let def = query_def_of_name(context, parent)?;
            // a process name can only be from a declaration, so can safely unwrap
            let decl_id = module.get_def_source(def).unwrap_decl_id();
            match &module.get_decl(decl_id).value {
                IrDeclEnum::Action { params } => {
                    query_resolved_sort(context, params[i]).map(Some)
                },
                IrDeclEnum::Process { params, proc } => {
                    query_resolved_sort(context, params[i].sort).map(Some)
                },
                _ => panic!("expected a process declaration"),
            }
        },
        NodeId::Decl(decl_id) => {
            todo!() // lots of work to be done here
        },
        NodeId::Expr(expr_id) => {
            todo!() // lots of work to be done here
        },
        NodeId::RewriteRule(rewrite_rule_id) => {
            todo!()
        },
        NodeId::Module(_) => Ok(None),
        NodeId::Param(_) |
        NodeId::Proc(_) |
        NodeId::RewriteVar(_) |
        NodeId::RewriteSet(_) |
        NodeId::Sort(_) => panic!(),
    }
}

/// Returns the sort of the variable/map/constructor that is denoted by `def`.
/// 
/// For instance, if there is a declaration `map x: Nat -> Nat` with ID
/// `0.def.0`, then `query_sort_of_def(0.def.0)` will return `Nat -> Nat`.
/// 
/// Another example: if there is a process `sum x: S . a(x)` with ID `0.proc.0`
/// and where the `def_id` of `x` is `0.def.1`, then
/// `query_sort_of_def(0.def.1)` will return the resolved sort of `S`.
/// 
/// However, this function does not make sense for definitions that do not
/// define expressions; for instance, if there is a sort declaration `sort X =
/// struct a | b` with def ID `0.def.2`, then `query_sort_of_def(0.def.2)` will
/// just give an error.
pub fn query_sort_of_def(
    context: &AnalysisContext,
    def: DefId,
) -> Result<Interned<ResolvedSort>, ()> {
    match context.sorts_of_def.get_or_lock(&def) {
        Ok(Some(value)) => value,
        Ok(None) => {
            let result = calculate_sort_of_def(context, def);
            context.sorts_of_def.unlock(&def, result.clone());
            result
        },
        Err(()) => {
            let ir_module = query_ir_module(context, def.get_module_id())?;
            let source = ir_module.get_def_source(def);
            let loc = get_node_loc(&ir_module, source);
            context.error_cyclic_dependency(loc, source)
        },
    }
}

fn calculate_sort_of_def(
    context: &AnalysisContext,
    def: DefId,
) -> Result<Interned<ResolvedSort>, ()> {
    let module = query_ir_module(context, def.get_module_id())?;
    let def_source = module.get_def_source(def);
    match def_source {
        NodeId::Decl(id) => {
            let decl = module.get_decl(id);
            match &decl.value {
                IrDeclEnum::Constructor { sort, .. } |
                IrDeclEnum::GlobalVariable { sort } |
                IrDeclEnum::Map { sort } => {
                    query_resolved_sort(context, *sort)
                },
                _ => { // NOTE: this has to be an actual error, not `panic`
                    let error = format!(
                        "declaration `{}` does not have a sort, cannot be used in an expression",
                        decl.identifier
                    );
                    return context.error(module.id, decl.loc, error);
                },
            }
        },
        NodeId::Expr(id) => {
            let expr = module.get_expr(id);
            match &expr.value {
                IrExprEnum::Binder { def_id, sort, .. } => {
                    assert_eq!(*def_id, def);
                    query_resolved_sort(context, *sort)
                },
                // NOTE: Lambda has parameters, so does not have a def_id
                IrExprEnum::Where { def_id, assigned, .. } => {
                    assert_eq!(*def_id, def);
                    query_sort_of_expr(context, *assigned)
                },
                _ => panic!("an expression not a binder or `whr` cannot define something"),
            }
        },
        NodeId::Param(id) => {
            query_resolved_sort(context, module.get_param(id).sort)
        },
        NodeId::Proc(id) => {
            let proc = module.get_proc(id);
            match &proc.value {
                IrProcEnum::Sum { def_id, sort, .. } => {
                    assert_eq!(*def_id, def);
                    query_resolved_sort(context, *sort)
                },
                _ => panic!("a process that is not `sum` cannot define something"),
            }
        },
        NodeId::RewriteVar(id) => {
            query_resolved_sort(context, module.get_rewrite_var(id).sort)
        },

        NodeId::Action(_) |
        NodeId::Module(_) |
        NodeId::RewriteSet(_) |
        NodeId::RewriteRule(_) |
        NodeId::Sort(_) => panic!("this node cannot define something"),
    }
}

pub fn query_resolved_sort(
    context: &AnalysisContext,
    sort: SortId,
) -> Result<Interned<ResolvedSort>, ()> {
    match context.resolved_sorts.get_or_lock(&sort) {
        Ok(Some(value)) => value,
        Ok(None) => {
            let result = calculate_resolved_sort(context, sort);
            context.resolved_sorts.unlock(&sort, result.clone());
            result
        },
        Err(()) => {
            let loc = query_ir_module(context, sort.get_module_id())?
                .get_sort(sort)
                .loc;
            context.error_cyclic_dependency(loc, sort.into())
        },
    }
}

fn calculate_resolved_sort(
    context: &AnalysisContext,
    sort: SortId,
) -> Result<Interned<ResolvedSort>, ()> {
    let module = query_ir_module(context, sort.get_module_id())?;
    let ir_sort = module.get_sort(sort);
    let sort_context = context.get_resolved_sort_context();
    match &ir_sort.value {
        IrSortEnum::Function { lhs, rhs } => {
            let lhs_resolved = lhs
                .iter()
                .map(|&x| {
                    query_resolved_sort(context, x)
                })
                .collect::<Result<Vec<Interned<ResolvedSort>>, ()>>();
            let (lhs_resolved, rhs_resolved) = chain_result(
                lhs_resolved,
                query_resolved_sort(context, *rhs),
            )?;
            Ok(sort_context.get_function_sort(&lhs_resolved, &rhs_resolved))
        },
        IrSortEnum::Generic { op, subsort } => {
            let s = query_resolved_sort(context, *subsort)?;
            Ok(sort_context.get_generic_sort(*op, &s))
        },
        IrSortEnum::Name { .. } => {
            let def_id = query_def_of_name(context, sort.into())?;
            Ok(sort_context.get_def_sort(def_id))
        },
        IrSortEnum::Primitive { sort } => {
            Ok(sort_context.get_primitive_sort(*sort))
        },
        IrSortEnum::Def { def_id } => {
            Ok(sort_context.get_def_sort(*def_id))
        },
    }
}

/// Finds the least common sort of two given sorts (the smallest sort that is a
/// supersort of both), or returns `None` if such a sort does not exist.
/// 
/// This operation is commutative, associative, and idempotent, meaning that:
/// - common(S1, S2) = common(S2, S1)
/// - common(S1, common(S2, S3)) = common(common(S1, S2), S3)
/// - common(S, S) = S
/// 
/// If we consider the set of sorts to be a lattice, then this is the join
/// operator.
/// 
/// Note that this function does not report errors, it just returns `None` if
/// the two sorts are incomparable.
fn find_common_sort(
    context: &AnalysisContext,
    sort1: &Interned<ResolvedSort>,
    sort2: &Interned<ResolvedSort>,
) -> Option<Interned<ResolvedSort>> {
    use ResolvedSort::*;

    // note this guard; this prevents a lot of checks in the `match` clause
    if *sort1 == *sort2 {
        return Some(Interned::clone(sort1));
    }

    let sort_context = context.get_resolved_sort_context();

    // a primitive sort can only have a common type with another primitive
    // sort; similar rules hold for generic sorts and function sorts
    match (&**sort1, &**sort2) {
        (Primitive { sort: s1 }, Primitive { sort: s2 }) => {
            assert_ne!(s1, s2); // should have been caught by top-level `if`
            let g1 = s1.get_number_sort_generality()?;
            let g2 = s2.get_number_sort_generality()?;
            Some(get_number_sort_from_generality(sort_context, g1.max(g2)))
        },
        // a generic sort can only have a common type with another generic sort
        (Generic { op: op1, subsort: s1 }, Generic { op: op2, subsort: s2 }) => {
            todo!()
        },
        (Function { lhs: lhs1, rhs: rhs1 }, Function { lhs: lh2, rhs: rhs2 }) => {
            todo!()
        },
        // note that two structural types (`ResolvedSort::Def`) are only equal
        // if they are the same name (i.e., nominal typing), and otherwise they
        // are always incomparable
        _ => None,
    }
}

/// Does the inverse of `ResolvedSort::get_number_sort_generality`.
/// 
/// Returns as follows:
/// - 0: `Pos`
/// - 1: `Nat`
/// - 2: `Int`
/// - 3: `Real`
fn get_number_sort_from_generality(
    sort_context: &ResolvedSortContext,
    generality: u32,
) -> Interned<ResolvedSort> {
    match generality {
        0 => sort_context.get_primitive_sort(PrimitiveSort::Pos),
        1 => sort_context.get_primitive_sort(PrimitiveSort::Nat),
        2 => sort_context.get_primitive_sort(PrimitiveSort::Int),
        3 => sort_context.get_primitive_sort(PrimitiveSort::Real),
        _ => panic!("{:?} is not a number sort generality", generality),
    }
}

fn display_number_sort_generality(generality: u32) -> &'static str {
    match generality {
        0 => "Pos",
        1 => "Nat",
        2 => "Int",
        3 => "Real",
        _ => panic!("{:?} is not a number sort generality", generality),
    }
}
