//! Implements functions for navigating and accessing nodes in the intermediate
//! representation (IR).

use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::decl::{DefId, IrDeclEnum, ParamId};
use crate::ir::expr::{IrExprEnum, IrRewriteVar, RewriteRuleId, RewriteVarId};
use crate::ir::module::{IrModule, NodeId};
use crate::ir::proc::{ActionId, IrProcEnum};
use crate::ir::sort::IrSortEnum;

impl<'m> IntoIterator for &'m IrModule {
    type IntoIter = IrIterator<'m>;
    type Item = NodeId;

    fn into_iter(self) -> Self::IntoIter {
        IrIterator::new(self, self.id.into())
    }
}

/// Iterates its module using depth-first search (preoder tree traversal),
/// which will output each node ID with its source location once.
pub struct IrIterator<'m> {
    module: &'m IrModule,
    stack: Vec<NodeId>,
}

impl<'m> IrIterator<'m> {
    pub fn new(module: &'m IrModule, starting_node: NodeId) -> Self {
        IrIterator {
            module,
            stack: vec![starting_node],
        }
    }
}

impl<'m> Iterator for IrIterator<'m> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(node_id) = self.stack.pop() {
            let writer = |item| self.stack.push(item);
            for_each_child(&self.module, node_id, writer);
            Some(node_id)
        } else {
            None
        }
    }
}

/// Calls `writer` once for each direct child of `node`.
pub fn for_each_child<T>(
    module: &IrModule,
    node: NodeId,
    mut writer: T,
)
where
    T: FnMut(NodeId),
{
    match node {
        NodeId::Action(id) => {
            let action = module.get_action(id);
            for &arg in &action.args {
                writer(arg.into());
            }
        },
        NodeId::Decl(id) => {
            let decl = module.get_decl(id);
            match &decl.value {
                IrDeclEnum::Action { params: sorts } => {
                    for &sort_id in sorts {
                        writer(sort_id.into());
                    }
                },
                IrDeclEnum::Constructor { params, sort } => {
                    for param in params {
                        writer((*param).into());
                    }
                    writer((*sort).into());
                },
                IrDeclEnum::GlobalVariable { sort } |
                IrDeclEnum::Map { sort } |
                IrDeclEnum::SortAlias { sort } => {
                    writer((*sort).into());
                },
                IrDeclEnum::Process { params, proc } => {
                    for index in 0 .. params.len() {
                        writer(ParamId { parent: id.into(), index }.into());
                    }
                    writer((*proc).into());
                },
                IrDeclEnum::Sort => {},
            }
        },
        NodeId::Expr(id) => {
            let expr = module.get_expr(id);
            match &expr.value {
                IrExprEnum::Name { .. } |
                IrExprEnum::NumberLiteral { .. } |
                IrExprEnum::BoolLiteral { .. } => {},
                IrExprEnum::ListLiteral { values } |
                IrExprEnum::SetLiteral { values } => {
                    for &expr_id in values {
                        writer(expr_id.into());
                    }
                },
                IrExprEnum::BagLiteral { values } => {
                    for &(expr_id1, expr_id2) in values {
                        writer(expr_id1.into());
                        writer(expr_id2.into());
                    }
                },
                IrExprEnum::FunctionUpdate { function, lhs, rhs } => {
                    writer((*function).into());
                    writer((*lhs).into());
                    writer((*rhs).into());
                },
                IrExprEnum::Apply { callee, args } => {
                    writer((*callee).into());
                    for &expr_id in args {
                        writer(expr_id.into());
                    }
                },
                IrExprEnum::Binder { sort, value, .. } => {
                    writer((*sort).into());
                    writer((*value).into());
                },
                IrExprEnum::Lambda { params, value } => {
                    for index in 0 .. params.len() {
                        writer(ParamId { parent: id.into(), index }.into());
                    }
                    writer((*value).into());
                },
                IrExprEnum::Unary { value, .. } => {
                    writer((*value).into());
                },
                IrExprEnum::Binary { lhs, rhs, .. } => {
                    writer((*lhs).into());
                    writer((*rhs).into());
                },
                IrExprEnum::If { condition, then_expr, else_expr } => {
                    writer((*condition).into());
                    writer((*then_expr).into());
                    writer((*else_expr).into());
                },
                IrExprEnum::Where { inner, assigned, .. } => {
                    writer((*inner).into());
                    writer((*assigned).into());
                },
            }
        },
        NodeId::Module(id) => {
            assert_eq!(module.id, id);
            for (&decl_id, _) in &module.decls {
                writer(decl_id.into());
            }
            for (&rewrite_set_id, _) in &module.rewrite_sets {
                writer(rewrite_set_id.into());
            }
        },
        NodeId::Param(id) => {
            let param = module.get_param(id);
            writer(param.sort.into());
        },
        NodeId::Proc(id) => {
            let proc = module.get_proc(id);
            match &proc.value {
                IrProcEnum::Binary { lhs, rhs, .. } => {
                    writer((*lhs).into());
                    writer((*rhs).into());
                },
                IrProcEnum::Delta => {},
                IrProcEnum::IfThenElse { condition, then_proc, else_proc } => {
                    writer((*condition).into());
                    writer((*then_proc).into());
                    writer((*else_proc).into());
                },
                IrProcEnum::MultiAction { actions } => {
                    for index in 0 .. actions.len() {
                        writer(ActionId { proc: id, index }.into());
                    }
                },
                IrProcEnum::Sum { sort, proc, .. } => {
                    writer((*sort).into());
                    writer((*proc).into());
                },
            }
        },
        NodeId::RewriteRule(id) => {
            let rewrite_rule = module.get_rewrite_rule(id);
            if let Some(condition) = rewrite_rule.condition {
                writer(condition.into());
            }
            writer(rewrite_rule.lhs.into());
            writer(rewrite_rule.rhs.into());
        },
        NodeId::RewriteSet(id) => {
            let rewrite_set = module.get_rewrite_set(id);
            for index in 0 .. rewrite_set.variables.len() {
                writer(RewriteVarId { rewrite_set: id, index }.into());
            }
            for index in 0 .. rewrite_set.rules.len() {
                writer(RewriteRuleId { rewrite_set: id, index }.into());
            }
        },
        NodeId::RewriteVar(id) => {
            let rewrite_var = module.get_rewrite_var(id);
            writer(rewrite_var.sort.into());
        },
        NodeId::Sort(id) => {
            let sort = module.get_sort(id);
            match &sort.value {
                IrSortEnum::Primitive { .. } => {},
                IrSortEnum::Generic { subsort, .. } => {
                    writer((*subsort).into());
                },
                IrSortEnum::Function { lhs, rhs } => {
                    for &param_sort_id in lhs {
                        writer(param_sort_id.into());
                    }
                    writer((*rhs).into());
                },
                IrSortEnum::Name { .. } => {},
                IrSortEnum::Def { .. } => {},
            }
        },
    }
}

/// For a node in the IR, finds the definition ID that is defined by this node,
/// the identifier for it and the location of that identifier.
/// 
/// For instance, for the node `forall x: Nat . x == y` with node ID
/// `0.expr.0`, where `x` has `def_id = 0.def.0`, calling
/// `get_def_data(0.expr.0)` will return `(0.def.0, "x", location of "x")`.
/// 
/// The function `module.get_def_source` is in essence an inverse of this; this
/// is because `get_def_data(module, module.get_def_source(def_id)).0.unwrap()
/// == def_id`.
pub fn get_def_data(
    module: &IrModule,
    node: NodeId,
) -> Option<(DefId, &Identifier, SourceRange)> {
    match node {
        NodeId::Action(_) => None,
        NodeId::Decl(id) => {
            let decl = module.get_decl(id);
            Some((decl.def_id, &decl.identifier, decl.identifier_loc))
        },
        NodeId::Expr(id) => {
            let expr = module.get_expr(id);
            match &expr.value {
                IrExprEnum::Binder { def_id, identifier, identifier_loc, .. } => {
                    Some((*def_id, identifier, *identifier_loc))
                },
                _ => None,
            }
        },
        NodeId::Module(_) => None,
        NodeId::Param(id) => {
            let param = module.get_param(id);
            Some((param.def_id, &param.identifier, param.identifier_loc))
        },
        NodeId::Proc(id) => {
            let proc = module.get_proc(id);
            match &proc.value {
                IrProcEnum::Sum { def_id, identifier, identifier_loc, .. } => {
                    Some((*def_id, identifier, *identifier_loc))
                },
                _ => None,
            }
        },
        NodeId::RewriteRule(_) => None,
        NodeId::RewriteSet(_) => None,
        NodeId::RewriteVar(id) => {
            let IrRewriteVar { def_id, identifier, identifier_loc, .. } =
                module.get_rewrite_var(id);
            Some((*def_id, &identifier, *identifier_loc))
        },
        NodeId::Sort(_) => None,
    }
}

/// Returns the source location of an arbitrary node in the IR.
/// 
/// # Panics
/// If the module that `node` is in does not match the given `module`, this
/// function will panic.
pub fn get_node_loc(
    module: &IrModule,
    node: NodeId,
) -> SourceRange {
    assert_eq!(node.get_module_id(), module.id);
    match node {
        NodeId::Action(id) => module.get_action(id).loc,
        NodeId::Decl(id) => module.get_decl(id).loc,
        NodeId::Expr(id) => module.get_expr(id).loc,
        NodeId::Module(_) => module.loc,
        NodeId::Param(id) => module.get_param(id).loc,
        NodeId::Proc(id) => module.get_proc(id).loc,
        NodeId::RewriteRule(id) => module.get_rewrite_rule(id).loc,
        NodeId::RewriteSet(id) => module.get_rewrite_set(id).loc,
        NodeId::RewriteVar(id) => module.get_rewrite_var(id).loc,
        NodeId::Sort(id) => module.get_sort(id).loc,
    }
}
