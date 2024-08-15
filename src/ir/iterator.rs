//! Implements functions for converting from in-source location to a node ID in
//! the intermediate representation (IR).

use crate::ir::decl::{DefId, IrDeclEnum, ParamId};
use crate::ir::expr::IrExprEnum;
use crate::ir::module::{IrModule, NodeId};
use crate::ir::proc::{ActionId, IrProcEnum};
use crate::ir::sort::IrSortEnum;

impl<'a> IntoIterator for &'a IrModule {
    type IntoIter = IrIterator<'a>;
    type Item = IrIteratorItem;

    fn into_iter(self) -> Self::IntoIter {
        IrIterator::new(self, self.id.into())
    }
}

pub enum IrIteratorItem {
    Node(NodeId),
    Def(DefId),
}

/// Iterates its module using depth-first search (preoder tree traversal),
/// which will output each node ID with its source location once.
pub struct IrIterator<'a> {
    module: &'a IrModule,
    /// The first element is the node ID. The second element in each pair is
    /// the index that this child has in its parent.
    stack: Vec<IrIteratorItem>,
}

impl<'a> IrIterator<'a> {
    pub fn new(module: &'a IrModule, starting_node: NodeId) -> Self {
        IrIterator {
            module,
            stack: vec![IrIteratorItem::Node(starting_node)],
        }
    }

    /// Convenience function for the iterator.
    fn push_node<T>(&mut self, id: T)
    where
        T: Into<NodeId>,
    {
        self.stack.push(IrIteratorItem::Node(id.into()));
    }

    fn push_def(&mut self, id: DefId) {
        self.stack.push(IrIteratorItem::Def(id));
    }
}

impl<'a> Iterator for IrIterator<'a> {
    type Item = IrIteratorItem;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(iterator_item) = self.stack.pop() {
            match iterator_item {
                IrIteratorItem::Node(NodeId::Action(id)) => {
                    let action = self.module.get_action(id);
                    for &arg in &action.args {
                        self.push_node(arg);
                    }
                },
                IrIteratorItem::Node(NodeId::Decl(id)) => {
                    let decl = self.module.get_decl(id);
                    self.push_def(decl.def_id);
                    match &decl.value {
                        IrDeclEnum::Action { sorts } => {
                            for &sort_id in sorts {
                                self.push_node(sort_id);
                            }
                        },
                        IrDeclEnum::Constructor { sort } |
                        IrDeclEnum::GlobalVariable { sort } |
                        IrDeclEnum::Map { sort } |
                        IrDeclEnum::SortAlias { sort } => {
                            self.push_node(*sort);
                        },
                        IrDeclEnum::Process { params, proc } => {
                            for index in 0 .. params.len() {
                                self.push_node(ParamId { decl: id, index });
                            }
                            self.push_node(*proc);
                        },
                        IrDeclEnum::Sort => {},
                    }
                },
                IrIteratorItem::Node(NodeId::Expr(id)) => {
                    let expr = self.module.get_expr(id);
                    match &expr.value {
                        IrExprEnum::Name { .. } |
                        IrExprEnum::NumberLiteral { .. } |
                        IrExprEnum::BoolLiteral { .. } => {},
                        IrExprEnum::ListLiteral { values } |
                        IrExprEnum::SetLiteral { values } => {
                            for &expr_id in values {
                                self.push_node(expr_id);
                            }
                        },
                        IrExprEnum::BagLiteral { values } => {
                            for &(expr_id1, expr_id2) in values {
                                self.push_node(expr_id1);
                                self.push_node(expr_id2);
                            }
                        },
                        IrExprEnum::FunctionUpdate { function, lhs, rhs } => {
                            self.push_node(*function);
                            self.push_node(*lhs);
                            self.push_node(*rhs);
                        },
                        IrExprEnum::Apply { callee, args } => {
                            self.push_node(*callee);
                            for &expr_id in args {
                                self.push_node(expr_id);
                            }
                        },
                        IrExprEnum::Binder { def_id, sort, expr, .. } => {
                            self.push_def(*def_id);
                            self.push_node(*sort);
                            self.push_node(*expr);
                        },
                        IrExprEnum::Unary { value, .. } => {
                            self.push_node(*value);
                        },
                        IrExprEnum::Binary { lhs, rhs, .. } => {
                            self.push_node(*lhs);
                            self.push_node(*rhs);
                        },
                        IrExprEnum::If { condition, then_expr, else_expr } => {
                            self.push_node(*condition);
                            self.push_node(*then_expr);
                            self.push_node(*else_expr);
                        },
                        IrExprEnum::Where {  } => {

                        },
                    }
                },
                IrIteratorItem::Node(NodeId::Module(id)) => {
                    assert_eq!(self.module.id, id);
                    for (&decl_id, _) in &self.module.decls {
                        self.push_node(decl_id);
                    }
                },
                IrIteratorItem::Node(NodeId::Param(id)) => {
                    let param = self.module.get_param(id);
                    self.push_node(param.sort);
                },
                IrIteratorItem::Node(NodeId::Proc(id)) => {
                    let proc = self.module.get_proc(id);
                    match &proc.value {
                        IrProcEnum::Binary { lhs, rhs, .. } => {
                            self.push_node(*lhs);
                            self.push_node(*rhs);
                        },
                        IrProcEnum::Delta => {},
                        IrProcEnum::IfThenElse { condition, then_proc, else_proc } => {
                            self.push_node(*condition);
                            self.push_node(*then_proc);
                            self.push_node(*else_proc);
                        },
                        IrProcEnum::MultiAction { actions } => {
                            for index in 0 .. actions.len() {
                                self.push_node(ActionId { proc: id, index });
                            }
                        },
                        IrProcEnum::Sum { def_id, sort, proc, .. } => {
                            self.push_def(*def_id);
                            self.push_node(*sort);
                            self.push_node(*proc);
                        },
                    }
                },
                IrIteratorItem::Node(NodeId::RewriteRule(id)) => {
                    let rewrite_rule = self.module.rewrite_rules.get(&id).unwrap();
                    todo!()
                },
                IrIteratorItem::Node(NodeId::Sort(id)) => {
                    let sort = self.module.get_sort(id);
                    match &sort.value {
                        IrSortEnum::Primitive { .. } => {},
                        IrSortEnum::Generic { subsort, .. } => {
                            self.push_node(*subsort);
                        },
                        IrSortEnum::Carthesian { lhs, rhs } => {
                            self.push_node(*lhs);
                            self.push_node(*rhs);
                        },
                        IrSortEnum::Function { lhs, rhs } => {
                            self.push_node(*lhs);
                            self.push_node(*rhs);
                        },
                        IrSortEnum::Name { .. } => {},
                        IrSortEnum::Struct {  } => {

                        },
                    }
                },
                IrIteratorItem::Def(_) => {},
            }
            Some(iterator_item)
        } else {
            None
        }
    }
}
