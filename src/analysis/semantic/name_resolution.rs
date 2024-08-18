
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::query_ir_module;
use crate::core::syntax::Identifier;
use crate::ir::decl::{DefId, IrDeclEnum};
use crate::ir::expr::IrExprEnum;
use crate::ir::module::{IrModule, NodeId};
use crate::ir::proc::IrProcEnum;
use crate::ir::sort::IrSortEnum;

/// # Panics
/// The `node` must refer to a node that refers to a declaration using an
/// identifier, or the function will panic.
pub fn query_def_of_name(
    context: &AnalysisContext,
    node: NodeId,
) -> Result<DefId, ()> {
    // TODO cache

    let ir_module = query_ir_module(context, node.get_module_id())?;

    // note that there are three separate name spaces, namely one for
    // expressions, one for processes, and one for sorts
    match node {
        NodeId::Action(id) => {
            // for processes, name lookup is slightly annoying because
            // there can be multiple actions with the same name but
            // with different signatures
            let action = ir_module.get_action(id);
            if let Some(parent) = ir_module.get_parent(id.into()) {
                find_def_of_name(
                    context,
                    parent,
                    &NameLookup::Proc { identifier: &action.identifier },
                    &ir_module,
                )
            } else {
                context.error();
                Err(())
            }
        },
        NodeId::Decl(_) => panic!("a declaration is not a name"),
        NodeId::Expr(id) => {
            match &ir_module.get_expr(id).value {
                IrExprEnum::Name { identifier } => {
                    // for expressions, name lookup is very annoying, because
                    // there can be multiple maps and constructors with the
                    // same identifier; then the name lookup depends on the sort
                    if let Some(parent) = ir_module.get_parent(id.into()) {
                        find_def_of_name(
                            context,
                            parent,
                            &NameLookup::Expr { identifier },
                            &ir_module,
                        )
                    } else {
                        context.error();
                        Err(())
                    }
                },
                _ => panic!("expression {:?} is not a name", id),
            }
        },
        NodeId::Module(_) => panic!("a module is not a name"),
        NodeId::Param(_) => panic!("a parameter is not a name"),
        NodeId::Proc(_) => panic!("a process is not a name"),
        NodeId::RewriteSet(_) => panic!("a rewrite set is not a name"),
        NodeId::RewriteRule(_) => panic!("a rewrite rule is not a name"),
        NodeId::RewriteVar(_) => panic!("a rewrite var is not a name"),
        NodeId::Sort(id) => {
            match &ir_module.get_sort(id).value {
                IrSortEnum::Name { identifier } => {
                    // for sorts, name lookup is simply looking at identifiers
                    let Some(parent) = ir_module.get_parent(id.into()) else {
                        context.error();
                        return Err(());
                    };
                    find_def_of_name(
                        context,
                        parent,
                        &NameLookup::Sort { identifier },
                        &ir_module,
                    )
                },
                _ => panic!("sort {:?} is not a name", id),
            }
        },
    }
}

/// Recursively searches all parents for the identifier that has to be looked
/// up and finds the one that matches according to the rules of mCRL2.
fn find_def_of_name(
    context: &AnalysisContext,
    node: NodeId,
    name_lookup: &NameLookup,
    module: &IrModule,
) -> Result<DefId, ()> {
    match node {
        NodeId::Action(_) => {},
        NodeId::Decl(id) => {
            let decl = module.decls.get(&id).unwrap();
            if let NameLookup::Expr { identifier } = name_lookup {
                if let IrDeclEnum::Process { params, .. } = &decl.value {
                    let mut result = None;
                    for param in params {
                        if &param.identifier == *identifier {
                            if result.is_some() {
                                context.error();
                                return Err(());
                            }
                            result = Some(param.def_id);
                        }
                    }
                    if let Some(result) = result {
                        return Ok(result);
                    }
                }
            }
            // checks for the identifiers of module-level declarations
            // themselves will be handled in the `NodeId::Module` case
        },
        NodeId::Expr(id) => {
            if let NameLookup::Expr { identifier } = name_lookup {
                let expr = module.exprs.get(&id).unwrap();
                match &expr.value {
                    IrExprEnum::Binder { def_id, identifier: i2, sort, .. } => {
                        if *identifier == i2 {
                            todo!()
                        }
                    },
                    IrExprEnum::Where {} => {
                        todo!()
                    },
                    _ => {},
                }
            }
        },
        NodeId::Module(id) => {
            // loop over top-level declarations and find the best matching one
            // (where the method of matching depends on if we're looking for an
            // expression, process or sorts)
            let borrow_checker_workaround;
            let module = if id == module.id {
                module
            } else {
                borrow_checker_workaround = query_ir_module(context, id)?;
                &*borrow_checker_workaround
            };

            match name_lookup {
                NameLookup::Expr { identifier } => {
                    for decl in module.decls.values() {
                        match &decl.value {
                            IrDeclEnum::Constructor { sort } => {
                                todo!()
                            },
                            IrDeclEnum::GlobalVariable { sort } => {
                                todo!()
                            },
                            IrDeclEnum::Map { sort } => {
                                todo!()
                            },
                            _ => {},
                        }
                    }
                },
                NameLookup::Proc { identifier } => {
                    for decl in module.decls.values() {
                        match &decl.value {
                            IrDeclEnum::Action { sorts } => {
                                todo!()
                            },
                            IrDeclEnum::Process { params, proc } => {
                                todo!()
                            },
                            _ => {},
                        }
                    }
                },
                NameLookup::Sort { identifier } => {
                    // sort name lookup is very easy!
                    let mut result = None;
                    for decl in module.decls.values() {
                        if matches!(decl.value,
                            IrDeclEnum::Sort |
                            IrDeclEnum::SortAlias { .. }
                        ) && *identifier == &decl.identifier {
                            if result.is_some() {
                                context.error();
                                return Err(());
                            }
                            result = Some(decl.def_id);
                        }
                    }
                    if let Some(result) = result {
                        return Ok(result);
                    }
                },
            }
        },
        NodeId::Param(_) => {
            // this case should never be encountered for `NameLookup::Expr`
        },
        NodeId::Proc(id) => {
            if let NameLookup::Expr { identifier } = name_lookup {
                let proc = module.procs.get(&id).unwrap();
                if let IrProcEnum::Sum { def_id, identifier: i2, .. } = &proc.value {
                    if *identifier == i2 {
                        return Ok(*def_id);
                    }
                }
            }
        },
        NodeId::RewriteRule(_) => {},
        NodeId::RewriteSet(id) => {
            if let NameLookup::Expr { identifier } = name_lookup {
                let rewrite_set = module.get_rewrite_set(id);
                for variable in &rewrite_set.variables {
                    if *identifier == &variable.identifier {
                        return Ok(variable.def_id);
                    }
                }
            }
        },
        NodeId::RewriteVar(_) => {
            // this case should never be encountered for `NameLookup::Expr`
        },
        NodeId::Sort(_) => {},
    }

    // if the ID did not match a definition bound by the current node, recurse
    // with the parent if it exists
    if let Some(parent) = module.get_parent(node) {
        find_def_of_name(context, parent, name_lookup, module)
    } else {
        context.error(); // did not find identifier
        Err(())
    }
}

enum NameLookup<'a> {
    Expr {
        identifier: &'a Identifier,
    },
    Proc {
        identifier: &'a Identifier,
    },
    Sort {
        identifier: &'a Identifier,
    },
}
