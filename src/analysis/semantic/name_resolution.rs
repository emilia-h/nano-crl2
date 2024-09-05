
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::query_ir_module;
use crate::core::syntax::{Identifier, SourceRange};
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

    let module = query_ir_module(context, node.get_module_id())?;

    // note that there are three separate name spaces, namely one for
    // expressions, one for processes, and one for sorts
    match node {
        NodeId::Action(id) => {
            // for processes, name lookup is slightly annoying because
            // there can be multiple actions with the same name but
            // with different signatures
            let action = module.get_action(id);
            let Some(parent) = module.get_parent(id.into()) else {
                let error = format!(
                    "unresolved identifier `{}`",
                    action.identifier,
                );
                context.error(module.id, action.loc, error);
                return Err(());
            };

            let name_lookup = NameLookup {
                value: NameLookupEnum::Proc {},
                identifier: &action.identifier,
                loc: action.loc,
            };
            find_def_of_name(context, parent, &name_lookup, &module)
        },
        NodeId::Decl(_) => panic!("a declaration is not a name"),
        NodeId::Expr(id) => {
            let expr = module.get_expr(id);
            match &expr.value {
                IrExprEnum::Name { identifier } => {
                    // for expressions, name lookup is very annoying, because
                    // there can be multiple maps and constructors with the
                    // same identifier; then the name lookup depends on the sort
                    let Some(parent) = module.get_parent(id.into()) else {
                        let error = format!(
                            "unresolved identifier `{}`",
                            identifier,
                        );
                        context.error(module.id, expr.loc, error);
                        return Err(());
                    };

                    let name_lookup = NameLookup {
                        value: NameLookupEnum::Expr {},
                        identifier,
                        loc: expr.loc,
                    };
                    find_def_of_name(context, parent, &name_lookup, &module)
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
            let sort = module.get_sort(id);
            match &sort.value {
                IrSortEnum::Name { identifier } => {
                    // for sorts, name lookup is simply looking at identifiers
                    let Some(parent) = module.get_parent(id.into()) else {
                        let error = format!(
                            "unresolved identifier `{}`",
                            identifier,
                        );
                        context.error(module.id, sort.loc, error);
                        return Err(());
                    };

                    let name_lookup = NameLookup {
                        value: NameLookupEnum::Sort {},
                        identifier,
                        loc: sort.loc,
                    };
                    find_def_of_name(context, parent, &name_lookup, &module)
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
            if let NameLookupEnum::Expr {} = &name_lookup.value {
                if let IrDeclEnum::Process { params, .. } = &decl.value {
                    let mut result = None;
                    for param in params {
                        if &param.identifier == name_lookup.identifier {
                            if result.is_some() {
                                let error = format!(
                                    "multiple parameters with name `{}`",
                                    &param.identifier,
                                );
                                context.error(module.id, param.identifier_loc, error);
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
            if let NameLookupEnum::Expr {} = &name_lookup.value {
                let expr = module.exprs.get(&id).unwrap();
                match &expr.value {
                    IrExprEnum::Binder { def_id, identifier: i2, sort, .. } => {
                        if name_lookup.identifier == i2 {
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

            match &name_lookup.value {
                NameLookupEnum::Expr {} => {
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
                NameLookupEnum::Proc {} => {
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
                NameLookupEnum::Sort {} => {
                    // sort name lookup is very easy!
                    let mut result = None;
                    for decl in module.decls.values() {
                        if matches!(decl.value,
                            IrDeclEnum::Sort |
                            IrDeclEnum::SortAlias { .. }
                        ) && name_lookup.identifier == &decl.identifier {
                            if result.is_some() {
                                let error = format!(
                                    "multiple `sort` declarations with identifier `{}`",
                                    decl.identifier,
                                );
                                context.error(module.id, decl.identifier_loc, error);
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
            if let NameLookupEnum::Expr {} = &name_lookup.value {
                let proc = module.procs.get(&id).unwrap();
                if let IrProcEnum::Sum { def_id, identifier: i2, .. } = &proc.value {
                    if name_lookup.identifier == i2 {
                        return Ok(*def_id);
                    }
                }
            }
        },
        NodeId::RewriteRule(_) => {},
        NodeId::RewriteSet(id) => {
            if let NameLookupEnum::Expr {} = &name_lookup.value {
                let rewrite_set = module.get_rewrite_set(id);
                for variable in &rewrite_set.variables {
                    if name_lookup.identifier == &variable.identifier {
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
        let error = format!(
            "undefined identifier `{}`",
            name_lookup.identifier.get_value(),
        );
        context.error(module.id, name_lookup.loc, error);
        Err(())
    }
}

struct NameLookup<'a> {
    identifier: &'a Identifier,
    value: NameLookupEnum,
    loc: SourceRange,
}

enum NameLookupEnum {
    Expr {},
    Proc {},
    Sort {},
}
