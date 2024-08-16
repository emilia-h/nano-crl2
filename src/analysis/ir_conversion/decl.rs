
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::expr::convert_ir_expr;
use crate::analysis::ir_conversion::proc::convert_ir_proc;
use crate::analysis::ir_conversion::sort::convert_ir_sort;
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::decl::{IrDecl, IrDeclEnum, IrParam, ParamId};
use crate::ir::expr::{IrRewriteRule, IrRewriteSet, IrRewriteVar, RewriteRuleId, RewriteVarId};
use crate::ir::module::IrModule;
use crate::ir::sort::SortId;
use crate::model::decl::{Decl, DeclEnum};
use crate::model::sort::{Sort, SortEnum};

use std::sync::Arc;

/// Recursively converts the AST nodes that `decl` refers to to IR.
/// 
/// One AST declaration might create several IR declarations.
/// 
/// For an `InitialDecl`, the `result` module will have its `initial` field set
/// to the IR translation of that initial declaration (if it is not already
/// set).
/// 
/// # Panics
/// The given `mapping` must contain an entry for every identifier that is in
/// `decl`, or this function will panic.
/// 
/// Also panics if a sort alias declaration is given with no or multiple
/// identifiers. For instance, `sort A, B = Nat;` is not syntactically correct
/// and also does not really make sense.
pub fn convert_ir_decl(
    context: &AnalysisContext,
    decl: &Arc<Decl>,
    module: &mut IrModule,
) -> Result<(), ()> {
    let add_def = |module: &mut IrModule, id: &Identifier, id_loc: SourceRange, value: IrDeclEnum| {
        let def_id = context.generate_def_id(module.id);
        let decl_id = context.generate_decl_id(module.id);
        module.decls.insert(decl_id, IrDecl {
            def_id,
            identifier: id.clone(),
            identifier_loc: id_loc,
            value,
            loc: decl.loc,
        });
        module.add_def_source(def_id, decl_id.into());
        decl_id
    };

    match &decl.value {
        DeclEnum::Action { ids, sort } => {
            for (identifier, identifier_loc) in ids {
                let sort_ids = if let Some(sort) = sort {
                    let mut components = Vec::new();
                    decompose_carthesian_sort(context, sort, module, &mut components)?;
                    components
                } else {
                    Vec::new()
                };
                let def_id = context.generate_def_id(module.id);
                let decl_id = context.generate_decl_id(module.id);
                for &sort_id in &sort_ids {
                    module.add_parent(sort_id.into(), decl_id.into());
                }
                module.decls.insert(decl_id, IrDecl {
                    def_id,
                    identifier: identifier.clone(),
                    identifier_loc: *identifier_loc,
                    value: IrDeclEnum::Action {
                        sorts: sort_ids,
                    },
                    loc: decl.loc,
                });
                module.add_def_source(def_id, decl_id.into());
            }
        },
        DeclEnum::Constructor { ids, sort } => {
            for (identifier, identifier_loc) in ids {
                let sort_id = convert_ir_sort(context, sort, module)?;
                let value = IrDeclEnum::Constructor {
                    sort: sort_id,
                };
                let decl_id = add_def(module, identifier, *identifier_loc, value);
                module.add_parent(sort_id.into(), decl_id.into());
            }
        },
        DeclEnum::EquationSet { variables, equations } => {
            let mut ir_variables = Vec::new();
            for variable_decl in variables {
                for (identifier, identifier_loc) in &variable_decl.ids {
                    let sort_id = convert_ir_sort(context, &variable_decl.sort, module)?;
                    let def_id = context.generate_def_id(module.id);
                    ir_variables.push(IrRewriteVar {
                        def_id,
                        identifier: identifier.clone(),
                        identifier_loc: *identifier_loc,
                        sort: sort_id,
                        loc: variable_decl.loc,
                    });
                }
            }

            let mut ir_rules = Vec::new();
            for equation_decl in equations {
                let condition = if let Some(cond) = &equation_decl.condition {
                    Some(convert_ir_expr(context, cond, module)?)
                } else {
                    None
                };
                let lhs = convert_ir_expr(context, &equation_decl.lhs, module)?;
                let rhs = convert_ir_expr(context, &equation_decl.rhs, module)?;
                ir_rules.push(IrRewriteRule { condition, lhs, rhs, loc: equation_decl.loc });
            }

            let rewrite_set_id = context.generate_rewrite_set_id(module.id);

            // add parent and def_source relations for the `var` section
            for (index, ir_variable) in ir_variables.iter().enumerate() {
                let rewrite_var_id = RewriteVarId {
                    rewrite_set: rewrite_set_id,
                    index,
                };
                module.add_parent(ir_variable.sort.into(), rewrite_var_id.into());
                module.add_parent(rewrite_var_id.into(), rewrite_set_id.into());
                module.add_def_source(ir_variable.def_id, rewrite_var_id.into());
            }

            // add parent and def_source relations for the `eqn` section
            for (index, ir_rule) in ir_rules.iter().enumerate() {
                let rewrite_rule_id = RewriteRuleId {
                    rewrite_set: rewrite_set_id,
                    index,
                };
                if let &Some(condition) = &ir_rule.condition {
                    module.add_parent(condition.into(), rewrite_rule_id.into());
                }
                module.add_parent(ir_rule.lhs.into(), rewrite_rule_id.into());
                module.add_parent(ir_rule.rhs.into(), rewrite_rule_id.into());
                module.add_parent(rewrite_rule_id.into(), rewrite_set_id.into());
            }

            module.rewrite_sets.insert(rewrite_set_id, IrRewriteSet {
                variables: ir_variables,
                rules: ir_rules,
            });
        },
        DeclEnum::GlobalVariable { variables } => {
            for variable_decl in variables {
                let sort_id = convert_ir_sort(context, &variable_decl.sort, module)?;
                for (identifier, identifier_loc) in &variable_decl.ids {
                    let value = IrDeclEnum::GlobalVariable {
                        sort: sort_id,
                    };
                    let decl_id = add_def(module, identifier, *identifier_loc, value);
                    module.add_parent(sort_id.into(), decl_id.into());
                }
            }
        },
        DeclEnum::Initial { value } => {
            let sort_id = convert_ir_proc(context, value, module)?;
            if module.initial.is_some() {
                context.error();
                // return Err(SemanticError::InitialProcError {
                //     message: "More than one `init` declaration".to_owned(),
                //     loc: decl.loc,
                // });
                return Err(());
            }
            module.initial = Some(sort_id);
        },
        DeclEnum::Map { id, id_loc, sort } => {
            let sort_id = convert_ir_sort(context, sort, module)?;
            let value = IrDeclEnum::Map { sort: sort_id };
            let decl_id = add_def(module, id, *id_loc, value);
            module.add_parent(sort_id.into(), decl_id.into());
        },
        DeclEnum::Sort { ids, sort } => {
            if let Some(sort) = sort {
                // sort A = something;
                assert_eq!(ids.len(), 1);
                let sort_id = convert_ir_sort(context, sort, module)?;
                let value = IrDeclEnum::SortAlias { sort: sort_id };
                let decl_id = add_def(module, &ids[0].0, ids[0].1, value);
                module.add_parent(sort_id.into(), decl_id.into());
            } else {
                // sort A_1, ..., A_n;
                for (identifier, identifier_loc) in ids {
                    let _ = add_def(module, identifier, *identifier_loc, IrDeclEnum::Sort);
                }
            }
        },
        DeclEnum::Process { id, id_loc, params, proc } => {
            // convert parameters' sorts to IR
            let mut ir_params = Vec::new();
            for variable_decl in params {
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

            // cannot simply reuse `add_def` because of borrow checker issues
            let proc_id = convert_ir_proc(context, proc, module)?;
            let def_id = context.generate_def_id(module.id);
            let decl_id = context.generate_decl_id(module.id);
            for (index, param) in ir_params.iter().enumerate() {
                let param_id = ParamId {
                    decl: decl_id,
                    index,
                };
                module.add_parent(param.sort.into(), param_id.into());
                module.add_parent(param_id.into(), decl_id.into());
                module.add_def_source(param.def_id, param_id.into());
            }
            module.decls.insert(decl_id, IrDecl {
                def_id,
                identifier: id.clone(),
                identifier_loc: *id_loc,
                value: IrDeclEnum::Process { params: ir_params, proc: proc_id },
                loc: decl.loc,
            });
            module.add_parent(proc_id.into(), decl_id.into());
            module.add_def_source(def_id, decl_id.into());
        },
    }
    Ok(())
}

fn decompose_carthesian_sort(
    context: &AnalysisContext,
    sort: &Arc<Sort>,
    module: &mut IrModule,
    result: &mut Vec<SortId>,
) -> Result<(), ()> {
    match &sort.value {
        SortEnum::Carthesian { lhs, rhs } => {
            decompose_carthesian_sort(context, lhs, module, result)?;
            decompose_carthesian_sort(context, rhs, module, result)?;
        },
        _ => result.push(convert_ir_sort(context, sort, module)?),
    }

    Ok(())
}
