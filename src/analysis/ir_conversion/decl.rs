
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::SemanticError;
use crate::analysis::ir_conversion::proc::convert_ir_proc;
use crate::analysis::ir_conversion::sort::convert_ir_sort;
use crate::core::syntax::Identifier;
use crate::ir::decl::{DefId, IrDecl, IrDeclEnum};
use crate::ir::module::IrModule;
use crate::ir::sort::{IrSort, IrSortEnum};
use crate::model::decl::{Decl, DeclEnum};

use std::collections::hash_map::HashMap;
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
    mapping: &HashMap<Identifier, DefId>,
    decl: &Arc<Decl>,
    module: &mut IrModule,
) -> Result<(), SemanticError> {
    match &decl.value {
        DeclEnum::Action { ids, sort } => {
            for id in ids {
                let def_id = *mapping.get(&id).unwrap();
                let sort_id = match sort {
                    Some(sort) => convert_ir_sort(context, sort, module)?,
                    None => {
                        let generated = context.generate_sort_id(module.id);
                        module.sorts.insert(generated, IrSort {
                            value: IrSortEnum::Unit,
                        });
                        generated
                    },
                };
                let decl_id = context.generate_decl_id(module.id);
                module.decls.insert(decl_id, IrDecl {
                    def_id,
                    value: IrDeclEnum::Action { sort: sort_id },
                });
            }
        },
        DeclEnum::Constructor { ids, sort } => {
            for id in ids {
                let def_id = *mapping.get(&id).unwrap();
                let sort_id = convert_ir_sort(context, sort, module)?;
                let decl_id = context.generate_decl_id(module.id);
                module.decls.insert(decl_id, IrDecl {
                    def_id,
                    value: IrDeclEnum::Action { sort: sort_id },
                });
            }
        },
        DeclEnum::EquationSet { variables, equations } => {
            // TODO
        },
        DeclEnum::GlobalVariable { variables } => {
            for variable_decl in variables {
                let sort_id = convert_ir_sort(context, &variable_decl.sort, module)?;
                for id in &variable_decl.ids {
                    let def_id = *mapping.get(&id).unwrap();
                    let decl_id = context.generate_decl_id(module.id);
                    module.decls.insert(decl_id, IrDecl {
                        def_id,
                        value: IrDeclEnum::GlobalVariable { sort: sort_id },
                    });
                }
            }
        },
        DeclEnum::Initial { value } => {
            let sort_id = convert_ir_proc(context, value, module)?;
            if module.initial.is_some() {
                return Err(SemanticError::InitialProcError {
                    message: "More than one `init` declaration".to_owned(),
                    loc: decl.loc,
                });
            }
            module.initial = Some(sort_id);
        },
        DeclEnum::Map { id, sort } => {
            let def_id = *mapping.get(&id).unwrap();
            let sort_id = convert_ir_sort(context, sort, module)?;
            let decl_id = context.generate_decl_id(module.id);
            module.decls.insert(decl_id, IrDecl {
                def_id,
                value: IrDeclEnum::Map { sort: sort_id },
            });
        },
        DeclEnum::Sort { ids, sort } => {
            if let Some(sort) = sort {
                // sort A = something;
                assert_eq!(ids.len(), 1);
                let def_id = *mapping.get(&ids[0]).unwrap();
                let sort_id = convert_ir_sort(context, sort, module)?;
                let decl_id = context.generate_decl_id(module.id);
                module.decls.insert(decl_id, IrDecl {
                    def_id,
                    value: IrDeclEnum::SortAlias { sort: sort_id },
                });
            } else {
                // sort A_1, ..., A_n;
                for id in ids {
                    let def_id = *mapping.get(id).unwrap();
                    let decl_id = context.generate_decl_id(module.id);
                    module.decls.insert(decl_id, IrDecl {
                        def_id,
                        value: IrDeclEnum::Sort,
                    });
                }
            }
        },
        DeclEnum::Process { id, params, proc } => {
            // convert parameters' sorts to IR
            let mut ir_params = Vec::new();
            for variable_decl in params {
                let sort_id = convert_ir_sort(context, &variable_decl.sort, module)?;
                for id in &variable_decl.ids {
                    let def_id = context.generate_def_id(module.id);
                    ir_params.push((def_id, id.clone(), sort_id));
                }
            }

            let proc_id = convert_ir_proc(context, proc, module)?;
            let def_id = *mapping.get(id).unwrap();
            let decl_id = context.generate_decl_id(module.id);
            module.decls.insert(decl_id, IrDecl {
                def_id,
                value: IrDeclEnum::Process { params: ir_params, proc: proc_id },
            });
        },
    }
    Ok(())
}
