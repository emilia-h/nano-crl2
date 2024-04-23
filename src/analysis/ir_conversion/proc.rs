
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::expr::convert_ir_expr;
use crate::analysis::ir_conversion::module::{ModuleIrMapping, SemanticError};
use crate::analysis::ir_conversion::sort::convert_ir_sort;
use crate::core::syntax::Identifier;
use crate::ir::decl::{DeclId, IrDeclEnum};
use crate::ir::proc::{IrProc, IrProcEnum, ProcId};
use crate::model::proc::{Proc, ProcEnum};

use std::collections::hash_map::HashMap;
use std::rc::Rc;

/// Constructs the intermediate representation of a process.
/// 
/// Note that this also maps the given ID to the constructed IR of `proc` and
/// that it generates IDs for its child nodes and maps those recursively.
/// 
/// Returns `result_id` for convenience.
pub fn convert_ir_proc(
    context: &AnalysisContext,
    id_map: &mut HashMap<Identifier, DeclId>,
    proc: &Rc<Proc>,
    result_id: ProcId,
    ir_mapping: &mut ModuleIrMapping,
) -> Result<ProcId, SemanticError> {
    match &proc.value {
        ProcEnum::Action { value } => {
            let decl_id = match id_map.get(&value.id) {
                Some(&decl_id) => decl_id,
                None => return Err(SemanticError::IdentifierError {
                    message: "unknown identifier".to_owned(),
                    id: value.id.clone(),
                }),
            };

            let mut args = Vec::with_capacity(value.args.len());
            for arg in &value.args {
                args.push(convert_ir_expr(
                    context, id_map,
                    arg, context.generate_expr_id(ir_mapping.module),
                    ir_mapping,
                )?);
            }

            let decl = ir_mapping.decls.get(&decl_id).unwrap();
            match &decl.value {
                IrDeclEnum::Action { .. } => {
                    ir_mapping.procs.insert(result_id, IrProc {
                        value: IrProcEnum::MultiAction { actions: vec![ (
                            decl_id,
                            args,
                        ) ] },
                        loc: proc.loc,
                    });
                },
                IrDeclEnum::Process { .. } => {
                    ir_mapping.procs.insert(result_id, IrProc {
                        value: IrProcEnum::Name { id: decl_id, args },
                        loc: proc.loc,
                    });
                },
                IrDeclEnum::Constructor { .. } |
                IrDeclEnum::GlobalVariable { .. } |
                IrDeclEnum::LocalVariable { .. } |
                IrDeclEnum::Map { .. } => return Err(SemanticError::NodeKindError {
                    message: format!("expected an identifier referring to an action or process, but `{}` is data", value.id),
                }),
                IrDeclEnum::Sort { .. } => return Err(SemanticError::NodeKindError {
                    message: format!("expected an identifier referring to an action or process, but `{}` is a sort", value.id),
                }),
            }
        },
        ProcEnum::Delta => {
            ir_mapping.procs.insert(result_id, IrProc {
                value: IrProcEnum::Delta,
                loc: proc.loc,
            });
        },
        ProcEnum::Tau => {
            ir_mapping.procs.insert(result_id, IrProc {
                value: IrProcEnum::MultiAction { actions: Vec::new() },
                loc: proc.loc,
            });
        },
        ProcEnum::Block { ids: _, proc: _ } => {
            todo!()
        },
        ProcEnum::Allow { multi_ids: _, proc: _ } => {
            todo!()
        },
        ProcEnum::Hide { ids: _, proc: _ } => {
            todo!()
        },
        ProcEnum::Rename { mappings: _, proc: _ } => {
            todo!()
        },
        ProcEnum::Comm { mappings: _, proc: _ } => {
            todo!()
        },
        ProcEnum::Add { lhs, rhs } => {
            add_binary_ir_proc(
                context, id_map,
                lhs, rhs, result_id,
                ir_mapping,
                |l, r| IrProc {
                    value: IrProcEnum::Add { lhs: l, rhs: r },
                    loc: proc.loc,
                },
            )?;
        },
        ProcEnum::Sum { variables, proc } => {
            for variable_decl in variables {
                for id in &variable_decl.ids {
                    let decl_id = context.generate_decl_id(ir_mapping.module);
                    id_map.insert(id.clone(), decl_id);
                }
            }

            // NOTE: we assume that `variables` contains at least one variable
            // TODO
            let mut intermediate_id = convert_ir_proc(
                context, id_map,
                proc, context.generate_proc_id(ir_mapping.module),
                ir_mapping,
            )?;
            // sum x: X, y: Y . z is equivalent to sum x: X . sum y: Y . z
            // we work inside out here
            for (i, variable_decl) in variables.iter().enumerate().rev() {
                for (j, id) in variable_decl.ids.iter().enumerate().rev() {
                    eprintln!("({}, {})", i, j);
                    let next_id = if i == 0 && j == 0 {
                        // the last generated IrProc has to be mapped to from result_id
                        result_id
                    } else {
                        context.generate_proc_id(ir_mapping.module)
                    };
                    let variable = id_map.remove(id).unwrap();

                    let sort_id = convert_ir_sort(
                        context, id_map,
                        &Some(Rc::clone(&variable_decl.sort)),
                        context.generate_sort_id(ir_mapping.module),
                        ir_mapping,
                    )?;

                    // intermediate := sum variable : sort . intermediate
                    ir_mapping.procs.insert(next_id, IrProc {
                        value: IrProcEnum::Sum {
                            variable,
                            sort: sort_id,
                            proc: intermediate_id,
                        },
                        loc: proc.loc,
                    });
                    intermediate_id = next_id;
                }
            }
        },
        ProcEnum::Parallel { lhs, rhs } => {
            add_binary_ir_proc(
                context, id_map,
                lhs, rhs, result_id,
                ir_mapping,
                |l, r| IrProc {
                    value: IrProcEnum::Parallel { lhs: l, rhs: r },
                    loc: proc.loc,
                },
            )?;
        },
        ProcEnum::RightParallel { lhs, rhs } => {
            add_binary_ir_proc(
                context, id_map,
                lhs, rhs, result_id,
                ir_mapping,
                |l, r| IrProc {
                    value: IrProcEnum::RightParallel { lhs: l, rhs: r },
                    loc: proc.loc,
                },
            )?;
        },
        ProcEnum::Multi { lhs: _, rhs: _ } => {
            todo!()
        },
        ProcEnum::IfThenElse { condition, then_proc, else_proc } => {
            let c = convert_ir_expr(
                context, id_map,
                condition, context.generate_expr_id(ir_mapping.module),
                ir_mapping,
            )?;

            let t = convert_ir_proc(
                context, id_map,
                then_proc, context.generate_proc_id(ir_mapping.module),
                ir_mapping,
            )?;

            let e = if let Some(else_proc) = else_proc {
                convert_ir_proc(
                    context, id_map,
                    else_proc, context.generate_proc_id(ir_mapping.module),
                    ir_mapping,
                )?
            } else {
                // `a -> b` is equivalent to `a -> b <> delta`
                let else_id = context.generate_proc_id(ir_mapping.module);
                ir_mapping.procs.insert(else_id, IrProc {
                    value: IrProcEnum::Delta,
                    loc: proc.loc,
                });
                else_id
            };

            ir_mapping.procs.insert(result_id, IrProc {
                value: IrProcEnum::IfThenElse { condition: c, then_proc: t, else_proc: e },
                loc: proc.loc,
            });
        },
        ProcEnum::LeftShift { .. } => {
            unimplemented!()
        },
        ProcEnum::Concat { lhs, rhs } => {
            let l = convert_ir_proc(
                context, id_map,
                lhs, context.generate_proc_id(ir_mapping.module),
                ir_mapping,
            )?;
            let r = convert_ir_proc(
                context, id_map,
                rhs, context.generate_proc_id(ir_mapping.module),
                ir_mapping,
            )?;

            ir_mapping.procs.insert(result_id, IrProc {
                value: IrProcEnum::Concat { lhs: l, rhs: r },
                loc: proc.loc,
            });
        },
        ProcEnum::Time { .. } => {
            unimplemented!()
        },
    }

    Ok(result_id)
}

fn add_binary_ir_proc<F>(
    context: &AnalysisContext,
    id_map: &mut HashMap<Identifier, DeclId>,
    lhs: &Rc<Proc>,
    rhs: &Rc<Proc>,
    result_id: ProcId,
    ir_mapping: &mut ModuleIrMapping,
    f: F,
) -> Result<(), SemanticError>
where
    F: FnOnce(ProcId, ProcId) -> IrProc,
{
    let l = convert_ir_proc(
        context, id_map,
        lhs, context.generate_proc_id(ir_mapping.module),
        ir_mapping,
    )?;
    let r = convert_ir_proc(
        context, id_map,
        rhs, context.generate_proc_id(ir_mapping.module),
        ir_mapping,
    )?;

    ir_mapping.procs.insert(result_id, f(l, r));

    Ok(())
}
