
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::expr::convert_ir_expr;
use crate::analysis::ir_conversion::sort::convert_ir_sort;
use crate::core::syntax::Identifier;
use crate::ir::expr::ExprId;
use crate::ir::module::IrModule;
use crate::ir::proc::{BinaryProcOp, IrProc, IrProcEnum, ProcId};
use crate::model::proc::{Proc, ProcEnum};

use std::sync::Arc;

/// Constructs the intermediate representation of a process.
/// 
/// Note that this also maps the given ID to the constructed IR of `proc` and
/// that it generates IDs for its child nodes and maps those recursively.
/// 
/// Returns `result_id` for convenience.
pub fn convert_ir_proc(
    context: &AnalysisContext,
    proc: &Arc<Proc>,
    module: &mut IrModule,
) -> Result<ProcId, ()> {
    let add_proc = |module: &mut IrModule, value: IrProcEnum| {
        let proc_id = context.generate_proc_id(module.id);
        module.procs.insert(proc_id, IrProc {
            value,
            loc: proc.loc,
        });
        proc_id
    };

    let convert_binary_proc = |
        module: &mut IrModule,
        op: BinaryProcOp,
        lhs: &Arc<Proc>,
        rhs: &Arc<Proc>,
    | -> Result<ProcId, ()> {
        let lhs_id = convert_ir_proc(context, lhs, module)?;
        let rhs_id = convert_ir_proc(context, rhs, module)?;        
        let proc_id = add_proc(module, IrProcEnum::Binary {
            op,
            lhs: lhs_id,
            rhs: rhs_id,
        });
        module.add_parent(lhs_id.into(), proc_id.into());
        module.add_parent(rhs_id.into(), proc_id.into());
        Ok(proc_id)
    };

    Ok(match &proc.value {
        ProcEnum::Action { id, args } => {
            let mut arg_ids = Vec::with_capacity(args.len());
            for arg in args {
                arg_ids.push(convert_ir_expr(context, arg, module)?);
            }
            let proc_id = context.generate_proc_id(module.id);
            for &arg_id in &arg_ids {
                module.add_parent(arg_id.into(), proc_id.into());
            }
            module.procs.insert(proc_id, IrProc {
                value: IrProcEnum::MultiAction {
                    actions: vec![(id.clone(), arg_ids)],
                },
                loc: proc.loc,
            });
            proc_id
        },
        ProcEnum::Delta => {
            add_proc(module, IrProcEnum::Delta)
        },
        ProcEnum::Tau => {
            add_proc(module, IrProcEnum::MultiAction { actions: Vec::new() })
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
            convert_binary_proc(module, BinaryProcOp::Sum, lhs, rhs)?
        },
        ProcEnum::Sum { variables, proc } => {
            // `sum x: X, y: Y . z` is syntactically a shorthand for
            // `sum x: X . sum y: Y . z`
            // we work inside out here (note the `rev()`)
            let mut current_id = convert_ir_proc(context, proc, module)?;
            for variable_decl in variables.iter().rev() {
                for (identifier, identifier_loc) in variable_decl.ids.iter().rev() {
                    let sort_id = convert_ir_sort(context, &variable_decl.sort, module)?;
                    let def_id = context.generate_def_id(module.id);
                    let next_id = add_proc(module, IrProcEnum::Sum {
                        def_id,
                        identifier: identifier.clone(),
                        identifier_loc: *identifier_loc,
                        sort: sort_id,
                        proc: current_id,
                    });
                    module.add_parent(sort_id.into(), next_id.into());
                    module.add_parent(current_id.into(), next_id.into());
                    module.add_def_source(def_id, next_id.into());
                    current_id = next_id;
                }
            }
            current_id
        },
        ProcEnum::Parallel { lhs, rhs } => {
            convert_binary_proc(module, BinaryProcOp::Parallel, lhs, rhs)?
        },
        ProcEnum::RightParallel { lhs, rhs } => {
            convert_binary_proc(module, BinaryProcOp::RightParallel, lhs, rhs)?
        },
        ProcEnum::Multi { lhs, rhs } => {
            // in the AST `|` is a binary operator, but in the IR we want it to
            // be a single vec, so we recursively collect all actions here
            let mut actions = Vec::new();
            extract_actions(context, lhs, module, &mut actions)?;
            extract_actions(context, rhs, module, &mut actions)?;

            add_proc(module, IrProcEnum::MultiAction { actions })
        },
        ProcEnum::IfThenElse { condition, then_proc, else_proc } => {
            let c = convert_ir_expr(context, condition, module)?;
            let t = convert_ir_proc(context, then_proc, module)?;
            let e = if let Some(else_proc) = else_proc {
                convert_ir_proc(context, else_proc, module)?
            } else {
                // `a -> b` is equivalent to `a -> b <> delta`
                let else_id = context.generate_proc_id(module.id);
                module.procs.insert(else_id, IrProc {
                    value: IrProcEnum::Delta,
                    loc: proc.loc,
                });
                else_id
            };

            add_proc(module, IrProcEnum::IfThenElse {
                condition: c,
                then_proc: t,
                else_proc: e,
            })
        },
        ProcEnum::LeftShift { lhs, rhs } => {
            convert_binary_proc(module, BinaryProcOp::LeftShift, lhs, rhs)?
        },
        ProcEnum::Concat { lhs, rhs } => {
            convert_binary_proc(module, BinaryProcOp::Concat, lhs, rhs)?
        },
        ProcEnum::Time { .. } => {
            unimplemented!()
        },
    })
}

/// Adds all actions within a multi-action, or returns an error if the
/// multi-action operator `|` is used on another (syntactic) construct than an
/// action.
fn extract_actions(
    context: &AnalysisContext,
    proc: &Arc<Proc>,
    result: &mut IrModule,
    output: &mut Vec<(Identifier, Vec<ExprId>)>,
) -> Result<(), ()> {
    match &proc.value {
        ProcEnum::Action { id, args } => {
            let mut arg_ids = Vec::new();
            for arg in args {
                arg_ids.push(convert_ir_expr(context, arg, result)?);
            }
            output.push((id.clone(), arg_ids));
        },
        ProcEnum::Multi { lhs, rhs } => {
            extract_actions(context, lhs, result, output)?;
            extract_actions(context, rhs, result, output)?;
        },
        _ => {
            context.error();
            // return Err(SemanticError::NodeKindError {
            //     message: "The multi-action operator | can only be used between (multi-)actions".to_owned(),
            // });
            return Err(());
        },
    }

    Ok(())
}
