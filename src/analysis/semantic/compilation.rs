
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::query_ir_module;
use crate::analysis::semantic::name_resolution::{
    is_name_node,
    query_def_of_name,
};
use crate::analysis::semantic::sort_resolution::{
    query_resolved_sort, query_sort_of_def, query_sort_of_expr
};
use crate::ir::iterator::IrIterator;
use crate::ir::module::{ModuleId, NodeId};

/// Returns `Ok` if compilation is successful (i.e., the semantic analysis
/// queries of the module have no errors), or `Err` if it is not.
/// 
/// It does not stop on the first error but tries to collect as much
/// error information as it can.
/// 
/// This query is cached.
pub fn query_compilation_check(
    context: &AnalysisContext,
    module: ModuleId,
) -> Result<(), ()> {
    match context.compilation_checks.get_or_lock(&module) {
        Ok(Some(value)) => value,
        Ok(None) => {
            let result = calculate_compilation_check(context, module);
            context.compilation_checks.unlock(&module, result.clone());
            result
        },
        Err(()) => panic!("cyclic error should really not happen"),
    }
}

fn calculate_compilation_check(
    context: &AnalysisContext,
    module: ModuleId,
) -> Result<(), ()> {
    use NodeId::*;

    let ir_module = query_ir_module(context, module)?;
    let mut err = false;
    for id in IrIterator::new(&ir_module, module.into()) {
        if is_name_node(&ir_module, id) {
            let def_result = query_def_of_name(context, id);
            match def_result {
                Ok(def) => {
                    if matches!(id, Expr(_)) {
                        err = query_sort_of_def(context, def).is_err() || err;
                    }
                },
                Err(_) => err = true,
            }
        }

        match id {
            Action(_) => {},
            Decl(_) => {},
            Expr(id) => {
                err = query_sort_of_expr(context, id).is_err() || err;
            },
            Module(_) => {},
            Param(_) => {},
            Proc(_) => {},
            RewriteRule(_) => {},
            RewriteSet(_) => {},
            RewriteVar(_) => {},
            Sort(id) => {
                err = query_resolved_sort(context, id).is_err() || err;
            },
        }
    }

    if err {
        Err(())
    } else {
        Ok(())
    }
}
