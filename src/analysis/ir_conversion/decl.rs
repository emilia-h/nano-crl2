
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::{ModuleAstMapping, ModuleIrMapping};
use crate::analysis::ir_conversion::proc::convert_ir_proc;
use crate::analysis::ir_conversion::sort::convert_ir_sort;
use crate::ir::decl::{DeclId, IrDeclEnum};

/// Recursively converts the AST nodes that `decl` refers to to IR.
/// 
/// If `decl` contains IR node IDs (like `ProcId` or `SortId`) that were picked
/// but not actually used yet (this is done in two passes because order of
/// declarations should not matter, so we should be able to refer to names that
/// are only defined later in the file), then these will also be converted.
pub fn convert_ir_decl(
    context: &AnalysisContext,
    mapping: &ModuleAstMapping,
    decl_id: DeclId,
    ir_mapping: &mut ModuleIrMapping,
) {
    assert_eq!(mapping.module, ir_mapping.module);
    let decl = ir_mapping.decls.get(&decl_id).unwrap();

    match &decl.value {
        IrDeclEnum::Action { sort } |
        IrDeclEnum::Constructor { sort } |
        IrDeclEnum::GlobalVariable { sort } |
        IrDeclEnum::Map { sort } |
        IrDeclEnum::Sort { sort } => {
            let ast_sort = mapping.ast_sort_map.get(sort)
                .expect("sort should have been added to mapping in step 1");

            // TODO error handling
            convert_ir_sort(context, &mapping.id_map, ast_sort, *sort, ir_mapping).unwrap();
        },
        IrDeclEnum::Process { params, proc } => {
            let proc = proc.clone();

            // go from process id to original AST parameters and process, then
            // convert them and add them to the name map
            let (ast_params, ast_proc) = mapping.ast_proc_map.get(&proc)
                .expect("process should have been added to mapping in step 1");

            assert_eq!(ast_params.len(), params.len());

            let mut id_map = mapping.id_map.clone();

            // add parameters to the name map
            for (index, (id, _)) in ast_params.iter().enumerate() {
                id_map.insert(id.clone(), params[index]);
            }

            // convert parameters' sorts to IR
            for (_, sort) in ast_params {
                let ast_sort = mapping.ast_sort_map.get(sort)
                    .expect("sort should have been added to mapping in step 1");

                // TODO error handling
                convert_ir_sort(
                    context, &mapping.id_map,
                    ast_sort, *sort,
                    ir_mapping,
                ).unwrap();
            }

            // TODO error handling
            convert_ir_proc(
                context, &mut id_map,
                ast_proc, proc,
                ir_mapping,
            ).unwrap();

            // remove parameters from name map
            for (id, _) in ast_params {
                id_map.remove(&id).unwrap();
            }
        },
        IrDeclEnum::LocalVariable { .. } => {
            panic!("A local variable should not exist at the global level");
        },
    }
}
