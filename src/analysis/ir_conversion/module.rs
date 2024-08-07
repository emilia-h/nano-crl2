
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::decl::convert_ir_decl;
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::module::{IrModule, ModuleId};

use std::sync::Arc;

#[derive(Debug)]
pub enum SemanticError {
    IdentifierError {
        message: String,
        identifier: Identifier,
        // id_loc: SourceRange,
        // duplicate_loc: Option<SourceRange>,
    },
    InitialProcError {
        message: String,
        loc: SourceRange,
    },
    NodeKindError {
        message: String,
    },
    TypeError {
        message: String,
    },
}

/// Generates the intermediate representation (IR) for a module, which stores
/// the code in a form that is easier to work with.
/// 
/// If for the given module ID this IR was previously constructed, then a
/// cached version is returned. Consequently, this function will only do real
/// work the first time it is called for each module.
pub fn query_ir_module(
    context: &AnalysisContext,
    module: ModuleId,
) -> Result<Arc<IrModule>, ()> {
    if let Some(result) = context.ir_modules.get_or_lock(&module)? {
        return result;
    }

    let (_, ast_module) = context.get_ast_module(module);

    let mut result = IrModule::new(module);
    let mut error = false;
    for decl in &ast_module.decls {
        if convert_ir_decl(context, decl, &mut result).is_err() {
            error = true;
        }
    }

    // unfortunate workaround for borrow checker :/
    let decl_ids = result.decls.keys().map(Clone::clone).collect::<Vec<_>>();
    for decl_id in decl_ids {
        result.add_parent(decl_id.into(), module.into());
    }

    if error {
        context.ir_modules.unlock(&module, Err(()));
        Err(())
    } else {
        let result = Arc::new(result);
        context.ir_modules.unlock(&module, Ok(Arc::clone(&result)));
        Ok(result)
    }
}
