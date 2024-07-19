
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::decl::convert_ir_decl;
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::decl::DefId;
use crate::ir::proc::ProcId;
use crate::ir::sort::SortId;
use crate::ir::module::{ModuleId, IrModule};
use crate::model::decl::DeclEnum;
use crate::model::proc::Proc;
use crate::model::sort::Sort;

use std::collections::hash_map::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub enum SemanticError {
    IdentifierError {
        message: String,
        id: Identifier,
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

/// A mapping of identifiers (both user-provided `Identifier`s and
/// auto-generated IDs) within a module.
pub struct ModuleAstMapping {
    pub module: ModuleId,

    /// Stores top-level identifiers from the code and maps them to
    /// declarations in the IR.
    pub id_map: HashMap<Identifier, DefId>,

    /// Stores IDs of process declarations in this module and maps them to its
    /// parameters in the AST and a `Proc` node in the AST.
    pub ast_proc_map: HashMap<ProcId, (Vec<(Identifier, SortId)>, Arc<Proc>)>,

    /// Stores IDs of sorts in this module and maps them to a node in the AST.
    /// 
    /// Note that if an ID is mapped to `None`, this means that the code did
    /// not explicitly denote a sort. Most likely this means that a "unit" sort
    /// is to be substituted.
    pub ast_sort_map: HashMap<SortId, Option<Arc<Sort>>>,
}

/// Given an AST module, generates and returns a structure that stores node IDs
/// and references to the AST nodes that they refer to.
/// 
/// If for the given module ID this structure was previously constructed, then
/// a cached version is returned.
pub fn query_ir_module(
    context: &AnalysisContext,
    module: ModuleId,
) -> Arc<IrModule> {
    if let Some(result) = context.ir_modules.borrow().get(&module) {
        return Arc::clone(result);
    }

    let (_, ast_module) = context.get_ast_module(module);
    let module_defs = query_module_defs(context, module);

    let mut result = IrModule::new(module);
    for decl in &ast_module.decls {
        // TODO error handling
        convert_ir_decl(context, &module_defs, decl, &mut result).unwrap();
    }

    let result = Arc::new(result);
    context.ir_modules.borrow_mut().insert(module, Arc::clone(&result));
    result
}

/// Extracts all publically visible identifiers of a module and assigns fresh
/// definition IDs for each of these identifiers.
/// 
/// Will error if an identifier is already .
pub fn query_module_defs(
    context: &AnalysisContext,
    module: ModuleId,
) -> Arc<HashMap<Identifier, DefId>> {
    if let Some(result) = context.module_defs.borrow().get(&module) {
        return Arc::clone(result);
    }

    let (_, ast_module) = context.get_ast_module(module);

    let add_def = |result: &mut HashMap<Identifier, DefId>, id: &Identifier| {
        let def_id = context.generate_def_id(module);
        if result.insert(id.clone(), def_id).is_some() {
            Err(SemanticError::IdentifierError {
                message: "Module-level identifier already used by other declaration".to_owned(),
                id: id.clone(),
            })
        } else {
            Ok(())
        }
    };

    // TODO error handling
    let mut result = HashMap::new();
    for decl in &ast_module.decls {
        match &decl.value {
            DeclEnum::Action { ids, .. } |
            DeclEnum::Constructor { ids, .. } |
            DeclEnum::Sort { ids, .. } => {
                for id in ids {
                    add_def(&mut result, id).unwrap();
                }
            },
            DeclEnum::GlobalVariable { variables } => {
                for variable_decl in variables {
                    for id in &variable_decl.ids {
                        add_def(&mut result, id).unwrap();
                    }
                }
            },
            DeclEnum::Map { id, .. } |
            DeclEnum::Process { id, .. } => {
                add_def(&mut result, id).unwrap();
            },
            DeclEnum::EquationSet { .. } => {},
            DeclEnum::Initial { .. } => {},
        }
    }

    let result = Arc::new(result);
    context.module_defs.borrow_mut().insert(module, Arc::clone(&result));
    result
}
