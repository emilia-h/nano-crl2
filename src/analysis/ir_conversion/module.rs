
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::decl::convert_ir_decl;
use crate::core::syntax::Identifier;
use crate::ir::decl::{DeclId, IrDecl, IrDeclEnum};
use crate::ir::expr::{ExprId, IrExpr};
use crate::ir::proc::{IrProc, ProcId};
use crate::ir::sort::{IrSort, SortId};
use crate::ir::module::{ModuleId, IrModule};
use crate::model::decl::{Decl, DeclEnum};
use crate::model::proc::Proc;
use crate::model::sort::Sort;

use std::collections::hash_map::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum SemanticError {
    IdentifierError {
        message: String,
        id: Identifier,
    },
    NodeKindError {
        message: String,
    },
    TypeError {
        message: String,
    },
}

/// Given an AST module, generates and returns a structure that stores node IDs
/// and references to the AST nodes that they refer to.
/// 
/// If for the given module ID this structure was previously constructed, then
/// a cached version is returned.
pub fn query_ir_module(
    context: &AnalysisContext,
    module: ModuleId,
) -> Rc<IrModule> {
    if let Some(result) = context.ir_modules.borrow().get(&module) {
        return Rc::clone(result);
    }

    let (name, ast_module) = context.get_ast_module(module);
    let name = name.clone();
    let decls = ast_module.decls.iter().map(|x| Rc::clone(x)).collect::<Vec<_>>();

    // step 1: collect all top-level declarations, convert those to `IrDecl`s
    // and store all necessary data in some maps
    let mut mapping = ModuleAstMapping {
        module,
        id_map: HashMap::new(),
        ast_proc_map: HashMap::new(),
        ast_sort_map: HashMap::new(),
    };
    let mut ir_decls = HashMap::new();
    for decl in &decls {
        // TODO handle errors
        add_decl_to_mapping(context, module, decl, &mut mapping, &mut ir_decls).unwrap();
    }

    // step 2: convert the data for each declaration to IR
    let mut ir_mapping = ModuleIrMapping {
        module,
        decls: ir_decls,
        exprs: HashMap::new(),
        procs: HashMap::new(),
        sorts: HashMap::new(),
    };
    for (_, &decl_id) in &mapping.id_map {
        convert_ir_decl(context, &mapping, decl_id, &mut ir_mapping);
    }

    let result = Rc::new(IrModule {
        name,
        decls: ir_mapping.decls,
        exprs: ir_mapping.exprs,
        procs: ir_mapping.procs,
        rewrite_rules: Vec::new(),
        top_level_symbols: mapping.id_map,
    });
    context.ir_modules.borrow_mut().insert(module, Rc::clone(&result));
    result
}

/// Converts an AST `Decl` to an IR decl, then adds it to the `decls` map with
/// a freshly generated ID. This does not yet convert the children of `decl` to
/// IR.
fn add_decl_to_mapping(
    context: &AnalysisContext,
    module: ModuleId,
    decl: &Rc<Decl>,
    mapping: &mut ModuleAstMapping,
    decls: &mut HashMap<DeclId, IrDecl>,
) -> Result<(), SemanticError> {
    let mut add_ir_decl = |decls: &mut HashMap<DeclId, IrDecl>, id: &Identifier, ir_decl| {
        let decl_id = context.generate_decl_id(module);
        decls.insert(decl_id, IrDecl {
            value: ir_decl,
        });
        mapping.id_map.insert(id.clone(), decl_id);
    };

    match &decl.value {
        DeclEnum::Action { ids, sort } => {
            let sort_id = context.generate_sort_id(module);
            mapping.ast_sort_map.insert(sort_id, sort.clone());

            for id in ids {
                add_ir_decl(decls, id, IrDeclEnum::Action { sort: sort_id });
            }
        },
        DeclEnum::Constructor { ids, sort } => {
            let sort_id = context.generate_sort_id(module);
            mapping.ast_sort_map.insert(sort_id, Some(Rc::clone(&sort)));

            for id in ids {
                add_ir_decl(decls, id, IrDeclEnum::Constructor { sort: sort_id });
            }
        },
        DeclEnum::GlobalVariable { variables } => {
            for variable_decl in variables {
                let sort_id = context.generate_sort_id(module);
                mapping.ast_sort_map.insert(sort_id, Some(Rc::clone(&variable_decl.sort)));

                for id in &variable_decl.ids {
                    add_ir_decl(decls, id, IrDeclEnum::GlobalVariable { sort: sort_id });
                }
            }
        },
        DeclEnum::Map { id, sort } => {
            let sort_id = context.generate_sort_id(module);
            mapping.ast_sort_map.insert(sort_id, Some(Rc::clone(&sort)));

            add_ir_decl(decls, id, IrDeclEnum::Map { sort: sort_id });
        },
        DeclEnum::Process { id, params, proc } => {
            let process_id = context.generate_proc_id(module);
            let mut param_ids = Vec::new();
            let mut param_mappings = Vec::new();
            for variable_decl in params {
                let sort_id = context.generate_sort_id(module);
                mapping.ast_sort_map.insert(sort_id, Some(Rc::clone(&variable_decl.sort)));

                for id in &variable_decl.ids {
                    let param_id = context.generate_decl_id(module);
                    decls.insert(param_id, IrDecl {
                        value: IrDeclEnum::LocalVariable { sort: sort_id },
                    });
                    param_ids.push(param_id);
                    param_mappings.push((id.clone(), sort_id));
                }
            }
            mapping.ast_proc_map.insert(process_id, (param_mappings, Rc::clone(&proc)));

            let proc = IrDeclEnum::Process { params: param_ids, proc: process_id };
            add_ir_decl(decls, id, proc);
        },
        DeclEnum::Sort { ids, sort } => {
            let sort_id = context.generate_sort_id(module);
            if let Some(s) = sort {
                mapping.ast_sort_map.insert(sort_id, Some(Rc::clone(&s)));
            } else {
                todo!("construct new structural type");
            }

            for id in ids {
                add_ir_decl(decls, id, IrDeclEnum::Sort { sort: sort_id });
            }
        },
        DeclEnum::EquationSet { .. } => {
            todo!("equations");
        },
        DeclEnum::Initial { .. } => panic!(),
    }

    Ok(())
}

/// A mapping of identifiers (both user-provided `Identifier`s and
/// auto-generated IDs) within a module.
pub struct ModuleAstMapping {
    pub module: ModuleId,

    /// Stores top-level identifiers from the code and maps them to
    /// declarations in the IR.
    pub id_map: HashMap<Identifier, DeclId>,

    /// Stores IDs of process declarations in this module and maps them to its
    /// parameters in the AST and a `Proc` node in the AST.
    pub ast_proc_map: HashMap<ProcId, (Vec<(Identifier, SortId)>, Rc<Proc>)>,

    /// Stores IDs of sorts in this module and maps them to a node in the AST.
    /// 
    /// Note that if an ID is mapped to `None`, this means that the code did
    /// not explicitly denote a sort. Most likely this means that a "unit" sort
    /// is to be substituted.
    pub ast_sort_map: HashMap<SortId, Option<Rc<Sort>>>,
}

pub struct ModuleIrMapping {
    pub module: ModuleId,
    pub decls: HashMap<DeclId, IrDecl>,
    pub exprs: HashMap<ExprId, IrExpr>,
    pub procs: HashMap<ProcId, IrProc>,
    pub sorts: HashMap<SortId, IrSort>,
}
