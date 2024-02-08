
use crate::unwrap_pattern;
use crate::analysis::context::AnalysisContext;
use crate::core::syntax::Identifier;
use crate::ir::decl::{DeclId, IrDecl, IrDeclEnum};
use crate::ir::expr::IrRewriteRule;
use crate::ir::module::ModuleId;
use crate::ir::proc::ProcId;
use crate::ir::sort::{IrSort, SortId};
use crate::model::decl::{Decl, DeclEnum};
use crate::model::proc::Proc;
use crate::model::sort::{Sort, SortEnum};

use std::collections::hash_map::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum SemanticError {
    IdentifierError(String, Identifier),
    TypeError(String),
}

pub struct ModuleAstMapping {
    pub id_map: HashMap<Identifier, Rc<IrDecl>>,
    pub ast_proc_map: HashMap<ProcId, Rc<Proc>>,
    pub ast_sort_map: HashMap<SortId, Rc<Sort>>,
}

/// Given an AST module, return the  to a new IR module.
pub fn query_module_ast_mapping(
    context: &AnalysisContext,
    module: ModuleId,
) -> Rc<ModuleAstMapping> {
    if let Some(result) = context.ir_module_mappings.borrow().get(&module) {
        return Rc::clone(result);
    }

    let (_name, ast_module) = context.get_ast_module(module);
    // let name = name.clone();
    let decls = ast_module.decls.iter().map(|x| Rc::clone(x)).collect::<Vec<_>>();

    let mut id_map = HashMap::new();
    let mut ast_proc_map = HashMap::new();
    let mut ast_sort_map = HashMap::new();
    let mut ir_rewrite_rules = Vec::new();
    for decl in &decls {
        match &decl.value {
            DeclEnum::EquationSet { .. } => {
                create_ir_rewrite_rule(
                    context,
                    decl,
                    &mut ir_rewrite_rules,
                ).unwrap();
            },
            _ => {
                create_ir_decl(
                    context, module,
                    decl,
                    &mut id_map, &mut ast_proc_map, &mut ast_sort_map,
                ).unwrap();
            },
        };
    }

    let result = Rc::new(ModuleAstMapping {
        id_map,
        ast_proc_map,
        ast_sort_map,
    });
    context.ir_module_mappings.borrow_mut().insert(module, Rc::clone(&result));
    result
}

fn create_ir_decl(
    context: &AnalysisContext,
    module: ModuleId,
    decl: &Rc<Decl>,
    id_map: &mut HashMap<Identifier, Rc<IrDecl>>,
    ast_proc_map: &mut HashMap<ProcId, Rc<Proc>>,
    ast_sort_map: &mut HashMap<SortId, Rc<Sort>>,
) -> Result<(), SemanticError> {
    match &decl.value {
        DeclEnum::Action { ids, sort } => {
            let sort_id = if let Some(v) = sort {
                let fresh = context.generate_sort_id(module);
                ast_sort_map.insert(fresh, Rc::clone(&v));
                fresh
            } else {
                todo!()
            };

            for id in ids {
                id_map.insert(id.clone(), Rc::new(IrDecl {
                    id: id.clone(),
                    value: IrDeclEnum::Action { sort: sort_id },
                }));
            }
        },
        DeclEnum::Constructor { ids: _, sort: _ } => {
            todo!();
        },
        DeclEnum::GlobalVariable { variables } => {
            for variable_decl in variables {
                let sort_id = context.generate_sort_id(module);
                ast_sort_map.insert(sort_id, Rc::clone(&variable_decl.sort));

                for id in &variable_decl.ids {
                    id_map.insert(id.clone(), Rc::new(IrDecl {
                        id: id.clone(),
                        value: IrDeclEnum::GlobalVariable { sort: sort_id },
                    }));
                }
            }
        },
        DeclEnum::Map { id, sort } => {
            let sort_id = context.generate_sort_id(module);
            ast_sort_map.insert(sort_id, Rc::clone(&sort));

            id_map.insert(id.clone(), Rc::new(IrDecl {
                id: id.clone(),
                value: IrDeclEnum::Map { sort: sort_id },
            }));
        },
        DeclEnum::Process { id, params, process } => {
            let proc_id = context.generate_proc_id(module);
            // ast_proc_map.insert(proc_id, (Rc::clone(&params), Rc::clone(&process)));

            id_map.insert(id.clone(), Rc::new(IrDecl {
                id: id.clone(),
                value: IrDeclEnum::Process {  },
            }));

            todo!();
        },
        DeclEnum::Sort { ids, value } => {
            let sort_id = if let Some(v) = &value {
                let fresh = context.generate_sort_id(module);
                ast_sort_map.insert(fresh, Rc::clone(&v));
                fresh
            } else {
                todo!()
            };

            for id in ids {
                id_map.insert(id.clone(), Rc::new(IrDecl {
                    id: id.clone(),
                    value: IrDeclEnum::Sort { sort: sort_id },
                }));
            }
        },
        DeclEnum::EquationSet { .. } | DeclEnum::Initial { .. } => panic!(),
    }

    Ok(())
}

/// Constructs the rewrite rule that corresponds to a given equation set
/// declaration.
/// 
/// # Preconditions
/// `decl` must a `DeclEnum::Equation` variant.
fn create_ir_rewrite_rule(
    _context: &AnalysisContext,
    decl: &Rc<Decl>,
    _output: &mut Vec<IrRewriteRule>,
) -> Result<(), SemanticError> {
    let (_variables, _equations) = unwrap_pattern!(
        &decl.value,
        DeclEnum::EquationSet { variables, equations } => (variables, equations)
    );

    todo!();
}

pub fn query_ir_proc(
    _context: &AnalysisContext,
    _proc: &Rc<Proc>,
) -> Result<(), SemanticError> {
    todo!();
}

/// Returns (and constructs if necessary) the intermediate representation of a
/// sort.
pub fn query_ir_sort(
    context: &AnalysisContext,
    sort: SortId,
) -> IrSort {
    let mapping = query_module_ast_mapping(context, sort.module);
    let ast_sort = mapping.ast_sort_map.get(&sort);
    match ast_sort {
        Some(s) => query_ir_sort_impl(context, &mapping, &s),
        None => context.get_sorts().get_unit_sort(),
    }
}

fn query_ir_sort_impl(
    context: &AnalysisContext,
    mapping: &Rc<ModuleAstMapping>,
    sort: &Rc<Sort>,
) -> IrSort {
    match &sort.value {
        SortEnum::Bool => context.get_sorts().get_bool_sort(),
        SortEnum::Pos => context.get_sorts().get_pos_sort(),
        SortEnum::Nat => context.get_sorts().get_nat_sort(),
        SortEnum::Int => context.get_sorts().get_int_sort(),
        SortEnum::Real => context.get_sorts().get_real_sort(),
        SortEnum::List { subsort } => {
            let s = query_ir_sort_impl(context, mapping, &subsort);
            context.get_sorts().get_list_sort(&s)
        },
        SortEnum::Set { subsort } => {
            let s = query_ir_sort_impl(context, mapping, &subsort);
            context.get_sorts().get_set_sort(&s)
        },
        SortEnum::Bag { subsort } => {
            let s = query_ir_sort_impl(context, mapping, &subsort);
            context.get_sorts().get_bag_sort(&s)
        },
        SortEnum::FSet { subsort } => {
            let s = query_ir_sort_impl(context, mapping, &subsort);
            context.get_sorts().get_fset_sort(&s)
        },
        SortEnum::FBag { subsort } => {
            let sort = query_ir_sort_impl(context, mapping, &subsort);
            context.get_sorts().get_fbag_sort(&sort)
        },
        SortEnum::Id { id } => {
            let ir_decl = mapping.id_map.get(&id);
            todo!()
        },
        SortEnum::Struct { constructors } => {
            todo!()
        },
        SortEnum::Carthesian { lhs, rhs } => {
            let l = query_ir_sort_impl(context, mapping, &lhs);
            let r = query_ir_sort_impl(context, mapping, &rhs);
            context.get_sorts().get_carthesian_sort(&l, &r)
        },
        SortEnum::Function { lhs, rhs } => {
            let l = query_ir_sort_impl(context, mapping, &lhs);
            let r = query_ir_sort_impl(context, mapping, &rhs);
            context.get_sorts().get_function_sort(&l, &r)
        },
    }
}
