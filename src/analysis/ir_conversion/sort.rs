
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::{ModuleIrMapping, SemanticError};
use crate::core::syntax::Identifier;
use crate::ir::decl::DeclId;
use crate::ir::sort::{IrSort, SortId};
use crate::model::sort::{Sort, SortEnum};

use std::collections::hash_map::HashMap;
use std::sync::Arc;

pub fn convert_ir_sort(
    context: &AnalysisContext,
    id_map: &HashMap<Identifier, DeclId>,
    sort: &Option<Arc<Sort>>,
    result_id: SortId,
    ir_mapping: &mut ModuleIrMapping,
) -> Result<SortId, SemanticError> {
    let ir_sort = match sort {
        Some(ast_sort) => {
            create_ir_sort(context, id_map, ast_sort)?
        },
        None => context.get_sorts().get_unit_sort(),
    };

    ir_mapping.sorts.insert(result_id, ir_sort);

    Ok(result_id)
}

/// Constructs and returns the intermediate representation of a sort.
fn create_ir_sort(
    context: &AnalysisContext,
    id_map: &HashMap<Identifier, DeclId>,
    sort: &Arc<Sort>,
) -> Result<IrSort, SemanticError> {
    Ok(match &sort.value {
        SortEnum::Bool => context.get_sorts().get_bool_sort(),
        SortEnum::Pos => context.get_sorts().get_pos_sort(),
        SortEnum::Nat => context.get_sorts().get_nat_sort(),
        SortEnum::Int => context.get_sorts().get_int_sort(),
        SortEnum::Real => context.get_sorts().get_real_sort(),
        SortEnum::List { subsort } => {
            let s = create_ir_sort(context, id_map, &subsort)?;
            context.get_sorts().get_list_sort(&s)
        },
        SortEnum::Set { subsort } => {
            let s = create_ir_sort(context, id_map, &subsort)?;
            context.get_sorts().get_set_sort(&s)
        },
        SortEnum::Bag { subsort } => {
            let s = create_ir_sort(context, id_map, &subsort)?;
            context.get_sorts().get_bag_sort(&s)
        },
        SortEnum::FSet { subsort } => {
            let s = create_ir_sort(context, id_map, &subsort)?;
            context.get_sorts().get_fset_sort(&s)
        },
        SortEnum::FBag { subsort } => {
            let sort = create_ir_sort(context, id_map, &subsort)?;
            context.get_sorts().get_fbag_sort(&sort)
        },
        SortEnum::Id { id } => {
            let _ir_decl = id_map.get(&id);
            todo!()
        },
        SortEnum::Struct { constructors: _ } => {
            todo!()
        },
        SortEnum::Carthesian { lhs, rhs } => {
            let l = create_ir_sort(context, id_map, &lhs)?;
            let r = create_ir_sort(context, id_map, &rhs)?;
            context.get_sorts().get_carthesian_sort(&l, &r)
        },
        SortEnum::Function { lhs, rhs } => {
            let l = create_ir_sort(context, id_map, &lhs)?;
            let r = create_ir_sort(context, id_map, &rhs)?;
            context.get_sorts().get_function_sort(&l, &r)
        },
    })
}
