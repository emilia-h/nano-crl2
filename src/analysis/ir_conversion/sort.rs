
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::SemanticError;
use crate::ir::module::IrModule;
use crate::ir::sort::{IrSort, IrSortEnum, SortId};
use crate::model::sort::{Sort, SortEnum};

use std::sync::Arc;

pub fn convert_ir_sort(
    context: &AnalysisContext,
    sort: &Arc<Sort>,
    result: &mut IrModule,
) -> Result<SortId, SemanticError> {
    let ir_sort_value = match &sort.value {
        SortEnum::Bool => IrSortEnum::Bool,
        SortEnum::Pos => IrSortEnum::Pos,
        SortEnum::Nat => IrSortEnum::Nat,
        SortEnum::Int => IrSortEnum::Int,
        SortEnum::Real => IrSortEnum::Real,
        SortEnum::List { subsort } => {
            let s = convert_ir_sort(context, subsort, result)?;
            IrSortEnum::List { subsort: s }
        },
        SortEnum::Set { subsort } => {
            let s = convert_ir_sort(context, subsort, result)?;
            IrSortEnum::Set { subsort: s }
        },
        SortEnum::Bag { subsort } => {
            let s = convert_ir_sort(context, subsort, result)?;
            IrSortEnum::Bag { subsort: s }
        },
        SortEnum::FSet { subsort } => {
            let s = convert_ir_sort(context, subsort, result)?;
            IrSortEnum::FSet { subsort: s }
        },
        SortEnum::FBag { subsort } => {
            let s = convert_ir_sort(context, subsort, result)?;
            IrSortEnum::FBag { subsort: s }
        },
        SortEnum::Id { id } => {
            IrSortEnum::Name { id: id.clone() }
        },
        SortEnum::Struct { constructors } => {
            // should be desugared into named type
            todo!()
        },
        SortEnum::Carthesian { lhs, rhs } => {
            let l = convert_ir_sort(context, lhs, result)?;
            let r = convert_ir_sort(context, rhs, result)?;
            IrSortEnum::Carthesian { lhs: l, rhs: r }
        },
        SortEnum::Function { lhs, rhs } => {
            let l = convert_ir_sort(context, lhs, result)?;
            let r = convert_ir_sort(context, rhs, result)?;
            IrSortEnum::Function { lhs: l, rhs: r }
        },
    };

    let sort_id = context.generate_sort_id(result.id);
    result.sorts.insert(sort_id, IrSort { value: ir_sort_value });
    Ok(sort_id)
}
