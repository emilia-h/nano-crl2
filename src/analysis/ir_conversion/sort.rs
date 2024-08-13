
use crate::analysis::context::AnalysisContext;
use crate::ir::module::IrModule;
use crate::ir::sort::{GenericSortOp, IrSort, IrSortEnum, PrimitiveSort, SortId};
use crate::model::sort::{Sort, SortEnum};

use std::sync::Arc;

pub fn convert_ir_sort(
    context: &AnalysisContext,
    sort: &Arc<Sort>,
    module: &mut IrModule,
) -> Result<SortId, ()> {
    let add_sort = |module: &mut IrModule, value: IrSortEnum| {
        let sort_id = context.generate_sort_id(module.id);
        module.sorts.insert(sort_id, IrSort {
            value,
            loc: sort.loc,
        });
        sort_id
    };

    let convert_generic_sort = |
        module: &mut IrModule,
        op: GenericSortOp,
        subsort: &Arc<Sort>,
    | -> Result<SortId, ()> {
        let subsort_id = convert_ir_sort(context, subsort, module)?;
        let sort_id = add_sort(module, IrSortEnum::Generic {
            op,
            subsort: subsort_id,
        });
        Ok(sort_id)
    };

    Ok(match &sort.value {
        SortEnum::Bool => {
            add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Bool })
        },
        SortEnum::Pos => {
            add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Pos })
        },
        SortEnum::Nat => {
            add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Nat })
        },
        SortEnum::Int => {
            add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Int })
        },
        SortEnum::Real => {
            add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Real })
        },
        SortEnum::List { subsort } => {
            convert_generic_sort(module, GenericSortOp::List, subsort)?
        },
        SortEnum::Set { subsort } => {
            convert_generic_sort(module, GenericSortOp::Set, subsort)?
        },
        SortEnum::Bag { subsort } => {
            convert_generic_sort(module, GenericSortOp::Bag, subsort)?
        },
        SortEnum::FSet { subsort } => {
            convert_generic_sort(module, GenericSortOp::FSet, subsort)?
        },
        SortEnum::FBag { subsort } => {
            convert_generic_sort(module, GenericSortOp::FBag, subsort)?
        },
        SortEnum::Id { id } => {
            add_sort(module, IrSortEnum::Name {
                identifier: id.clone(),
            })
        },
        SortEnum::Struct { constructors: _ } => {
            // should be desugared into named type
            todo!()
        },
        SortEnum::Carthesian { lhs, rhs } => {
            let lhs_id = convert_ir_sort(context, lhs, module)?;
            let rhs_id = convert_ir_sort(context, rhs, module)?;
            let sort_id = add_sort(module, IrSortEnum::Carthesian {
                lhs: lhs_id,
                rhs: rhs_id,
            });
            module.add_parent(lhs_id.into(), sort_id.into());
            module.add_parent(rhs_id.into(), sort_id.into());
            sort_id
        },
        SortEnum::Function { lhs, rhs } => {
            let lhs_id = convert_ir_sort(context, lhs, module)?;
            let rhs_id = convert_ir_sort(context, rhs, module)?;
            let sort_id = add_sort(module, IrSortEnum::Function {
                lhs: lhs_id,
                rhs: rhs_id,
            });
            module.add_parent(lhs_id.into(), sort_id.into());
            module.add_parent(rhs_id.into(), sort_id.into());
            sort_id
        },
    })
}
