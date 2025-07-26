
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::decl::add_decl_to_ir_module;
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::decl::{DeclId, IrDecl, IrDeclEnum};
use crate::ir::module::IrModule;
use crate::ir::sort::{GenericSortOp, IrSort, IrSortEnum, PrimitiveSort, SortId};
use crate::model::sort::{Constructor, Sort, SortEnum};

use std::sync::Arc;

pub fn convert_ir_sort(
    context: &AnalysisContext,
    sort: &Arc<Sort>,
    module: &mut IrModule,
) -> Result<SortId, ()> {
    let add_sort = |module: &mut IrModule, value: IrSortEnum| {
        add_sort_to_ir_module(context, module, value, sort.loc)
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

    match &sort.value {
        SortEnum::Bool => {
            Ok(add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Bool }))
        },
        SortEnum::Pos => {
            Ok(add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Pos }))
        },
        SortEnum::Nat => {
            Ok(add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Nat }))
        },
        SortEnum::Int => {
            Ok(add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Int }))
        },
        SortEnum::Real => {
            Ok(add_sort(module, IrSortEnum::Primitive { sort: PrimitiveSort::Real }))
        },
        SortEnum::List { subsort } => {
            convert_generic_sort(module, GenericSortOp::List, subsort)
        },
        SortEnum::Set { subsort } => {
            convert_generic_sort(module, GenericSortOp::Set, subsort)
        },
        SortEnum::Bag { subsort } => {
            convert_generic_sort(module, GenericSortOp::Bag, subsort)
        },
        SortEnum::FSet { subsort } => {
            convert_generic_sort(module, GenericSortOp::FSet, subsort)
        },
        SortEnum::FBag { subsort } => {
            convert_generic_sort(module, GenericSortOp::FBag, subsort)
        },
        SortEnum::Id { id } => {
            Ok(add_sort(module, IrSortEnum::Name {
                identifier: id.clone(),
            }))
        },
        SortEnum::Struct { constructors } => {
            let generated = Identifier::new_from_owned(
                format!("__Struct_{}", context.generate_name_id(module.id))
            );
            let _ = desugar_struct_sort(
                context,
                generated.clone(), SourceRange::EMPTY,
                constructors, sort.loc,
                module,
            );
            Ok(add_sort_to_ir_module(
                context, module,
                IrSortEnum::Name { identifier: generated }, sort.loc,
            ))
        },
        SortEnum::Carthesian { lhs, rhs } => {
            let error = format!(
                "`#` sort `{} # {}` is only allowed on the left of a `->` sort or in an `act` declaration",
                lhs,
                rhs,
            );
            return context.error(module.id, sort.loc, error);
        },
        SortEnum::Function { lhs, rhs } => {
            let mut lhs_ids = Vec::new();
            decompose_carthesian_sort(context, lhs, module, &mut lhs_ids)?;
            let rhs_id = convert_ir_sort(context, rhs, module)?;
            let sort_id = context.generate_sort_id(module.id);
            for &lhs_id in &lhs_ids {
                module.add_parent(lhs_id.into(), sort_id.into());
            }
            module.sorts.insert(sort_id, IrSort {
                value: IrSortEnum::Function {
                    lhs: lhs_ids,
                    rhs: rhs_id,
                },
                loc: sort.loc,
            });
            module.add_parent(rhs_id.into(), sort_id.into());
            Ok(sort_id)
        },
    }
}

/// Adds a struct sort with the given constructors and identifier to the
/// module.
pub(crate) fn desugar_struct_sort(
    context: &AnalysisContext,
    identifier: Identifier,
    identifier_loc: SourceRange,
    constructors: &Vec<Constructor>,
    loc: SourceRange,
    module: &mut IrModule,
) -> Result<DeclId, ()> {
    // desugar into named structured type
    // https://www.mcrl2.org/web/user_manual/language_reference/data.html#structured-sorts
    let sort_decl_id = add_decl_to_ir_module(
        context, module,
        &identifier, identifier_loc,
        IrDeclEnum::Sort,
        loc,
    );
    module.add_parent(sort_decl_id.into(), module.id.into());

    let name = |module: &mut IrModule| add_sort_to_ir_module(
        context, module,
        IrSortEnum::Name { identifier: identifier.clone() },
        loc,
    );

    for constructor in constructors {
        let struct_sort_id = name(module);
        let params = constructor.properties
            .iter()
            .map(|(_, sort)| {
                convert_ir_sort(context, sort, module)
            })
            .collect::<Result<Vec<SortId>, ()>>()?;

        let def_id = context.generate_def_id(module.id);
        let cons_decl_id = context.generate_decl_id(module.id);
        for &param in &params {
            module.add_parent(param.into(), cons_decl_id.into());
        }
        module.decls.insert(cons_decl_id, IrDecl {
            def_id,
            identifier: constructor.id.clone(),
            identifier_loc: constructor.id_loc,
            value: IrDeclEnum::Constructor {
                params,
                sort: struct_sort_id,
            },
            loc: constructor.loc,
        });
        module.add_def_source(def_id, cons_decl_id.into());
        module.add_parent(struct_sort_id.into(), cons_decl_id.into());
        module.add_parent(cons_decl_id.into(), module.id.into());

        // TODO
        // for (optional_identifier, property_sort) in &constructor.properties {
        //     if let Some(property_identifier) = optional_identifier {
        //         todo!()
        //     }
        // }
        // if let Some((recognizer, recognizer_loc)) = &constructor.recognizer_id {
        //     todo!()
        // }
    }

    Ok(sort_decl_id)
}

fn add_sort_to_ir_module(
    context: &AnalysisContext,
    module: &mut IrModule,
    value: IrSortEnum,
    loc: SourceRange,
) -> SortId {
    let sort_id = context.generate_sort_id(module.id);
    module.sorts.insert(sort_id, IrSort { value, loc });
    sort_id
}

pub fn decompose_carthesian_sort(
    context: &AnalysisContext,
    sort: &Arc<Sort>,
    module: &mut IrModule,
    result: &mut Vec<SortId>,
) -> Result<(), ()> {
    match &sort.value {
        SortEnum::Carthesian { lhs, rhs } => {
            decompose_carthesian_sort(context, lhs, module, result)?;
            decompose_carthesian_sort(context, rhs, module, result)?;
        },
        _ => result.push(convert_ir_sort(context, sort, module)?),
    }

    Ok(())
}
