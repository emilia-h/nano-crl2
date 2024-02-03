
use crate::unwrap_pattern;
use crate::ir::decl::{IrDecl, IrDeclEnum};
use crate::ir::expr::IrRewriteRule;
use crate::ir::module::{add_module, IrModule};
use crate::ir::sort::IrSort;
use crate::ir::translation_unit::TranslationUnit;
use crate::model::decl::{Decl, DeclEnum};
use crate::model::module::Module;
use crate::model::sort::{Sort, SortEnum};

use std::cell::RefCell;
use std::rc::{Rc, Weak};

/// Converts an AST module (including its declarations) to a new IR module and
/// adds it to the translation unit.
pub fn add_module_from_ast(
    context: &Rc<RefCell<TranslationUnit>>,
    module: &Module,
) -> Rc<RefCell<IrModule>> {
    let ir_module = add_module(&context);
    for decl in &module.decls {
        match &decl.value {
            DeclEnum::EquationSet { .. } => {
                add_rewrite_rule_from_ast(&mut ir_module.borrow_mut(), decl);
            },
            _ => {
                add_decl_from_ast(&mut ir_module.borrow_mut(), decl);
            }
        };
    }

    ir_module
}

fn add_decl_from_ast(
    module: &mut IrModule,
    decl: &Decl,
) {
    let context_ptr = Weak::upgrade(&module.context).unwrap();
    let mut context = context_ptr.borrow_mut();

    match &decl.value {
        DeclEnum::Action { ids, sort } => {
            let sort = if let Some(v) = &sort {
                create_sort_from_ast(&mut context, v)
            } else {
                todo!()
                // context.get_sorts().get_unit_sort()
            };

            for id in ids {
                module.push_decl(IrDecl {
                    id: id.clone(),
                    value: IrDeclEnum::Action { sort: sort.clone() },
                });
            }
        },
        DeclEnum::Constructor { ids, sort } => {
            todo!()
        },
        DeclEnum::GlobalVariable { variables } => {
            for variable_decl in variables {
                let sort = create_sort_from_ast(&mut context, &variable_decl.sort);
                for id in &variable_decl.ids {
                    module.push_decl(IrDecl {
                        id: id.clone(),
                        value: IrDeclEnum::GlobalVariable { sort: sort.clone() },
                    });
                }
            }
        },
        DeclEnum::Map { id, sort } => {
            let sort = create_sort_from_ast(&mut context, sort);
            module.push_decl(IrDecl {
                id: id.clone(),
                value: IrDeclEnum::Map { sort },
            });
        },
        DeclEnum::Process { id, params, process } => {
            todo!()
        },
        DeclEnum::Sort { ids, value } => {
            let sort = if let Some(v) = &value {
                create_sort_from_ast(&mut context, v)
            } else {
                // context.get_sorts().get_unit_sort()
                todo!()
            };

            for id in ids {
                module.push_decl(IrDecl {
                    id: id.clone(),
                    value: IrDeclEnum::Sort { sort: sort.clone() },
                });
            }
        },
        _ => panic!(),
    }
}

// # Preconditions
// the value of `decl` must be of the `DeclEnum::Equation` variant.
fn add_rewrite_rule_from_ast(
    module: &mut IrModule,
    decl: &Decl,
) {
    let (variables, equations) = unwrap_pattern!(
        &decl.value,
        DeclEnum::EquationSet { variables, equations } => (variables, equations)
    );

    todo!();
}

fn create_sort_from_ast(
    context: &mut TranslationUnit,
    sort: &Sort,
) -> IrSort {
    match &sort.value {
        SortEnum::Bool => context.get_sorts().get_bool_sort(),
        SortEnum::Pos => context.get_sorts().get_pos_sort(),
        SortEnum::Nat => context.get_sorts().get_nat_sort(),
        SortEnum::Int => context.get_sorts().get_int_sort(),
        SortEnum::Real => context.get_sorts().get_real_sort(),
        SortEnum::List { subsort } => {
            let sort = create_sort_from_ast(context, &subsort);
            context.get_sorts().get_list_sort(&sort)
        },
        SortEnum::Set { subsort } => {
            let sort = create_sort_from_ast(context, &subsort);
            // context.get_sorts().get_set_sort(&sort)
            todo!()
        },
        SortEnum::Bag { subsort } => {
            let sort = create_sort_from_ast(context, &subsort);
            // context.get_sorts().get_bag_sort(&sort)
            todo!()
        },
        SortEnum::FSet { subsort } => {
            let sort = create_sort_from_ast(context, &subsort);
            // context.get_sorts().get_fset_sort(&sort)
            todo!()
        },
        SortEnum::FBag { subsort } => {
            let sort = create_sort_from_ast(context, &subsort);
            // context.get_sorts().get_fbag_sort(&sort)
            todo!()
        },
        SortEnum::Id { id } => {
            todo!()
        },
        SortEnum::Struct { constructors } => {
            todo!()
        },
        SortEnum::Carthesian { lhs, rhs } => {
            let l = create_sort_from_ast(context, &lhs);
            let r = create_sort_from_ast(context, &rhs);
            // context.get_sorts().get_carthesian_sort(&l, &r)
            todo!()
        },
        SortEnum::Function { lhs, rhs } => {
            let l = create_sort_from_ast(context, &lhs);
            let r = create_sort_from_ast(context, &rhs);
            // context.get_sorts().get_function_sort(&l, &r)
            todo!()
        },
    }
}
