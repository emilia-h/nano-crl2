
use crate::ir::module::IrModule;
use crate::ir::sort::{IrSort, IrSortDefinition, IrSortDefinitionEnum};

use std::cell::RefCell;
use std::collections::hash_map::HashMap;
use std::hash::Hash;
use std::rc::Rc;

pub struct TranslationUnit {
    modules: Vec<Rc<RefCell<IrModule>>>,
    sort_definitions: Vec<IrSortDefinition>,
    list_sorts: HashMap<usize, usize>,
    set_sorts: HashMap<usize, usize>,
    bag_sorts: HashMap<usize, usize>,
    fset_sorts: HashMap<usize, usize>,
    fbag_sorts: HashMap<usize, usize>,
    // alias_sorts: ,
    // struct_sorts: ,
    carthesian_sorts: HashMap<(usize, usize), usize>,
    function_sorts: HashMap<(usize, usize), usize>,
}

impl TranslationUnit {
    pub fn new_rc() -> Rc<RefCell<Self>> {
        let mut sort_definitions = Vec::new();
        sort_definitions.push(IrSortDefinition { value: IrSortDefinitionEnum::Unit });
        sort_definitions.push(IrSortDefinition { value: IrSortDefinitionEnum::Bool });
        sort_definitions.push(IrSortDefinition { value: IrSortDefinitionEnum::Pos });
        sort_definitions.push(IrSortDefinition { value: IrSortDefinitionEnum::Nat });
        sort_definitions.push(IrSortDefinition { value: IrSortDefinitionEnum::Int });
        sort_definitions.push(IrSortDefinition { value: IrSortDefinitionEnum::Real });

        Rc::new(RefCell::new(TranslationUnit {
            modules: Vec::new(),
            sort_definitions,
            list_sorts: HashMap::new(),
            set_sorts: HashMap::new(),
            bag_sorts: HashMap::new(),
            fset_sorts: HashMap::new(),
            fbag_sorts: HashMap::new(),
            carthesian_sorts: HashMap::new(),
            function_sorts: HashMap::new(),
        }))
    }

    pub fn get_unit_sort(&self) -> IrSort {
        IrSort { index: 0 }
    }

    pub fn get_bool_sort(&self) -> IrSort {
        IrSort { index: 1 }
    }

    pub fn get_pos_sort(&self) -> IrSort {
        IrSort { index: 2 }
    }
    
    pub fn get_nat_sort(&self) -> IrSort {
        IrSort { index: 3 }
    }

    pub fn get_int_sort(&self) -> IrSort {
        IrSort { index: 4 }
    }

    pub fn get_real_sort(&self) -> IrSort {
        IrSort { index: 5 }
    }

    pub fn get_list_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.sort_definitions,
            &mut self.list_sorts,
            subsort.index,
            IrSortDefinitionEnum::List { subsort: subsort.index },
        )
    }

    pub fn get_set_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.sort_definitions,
            &mut self.set_sorts,
            subsort.index,
            IrSortDefinitionEnum::Set { subsort: subsort.index },
        )
    }

    pub fn get_bag_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.sort_definitions,
            &mut self.bag_sorts,
            subsort.index,
            IrSortDefinitionEnum::Bag { subsort: subsort.index },
        )
    }

    pub fn get_fset_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.sort_definitions,
            &mut self.fset_sorts,
            subsort.index,
            IrSortDefinitionEnum::FSet { subsort: subsort.index },
        )
    }

    pub fn get_fbag_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.sort_definitions,
            &mut self.fbag_sorts,
            subsort.index,
            IrSortDefinitionEnum::FBag { subsort: subsort.index },
        )
    }

    pub fn get_struct_sort(&mut self) -> IrSort {
        todo!()
    }

    pub fn get_carthesian_sort(&mut self, lhs: &IrSort, rhs: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.sort_definitions,
            &mut self.carthesian_sorts,
            (lhs.index, rhs.index),
            IrSortDefinitionEnum::Carthesian { lhs: lhs.index, rhs: rhs.index },
        )
    }

    pub fn get_function_sort(&mut self, lhs: &IrSort, rhs: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.sort_definitions,
            &mut self.function_sorts,
            (lhs.index, rhs.index),
            IrSortDefinitionEnum::Function { lhs: lhs.index, rhs: rhs.index },
        )
    }

    pub(super) fn push_module(&mut self, module: Rc<RefCell<IrModule>>) {
        self.modules.push(module);
    }

    fn get_cached_sort<T: Hash + Eq>(
        sort_definitions: &mut Vec<IrSortDefinition>,
        sorts: &mut HashMap<T, usize>,
        key: T,
        value: IrSortDefinitionEnum,
    ) -> IrSort {
        if let Some(&index) = sorts.get(&key) {
            IrSort { index }
        } else {
            sort_definitions.push(IrSortDefinition { value });
            let index = sort_definitions.len() - 1;
            sorts.insert(key, index);
            IrSort { index }
        }
    }
}
