
use crate::ir::module::IrModule;
use crate::ir::sort::{IrSort, IrSortEnum};
use crate::util::interning::Interned;

use std::cell::RefCell;
use std::collections::hash_map::HashMap;
use std::hash::Hash;
use std::rc::Rc;

pub struct TranslationUnit {
    modules: Vec<Rc<RefCell<IrModule>>>,
    sorts: SortContext,
}

impl TranslationUnit {
    pub fn new_rc() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(TranslationUnit {
            modules: Vec::new(),
            sorts: SortContext::new(),
        }))
    }

    pub fn get_sorts(&mut self) -> &mut SortContext {
        &mut self.sorts
    }

    pub(super) fn push_module(&mut self, module: Rc<RefCell<IrModule>>) {
        self.modules.push(module);
    }
}

pub struct SortContext {
    unit_sort: IrSort,
    bool_sort: IrSort,
    pos_sort: IrSort,
    nat_sort: IrSort,
    int_sort: IrSort,
    real_sort: IrSort,
    list_sorts: HashMap<IrSort, IrSort>,
    set_sorts: HashMap<IrSort, IrSort>,
    bag_sorts: HashMap<IrSort, IrSort>,
    fset_sorts: HashMap<IrSort, IrSort>,
    fbag_sorts: HashMap<IrSort, IrSort>,
    // alias_sorts: ,
    // struct_sorts: ,
    carthesian_sorts: HashMap<(IrSort, IrSort), IrSort>,
    function_sorts: HashMap<(IrSort, IrSort), IrSort>,
}

impl SortContext {
    fn new() -> Self {
        SortContext {
            unit_sort: IrSort { value: Interned::new(Rc::new(IrSortEnum::Unit)) },
            bool_sort: IrSort { value: Interned::new(Rc::new(IrSortEnum::Bool)) },
            pos_sort: IrSort { value: Interned::new(Rc::new(IrSortEnum::Pos)) },
            nat_sort: IrSort { value: Interned::new(Rc::new(IrSortEnum::Nat)) },
            int_sort: IrSort { value: Interned::new(Rc::new(IrSortEnum::Int)) },
            real_sort: IrSort { value: Interned::new(Rc::new(IrSortEnum::Real)) },
            list_sorts: HashMap::new(),
            set_sorts: HashMap::new(),
            bag_sorts: HashMap::new(),
            fset_sorts: HashMap::new(),
            fbag_sorts: HashMap::new(),
            carthesian_sorts: HashMap::new(),
            function_sorts: HashMap::new(),
        }
    }

    pub fn get_unit_sort(&self) -> IrSort {
        self.unit_sort.clone()
    }

    pub fn get_bool_sort(&self) -> IrSort {
        self.bool_sort.clone()
    }

    pub fn get_pos_sort(&self) -> IrSort {
        self.pos_sort.clone()
    }

    pub fn get_nat_sort(&self) -> IrSort {
        self.nat_sort.clone()
    }

    pub fn get_int_sort(&self) -> IrSort {
        self.int_sort.clone()
    }

    pub fn get_real_sort(&self) -> IrSort {
        self.real_sort.clone()
    }

    pub fn get_list_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.list_sorts,
            subsort.clone(),
            || IrSortEnum::List { subsort: subsort.clone() },
        )
    }

    pub fn get_set_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.set_sorts,
            subsort.clone(),
            || IrSortEnum::Set { subsort: subsort.clone() },
        )
    }

    pub fn get_bag_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.bag_sorts,
            subsort.clone(),
            || IrSortEnum::Bag { subsort: subsort.clone() },
        )
    }

    pub fn get_fset_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.fset_sorts,
            subsort.clone(),
            || IrSortEnum::FSet { subsort: subsort.clone() },
        )
    }

    pub fn get_fbag_sort(&mut self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.fbag_sorts,
            subsort.clone(),
            || IrSortEnum::FBag { subsort: subsort.clone() },
        )
    }

    pub fn get_struct_sort(&mut self) -> IrSort {
        todo!()
    }

    pub fn get_carthesian_sort(&mut self, lhs: &IrSort, rhs: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.carthesian_sorts,
            (lhs.clone(), rhs.clone()),
            || IrSortEnum::Carthesian { lhs: lhs.clone(), rhs: rhs.clone() },
        )
    }

    pub fn get_function_sort(&mut self, lhs: &IrSort, rhs: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.function_sorts,
            (lhs.clone(), rhs.clone()),
            || IrSortEnum::Function { lhs: lhs.clone(), rhs: rhs.clone() },
        )
    }

    fn get_cached_sort<'a, T: Hash + Eq, F>(
        sorts: &'a mut HashMap<T, IrSort>,
        key: T,
        value: F,
    ) -> IrSort
    where
        F: Fn() -> IrSortEnum
    {
        sorts.entry(key)
            .or_insert_with(|| IrSort { value: Interned::new(Rc::new(value())) })
            .clone()
    }
}
