
use crate::analysis::semantic::ir_conversion::ModuleAstMapping;
use crate::ir::module::ModuleId;
use crate::ir::proc::ProcId;
use crate::ir::sort::{IrSort, IrSortEnum, SortId};
use crate::model::module::Module;
use crate::util::hashing::HashByAddress;

use std::cell::{Cell, RefCell};
use std::collections::hash_map::HashMap;
use std::hash::Hash;
use std::rc::Rc;

// https://rustc-dev-guide.rust-lang.org/query.html
// https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html

pub struct AnalysisContext {
    pub(crate) ast_modules: Vec<(String, Module)>,
    pub(crate) ir_module_mappings: RefCell<HashMap<ModuleId, Rc<ModuleAstMapping>>>,
    pub(crate) sort_context: SortContext,
    id_counter: Cell<usize>,
}

impl AnalysisContext {
    pub fn new() -> Self {
        AnalysisContext {
            ast_modules: Vec::new(),
            ir_module_mappings: RefCell::new(HashMap::new()),
            sort_context: SortContext::new(),
            id_counter: Cell::new(0),
        }
    }

    pub fn add_ast_module(&mut self, name: String, module: Module) -> ModuleId {
        self.ast_modules.push((name, module));
        ModuleId { index: self.ast_modules.len() - 1 }
    }

    pub fn get_ast_module(&self, id: ModuleId) -> &(String, Module) {
        &self.ast_modules[id.index]
    }

    pub fn get_sorts(&self) -> &SortContext {
        &self.sort_context
    }

    pub fn generate_sort_id(&self, module: ModuleId) -> SortId {
        let value = self.id_counter.get();
        self.id_counter.set(value + 1);
        SortId { module, value }
    }

    pub fn generate_proc_id(&self, module: ModuleId) -> ProcId {
        let value = self.id_counter.get();
        self.id_counter.set(value + 1);
        ProcId { module, value }
    }
}

pub struct SortContext {
    unit_sort: IrSort,
    bool_sort: IrSort,
    pos_sort: IrSort,
    nat_sort: IrSort,
    int_sort: IrSort,
    real_sort: IrSort,
    list_sorts: RefCell<HashMap<IrSort, IrSort>>,
    set_sorts: RefCell<HashMap<IrSort, IrSort>>,
    bag_sorts: RefCell<HashMap<IrSort, IrSort>>,
    fset_sorts: RefCell<HashMap<IrSort, IrSort>>,
    fbag_sorts: RefCell<HashMap<IrSort, IrSort>>,
    // alias_sorts: ,
    // struct_sorts: ,
    carthesian_sorts: RefCell<HashMap<(IrSort, IrSort), IrSort>>,
    function_sorts: RefCell<HashMap<(IrSort, IrSort), IrSort>>,
}

impl SortContext {
    fn new() -> Self {
        SortContext {
            unit_sort: IrSort { value: HashByAddress::new(Rc::new(IrSortEnum::Unit)) },
            bool_sort: IrSort { value: HashByAddress::new(Rc::new(IrSortEnum::Bool)) },
            pos_sort: IrSort { value: HashByAddress::new(Rc::new(IrSortEnum::Pos)) },
            nat_sort: IrSort { value: HashByAddress::new(Rc::new(IrSortEnum::Nat)) },
            int_sort: IrSort { value: HashByAddress::new(Rc::new(IrSortEnum::Int)) },
            real_sort: IrSort { value: HashByAddress::new(Rc::new(IrSortEnum::Real)) },
            list_sorts: RefCell::new(HashMap::new()),
            set_sorts: RefCell::new(HashMap::new()),
            bag_sorts: RefCell::new(HashMap::new()),
            fset_sorts: RefCell::new(HashMap::new()),
            fbag_sorts: RefCell::new(HashMap::new()),
            carthesian_sorts: RefCell::new(HashMap::new()),
            function_sorts: RefCell::new(HashMap::new()),
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

    pub fn get_list_sort(&self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.list_sorts.borrow_mut(),
            subsort.clone(),
            || IrSortEnum::List { subsort: subsort.clone() },
        )
    }

    pub fn get_set_sort(&self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.set_sorts.borrow_mut(),
            subsort.clone(),
            || IrSortEnum::Set { subsort: subsort.clone() },
        )
    }

    pub fn get_bag_sort(&self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.bag_sorts.borrow_mut(),
            subsort.clone(),
            || IrSortEnum::Bag { subsort: subsort.clone() },
        )
    }

    pub fn get_fset_sort(&self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.fset_sorts.borrow_mut(),
            subsort.clone(),
            || IrSortEnum::FSet { subsort: subsort.clone() },
        )
    }

    pub fn get_fbag_sort(&self, subsort: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.fbag_sorts.borrow_mut(),
            subsort.clone(),
            || IrSortEnum::FBag { subsort: subsort.clone() },
        )
    }

    pub fn get_struct_sort(&mut self) -> IrSort {
        todo!()
    }

    pub fn get_carthesian_sort(&self, lhs: &IrSort, rhs: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.carthesian_sorts.borrow_mut(),
            (lhs.clone(), rhs.clone()),
            || IrSortEnum::Carthesian { lhs: lhs.clone(), rhs: rhs.clone() },
        )
    }

    pub fn get_function_sort(&self, lhs: &IrSort, rhs: &IrSort) -> IrSort {
        Self::get_cached_sort(
            &mut self.function_sorts.borrow_mut(),
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
            .or_insert_with(|| IrSort { value: HashByAddress::new(Rc::new(value())) })
            .clone()
    }
}
