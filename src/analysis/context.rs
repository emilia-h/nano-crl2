//! Implements data structures that assist in analysis in this library.

use crate::core::lexer::Token;
use crate::ir::decl::{DeclId, DefId};
use crate::ir::expr::{ExprId, RewriteSetId};
use crate::ir::module::{IrModule, ModuleId};
use crate::ir::proc::ProcId;
use crate::ir::sort::{GenericSortOp, PrimitiveSort, ResolvedSort, SortId};
use crate::model::module::Module;
use crate::util::caching::{Interned, QueryHashMap};

use std::cell::{Cell, RefCell};
use std::collections::hash_map::HashMap;
use std::sync::Arc;

// my work is for a part based on the rustc compiler architecture:
// https://rustc-dev-guide.rust-lang.org/query.html
// https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html

/// The central data structure for analysis in this library.
/// 
/// It takes a couple of inputs, namely ASTs that are obtained from a parser.
/// 
/// Other data, such as intermediate representations (IRs), are computed using
/// queries that are cached to avoid computing the same result more than once.
pub struct AnalysisContext {
    /// The set of input modules, where each module has a name and an input
    /// string (usually this is just the contents of the file).
    model_inputs: HashMap<ModuleId, (String, String)>,

    pub(crate) ast_modules: QueryHashMap<ModuleId, Result<Arc<Module>, ()>>,
    pub(crate) ir_modules: QueryHashMap<ModuleId, Result<Arc<IrModule>, ()>>,
    pub(crate) sorts_of_expr: QueryHashMap<ExprId, Result<Interned<ResolvedSort>, ()>>,
    pub(crate) token_lists: QueryHashMap<ModuleId, Result<Arc<Vec<Token>>, ()>>,

    resolved_sort_context: ResolvedSortContext,
    module_id_counter: Cell<usize>,
    id_counter: Cell<usize>,
}

impl AnalysisContext {
    /// Constructs a new empty context, with no inputs.
    pub fn new() -> Self {
        AnalysisContext {
            model_inputs: HashMap::new(),

            ast_modules: QueryHashMap::new(),
            ir_modules: QueryHashMap::new(),
            sorts_of_expr: QueryHashMap::new(),
            token_lists: QueryHashMap::new(),

            resolved_sort_context: ResolvedSortContext::new(),
            module_id_counter: Cell::new(0),
            id_counter: Cell::new(0),
        }
    }

    /// Adds the AST of a module as a new input to the context, and returns the
    /// ID that it is given internally.
    /// 
    /// Note that multiple inputs with the same module name should in general
    /// not be added without removing them first, because this may lead to
    /// strange results if semantic analysis is performed.
    pub fn add_model_input(&mut self, module_name: String, input: String) -> ModuleId {
        let next = self.module_id_counter.get();
        self.module_id_counter.set(next + 1);
        let module_id = ModuleId { index: next };
        self.model_inputs.insert(module_id, (module_name, input));
        module_id
    }

    /// # Panics
    /// If the given module ID does not exist, this function will panic.
    pub fn get_model_input(&self, id: ModuleId) -> &(String, String) {
        self.model_inputs.get(&id).unwrap()
    }

    /// # Panics
    /// If the given module ID does not exist, this function will panic.
    pub fn remove_model_input(&mut self, id: ModuleId) {
        self.model_inputs.remove(&id).unwrap();
        // we don't have to invalidate every query hash map!
        self.ast_modules = QueryHashMap::new();
        self.ir_modules = QueryHashMap::new();
        self.sorts_of_expr = QueryHashMap::new();
        self.token_lists.invalidate(&id);
    }

    pub fn get_resolved_sort_context(&self) -> &ResolvedSortContext {
        &self.resolved_sort_context
    }

    /// Returns a declaration ID that was not returned by this function before.
    pub fn generate_decl_id(&self, module: ModuleId) -> DeclId {
        let value = self.id_counter.get();
        self.id_counter.set(value + 1);
        DeclId { module, value }
    }

    /// Returns a definition ID that was not returned by this function before.
    pub fn generate_def_id(&self, module: ModuleId) -> DefId {
        let value = self.id_counter.get();
        self.id_counter.set(value + 1);
        DefId { module, value }
    }

    pub fn generate_expr_id(&self, module: ModuleId) -> ExprId {
        let value = self.id_counter.get();
        self.id_counter.set(value + 1);
        ExprId { module, value }
    }

    /// Returns a process ID that was not returned by this function before.
    pub fn generate_proc_id(&self, module: ModuleId) -> ProcId {
        let value = self.id_counter.get();
        self.id_counter.set(value + 1);
        ProcId { module, value }
    }

    pub fn generate_rewrite_set_id(&self, module: ModuleId) -> RewriteSetId {
        let value = self.id_counter.get();
        self.id_counter.set(value + 1);
        RewriteSetId { module, value }
    }

    /// Returns a sort ID that was not returned by this function before.
    pub fn generate_sort_id(&self, module: ModuleId) -> SortId {
        let value = self.id_counter.get();
        self.id_counter.set(value + 1);
        SortId { module, value }
    }

    /// Adds an error to the error list.
    pub fn error(&self, ) {

    }
}

/// Stores the intermediate representation of sorts, which are cached/interned.
/// 
/// This allows for constant-time equality checks and avoids creating too many
/// objects that all look the same.
pub struct ResolvedSortContext {
    // TODO use an arena allocator instead of `Arc`
    // pub(crate) resolved_sort_arena: ArenaAllocator<ResolvedSort>,

    // primitive sorts
    unit_sort: Interned<ResolvedSort>,
    bool_sort: Interned<ResolvedSort>,
    pos_sort: Interned<ResolvedSort>,
    nat_sort: Interned<ResolvedSort>,
    int_sort: Interned<ResolvedSort>,
    real_sort: Interned<ResolvedSort>,

    // generic sorts
    list_sorts: RefCell<HashMap<
        Interned<ResolvedSort>,
        Interned<ResolvedSort>,
    >>,
    set_sorts: RefCell<HashMap<
        Interned<ResolvedSort>,
        Interned<ResolvedSort>,
    >>,
    bag_sorts: RefCell<HashMap<
        Interned<ResolvedSort>,
        Interned<ResolvedSort>,
    >>,
    fset_sorts: RefCell<HashMap<
        Interned<ResolvedSort>,
        Interned<ResolvedSort>,
    >>,
    fbag_sorts: RefCell<HashMap<
        Interned<ResolvedSort>,
        Interned<ResolvedSort>,
    >>,

    // binary sort operators
    carthesian_sorts: RefCell<HashMap<
        (Interned<ResolvedSort>, Interned<ResolvedSort>),
        Interned<ResolvedSort>,
    >>,
    function_sorts: RefCell<HashMap<
        (Interned<ResolvedSort>, Interned<ResolvedSort>),
        Interned<ResolvedSort>,
    >>,

    // other
    def_sorts: RefCell<HashMap<DefId, Interned<ResolvedSort>>>,
}

impl ResolvedSortContext {
    pub fn new() -> Self {
        ResolvedSortContext {
            unit_sort: Interned::new(ResolvedSort::Primitive { sort: PrimitiveSort::Unit }),
            bool_sort: Interned::new(ResolvedSort::Primitive { sort: PrimitiveSort::Bool }),
            pos_sort: Interned::new(ResolvedSort::Primitive { sort: PrimitiveSort::Pos }),
            nat_sort: Interned::new(ResolvedSort::Primitive { sort: PrimitiveSort::Nat }),
            int_sort: Interned::new(ResolvedSort::Primitive { sort: PrimitiveSort::Int }),
            real_sort: Interned::new(ResolvedSort::Primitive { sort: PrimitiveSort::Real }),
            list_sorts: RefCell::new(HashMap::new()),
            set_sorts: RefCell::new(HashMap::new()),
            bag_sorts: RefCell::new(HashMap::new()),
            fset_sorts: RefCell::new(HashMap::new()),
            fbag_sorts: RefCell::new(HashMap::new()),
            def_sorts: RefCell::new(HashMap::new()),
            carthesian_sorts: RefCell::new(HashMap::new()),
            function_sorts: RefCell::new(HashMap::new()),
        }
    }

    pub fn get_primitive_sort(&self, sort: PrimitiveSort) -> Interned<ResolvedSort> {
        match sort {
            PrimitiveSort::Unit => Interned::clone(&self.unit_sort),
            PrimitiveSort::Bool => Interned::clone(&self.bool_sort),
            PrimitiveSort::Pos => Interned::clone(&self.pos_sort),
            PrimitiveSort::Nat => Interned::clone(&self.nat_sort),
            PrimitiveSort::Int => Interned::clone(&self.int_sort),
            PrimitiveSort::Real => Interned::clone(&self.real_sort),
        }
    }

    pub fn get_generic_sort(
        &self,
        op: GenericSortOp,
        subsort: &Interned<ResolvedSort>,
    ) -> Interned<ResolvedSort> {
        let key = Interned::clone(subsort);
        let mut borrowed_map = match op {
            GenericSortOp::List => self.list_sorts.borrow_mut(),
            GenericSortOp::Set => self.set_sorts.borrow_mut(),
            GenericSortOp::FSet => self.fset_sorts.borrow_mut(),
            GenericSortOp::Bag => self.bag_sorts.borrow_mut(),
            GenericSortOp::FBag => self.fbag_sorts.borrow_mut(),
        };
        Interned::clone(borrowed_map.entry(key).or_insert_with(move || {
            Interned::new(ResolvedSort::Generic {
                op,
                subsort: Interned::clone(subsort),
            })
        }))
    }

    pub fn get_carthesian_sort(
        &self,
        lhs: &Interned<ResolvedSort>,
        rhs: &Interned<ResolvedSort>,
    ) -> Interned<ResolvedSort> {
        let key = (
            Interned::clone(lhs),
            Interned::clone(rhs),
        );
        let mut borrow = self.carthesian_sorts.borrow_mut();
        Interned::clone(borrow.entry(key).or_insert_with(move || {
            Interned::new(ResolvedSort::Carthesian {
                lhs: Interned::clone(&lhs),
                rhs: Interned::clone(&rhs),
            })
        }))
    }

    pub fn get_function_sort(
        &self,
        lhs: &Interned<ResolvedSort>,
        rhs: &Interned<ResolvedSort>,
    ) -> Interned<ResolvedSort> {
        let key = (
            Interned::clone(lhs),
            Interned::clone(rhs),
        );
        let mut borrow = self.function_sorts.borrow_mut();
        Interned::clone(borrow.entry(key).or_insert_with(move || {
            Interned::new(ResolvedSort::Function {
                lhs: Interned::clone(&lhs),
                rhs: Interned::clone(&rhs),
            })
        }))
    }

    pub fn get_def_sort(&self, def_id: DefId) -> Interned<ResolvedSort> {
        let mut borrow = self.def_sorts.borrow_mut();
        Interned::clone(borrow.entry(def_id).or_insert_with(move || {
            Interned::new(ResolvedSort::Def { id: def_id })
        }))
    }
}
