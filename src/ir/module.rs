
use crate::core::syntax::SourceRange;
use crate::ir::decl::{DeclId, DefId, IrDecl, IrDeclEnum, IrParam, ParamId};
use crate::ir::expr::{
    ExprId, IrExpr, IrRewriteRule, IrRewriteSet, IrRewriteVar, RewriteRuleId,
    RewriteSetId, RewriteVarId,
};
use crate::ir::proc::{ActionId, IrAction, IrProc, IrProcEnum, ProcId};
use crate::ir::sort::{IrSort, SortId};

use std::collections::hash_map::HashMap;
use std::fmt::{Debug, Formatter};

/// Essentially a single file of mCRL2 code.
/// 
/// It can optionally contain an "initial" process, which is where the model
/// begins execution.
/// 
/// This type has some functions for mutating it, but these should in principle
/// only be used during construction. After that, the struct can be considered
/// immutable.
#[derive(Debug)]
pub struct IrModule {
    pub id: ModuleId,
    pub(crate) decls: HashMap<DeclId, IrDecl>,
    pub(crate) rewrite_sets: HashMap<RewriteSetId, IrRewriteSet>,
    pub(crate) exprs: HashMap<ExprId, IrExpr>,
    pub(crate) procs: HashMap<ProcId, IrProc>,
    pub(crate) sorts: HashMap<SortId, IrSort>,
    pub initial: Option<ProcId>,
    pub loc: SourceRange,
    parent_map: HashMap<NodeId, NodeId>,
    def_source_map: HashMap<DefId, NodeId>,
}

impl IrModule {
    /// Creates an empty IR module structure with an empty initial process.
    pub fn new(id: ModuleId, loc: SourceRange) -> Self {
        IrModule {
            id,
            decls: HashMap::new(),
            rewrite_sets: HashMap::new(),
            exprs: HashMap::new(),
            procs: HashMap::new(),
            sorts: HashMap::new(),
            initial: None,
            loc,
            parent_map: HashMap::new(),
            def_source_map: HashMap::new(),
        }
    }

    /// Sets the parent node ID of a node.
    /// 
    /// This function should basically only be used while constructing the IR.
    /// 
    /// # Panics
    /// The node must not already have a different parent or this function will
    /// panic.
    /// 
    /// Additionally, the module that the given nodes are in must be this
    /// module or this function will panic.
    pub fn add_parent(&mut self, child: NodeId, parent: NodeId) {
        assert_eq!(child.get_module_id(), self.id);
        assert_eq!(parent.get_module_id(), self.id);
        if let Some(prev) = self.parent_map.insert(child, parent) {
            assert_eq!(prev, parent);
        }
    }

    /// Sets the source node ID of a definition.
    /// 
    /// This function should basically only be used while constructing the IR.
    /// 
    /// # Panics
    /// The given definition ID must not already be mapped to a different
    /// source or this function will panic.
    /// 
    /// Additionally, the module that the given IDs are in must be this
    /// module or this function will panic.
    pub fn add_def_source(&mut self, def_id: DefId, source: NodeId) {
        assert_eq!(source.get_module_id(), self.id);
        assert_eq!(def_id.module, self.id);
        if let Some(prev) = self.def_source_map.insert(def_id, source) {
            assert_eq!(prev, source);
        }
    }

    /// Returns the parent node ID of a node, or `None` if the node does not
    /// have a parent.
    /// 
    /// # Panics
    /// The module that the given node is in must be this module, or this
    /// function will panic.
    pub fn get_parent(&self, child: NodeId) -> Option<NodeId> {
        assert_eq!(child.get_module_id(), self.id);
        self.parent_map.get(&child).copied()
    }

    /// # Panics
    /// The module that the given ID is in must be this module, or this
    /// function will panic.
    /// 
    /// Additionally, an invariant of this type is that any definition ID has
    /// one source, so this function will panic if the given ID is not mapped
    /// to anything.
    pub fn get_def_source(&self, def_id: DefId) -> NodeId {
        assert_eq!(def_id.module, self.id);
        *self.def_source_map.get(&def_id).unwrap()
    }

    pub fn get_action(&self, node: ActionId) -> &IrAction {
        assert_eq!(node.get_module_id(), self.id);
        let proc = self.procs.get(&node.proc).unwrap();
        match &proc.value {
            IrProcEnum::MultiAction { actions } => {
                assert!(node.index < actions.len());
                &actions[node.index]
            },
            _ => panic!("node {:?} does not have actions", node.proc),
        }
    }

    pub fn get_decl(&self, node: DeclId) -> &IrDecl {
        assert_eq!(node.get_module_id(), self.id);
        self.decls.get(&node).unwrap()
    }

    pub fn get_expr(&self, node: ExprId) -> &IrExpr {
        assert_eq!(node.get_module_id(), self.id);
        self.exprs.get(&node).unwrap()
    }

    pub fn get_param(&self, node: ParamId) -> &IrParam {
        let decl = self.decls.get(&node.decl).unwrap();
        match &decl.value {
            IrDeclEnum::Process { params, .. } => {
                assert!(node.index < params.len());
                &params[node.index]
            },
            _ => panic!("node {:?} does not have parameters", node.decl),
        }
    }

    pub fn get_proc(&self, node: ProcId) -> &IrProc {
        assert_eq!(node.get_module_id(), self.id);
        self.procs.get(&node).unwrap()
    }

    pub fn get_rewrite_rule(&self, node: RewriteRuleId) -> &IrRewriteRule {
        let rewrite_set = self.rewrite_sets.get(&node.rewrite_set).unwrap();
        assert!(node.index < rewrite_set.rules.len());
        &rewrite_set.rules[node.index]
    }

    pub fn get_rewrite_set(&self, node: RewriteSetId) -> &IrRewriteSet {
        assert_eq!(node.get_module_id(), self.id);
        self.rewrite_sets.get(&node).unwrap()
    }

    pub fn get_rewrite_var(&self, node: RewriteVarId) -> &IrRewriteVar {
        let rewrite_set = self.rewrite_sets.get(&node.rewrite_set).unwrap();
        assert!(node.index < rewrite_set.variables.len());
        &rewrite_set.variables[node.index]
    }

    pub fn get_sort(&self, node: SortId) -> &IrSort {
        assert_eq!(node.get_module_id(), self.id);
        self.sorts.get(&node).unwrap()
    }

    pub fn get_loc(&self, node: NodeId) -> SourceRange {
        use NodeId::*;

        match node {
            Action(action_id) => self.get_action(action_id).loc,
            Decl(decl_id) => self.get_decl(decl_id).loc,
            Expr(expr_id) => self.get_expr(expr_id).loc,
            Module(module_id) => {
                assert_eq!(module_id, self.id);
                self.loc
            },
            Param(param_id) => self.get_param(param_id).loc,
            Proc(proc_id) => self.get_proc(proc_id).loc,
            RewriteRule(rewrite_rule_id) => self.get_rewrite_rule(rewrite_rule_id).loc,
            RewriteSet(rewrite_set_id) => self.get_rewrite_set(rewrite_set_id).loc,
            RewriteVar(rewrite_var_id) => self.get_rewrite_var(rewrite_var_id).loc,
            Sort(sort_id) => self.get_sort(sort_id).loc,
        }
    }
}

/// A (nameless) identifier that, within a given analysis context, refers to a
/// specific module.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ModuleId {
    pub(crate) index: usize,
}

impl Debug for ModuleId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.index)
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum NodeId {
    Action(ActionId),
    Decl(DeclId),
    Expr(ExprId),
    Module(ModuleId),
    Param(ParamId),
    Proc(ProcId),
    RewriteRule(RewriteRuleId),
    RewriteSet(RewriteSetId),
    RewriteVar(RewriteVarId),
    Sort(SortId),
}

impl NodeId {
    /// Returns the module that the referred node lies in.
    /// 
    /// If the node ID is a module ID, then it simply returns that.
    pub fn get_module_id(&self) -> ModuleId {
        match self {
            Self::Module(id) => *id,
            Self::Action(id) => id.proc.module,
            Self::Decl(id) => id.module,
            Self::Expr(id) => id.module,
            Self::Param(id) => id.decl.module,
            Self::Proc(id) => id.module,
            Self::RewriteRule(id) => id.rewrite_set.module,
            Self::RewriteSet(id) => id.module,
            Self::RewriteVar(id) => id.rewrite_set.module,
            Self::Sort(id) => id.module,
        }
    }
}

impl Debug for NodeId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Self::Action(id) => write!(f, "{:?}", id),
            Self::Decl(id) => write!(f, "{:?}", id),
            Self::Expr(id) => write!(f, "{:?}", id),
            Self::Module(id) => write!(f, "{:?}", id),
            Self::Param(id) => write!(f, "{:?}", id),
            Self::Proc(id) => write!(f, "{:?}", id),
            Self::RewriteRule(id) => write!(f, "{:?}", id),
            Self::RewriteSet(id) => write!(f, "{:?}", id),
            Self::RewriteVar(id) => write!(f, "{:?}", id),
            Self::Sort(id) => write!(f, "{:?}", id),
        }
    }
}

impl From<ActionId> for NodeId {
    fn from(value: ActionId) -> Self {
        Self::Action(value)
    }
}

impl From<DeclId> for NodeId {
    fn from(value: DeclId) -> Self {
        Self::Decl(value)
    }
}

impl From<ExprId> for NodeId {
    fn from(value: ExprId) -> Self {
        Self::Expr(value)
    }
}

impl From<ModuleId> for NodeId {
    fn from(value: ModuleId) -> Self {
        Self::Module(value)
    }
}

impl From<ParamId> for NodeId {
    fn from(value: ParamId) -> Self {
        Self::Param(value)
    }
}

impl From<ProcId> for NodeId {
    fn from(value: ProcId) -> Self {
        Self::Proc(value)
    }
}

impl From<RewriteRuleId> for NodeId {
    fn from(value: RewriteRuleId) -> Self {
        Self::RewriteRule(value)
    }
}

impl From<RewriteSetId> for NodeId {
    fn from(value: RewriteSetId) -> Self {
        Self::RewriteSet(value)
    }
}

impl From<RewriteVarId> for NodeId {
    fn from(value: RewriteVarId) -> Self {
        Self::RewriteVar(value)
    }
}

impl From<SortId> for NodeId {
    fn from(value: SortId) -> Self {
        Self::Sort(value)
    }
}
