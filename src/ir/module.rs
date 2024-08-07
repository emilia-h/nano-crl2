
use crate::ir::decl::{DeclId, IrDecl, IrParam, ParamId};
use crate::ir::expr::{ExprId, IrExpr, IrRewriteRule, RewriteRuleId};
use crate::ir::proc::{IrProc, ProcId};
use crate::ir::sort::{IrSort, SortId};

use std::collections::hash_map::HashMap;
use std::fmt::{Debug, Formatter};

/// Essentially a single file of mCRL2 code.
/// 
/// It can optionally contain an "initial" process, which is where the model
/// begins execution. An IR module belongs to a [`TranslationUnit`], which it
/// holds a weak pointer to.
/// 
/// [`TranslationUnit`]: ../../translation_unit/struct.TranslationUnit.html
#[derive(Debug)]
pub struct IrModule {
    pub id: ModuleId,
    pub(crate) decls: HashMap<DeclId, IrDecl>,
    pub(crate) exprs: HashMap<ExprId, IrExpr>,
    pub(crate) params: HashMap<ParamId, IrParam>,
    pub(crate) procs: HashMap<ProcId, IrProc>,
    pub(crate) rewrite_rules: HashMap<RewriteRuleId, IrRewriteRule>,
    pub(crate) sorts: HashMap<SortId, IrSort>,
    pub initial: Option<ProcId>,
    parent_map: HashMap<NodeId, NodeId>,
}

impl IrModule {
    pub fn new(id: ModuleId) -> Self {
        IrModule {
            id,
            decls: HashMap::new(),
            exprs: HashMap::new(),
            params: HashMap::new(),
            procs: HashMap::new(),
            rewrite_rules: HashMap::new(),
            sorts: HashMap::new(),
            initial: None,
            parent_map: HashMap::new(),
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

    pub fn get_decl(&self, node: DeclId) -> &IrDecl {
        assert_eq!(node.get_module_id(), self.id);
        self.decls.get(&node).unwrap()
    }

    pub fn get_expr(&self, node: ExprId) -> &IrExpr {
        self.exprs.get(&node).unwrap()
    }

    pub fn get_proc(&self, node: ProcId) -> &IrProc {
        self.procs.get(&node).unwrap()
    }

    pub fn get_sort(&self, node: SortId) -> &IrSort {
        self.sorts.get(&node).unwrap()
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
    Decl(DeclId),
    Expr(ExprId),
    Module(ModuleId),
    Param(ParamId),
    Proc(ProcId),
    RewriteRule(RewriteRuleId),
    Sort(SortId),
}

impl NodeId {
    /// Returns the module that the referred node lies in.
    /// 
    /// If the node ID is a module ID, then it simply returns that.
    pub fn get_module_id(&self) -> ModuleId {
        match self {
            Self::Module(id) => *id,
            Self::Decl(id) => id.module,
            Self::Expr(id) => id.module,
            Self::Param(id) => id.module,
            Self::Proc(id) => id.module,
            Self::RewriteRule(id) => id.module,
            Self::Sort(id) => id.module,
        }
    }
}

impl Debug for NodeId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Self::Module(id) => write!(f, "{:?}", id),
            Self::Decl(id) => write!(f, "{:?}", id),
            Self::Expr(id) => write!(f, "{:?}", id),
            Self::Param(id) => write!(f, "{:?}", id),
            Self::Proc(id) => write!(f, "{:?}", id),
            Self::RewriteRule(id) => write!(f, "{:?}", id),
            Self::Sort(id) => write!(f, "{:?}", id),
        }
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

impl From<SortId> for NodeId {
    fn from(value: SortId) -> Self {
        Self::Sort(value)
    }
}
