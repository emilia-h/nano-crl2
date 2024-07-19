
use crate::ir::decl::{DeclId, IrDecl};
use crate::ir::expr::{ExprId, IrExpr, IrRewriteRule};
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
    pub decls: HashMap<DeclId, IrDecl>,
    pub exprs: HashMap<ExprId, IrExpr>,
    pub procs: HashMap<ProcId, IrProc>,
    pub sorts: HashMap<SortId, IrSort>,
    pub rewrite_rules: Vec<IrRewriteRule>,
    pub initial: Option<ProcId>,
}

impl IrModule {
    pub fn new(id: ModuleId) -> Self {
        IrModule {
            id,
            decls: HashMap::new(),
            exprs: HashMap::new(),
            procs: HashMap::new(),
            sorts: HashMap::new(),
            rewrite_rules: Vec::new(),
            initial: None,
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
