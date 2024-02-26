
use crate::core::syntax::Identifier;
use crate::ir::decl::{DeclId, IrDecl};
use crate::ir::expr::{ExprId, IrExpr, IrRewriteRule};
use crate::ir::proc::{IrProc, ProcId};

use std::collections::hash_map::HashMap;

/// Essentially a single file of mCRL2 code.
/// 
/// It can optionally contain an "initial" process, which is where the model
/// begins execution. An IR module belongs to a [`TranslationUnit`], which it
/// holds a weak pointer to.
/// 
/// [`TranslationUnit`]: ../../translation_unit/struct.TranslationUnit.html
#[derive(Debug)]
pub struct IrModule {
    pub name: String,
    pub decls: HashMap<DeclId, IrDecl>,
    pub exprs: HashMap<ExprId, IrExpr>,
    pub procs: HashMap<ProcId, IrProc>,
    pub rewrite_rules: Vec<IrRewriteRule>,
    pub top_level_symbols: HashMap<Identifier, DeclId>,
}

/// A (nameless) identifier that, within a given analysis context, refers to a
/// specific module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ModuleId {
    pub(crate) index: usize,
}
