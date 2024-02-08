
use crate::ir::decl::IrDecl;
use crate::ir::expr::IrRewriteRule;

/// Essentially a single file of mCRL2 code.
/// 
/// It can optionally contain an "initial" process, which is where the model
/// begins execution. An IR module belongs to a [`TranslationUnit`], which it
/// holds a weak pointer to.
/// 
/// [`TranslationUnit`]: ../../translation_unit/struct.TranslationUnit.html
pub struct IrModule {
    pub name: String,
    pub decl_ids: Vec<IrDecl>,
    pub rewrite_rules: Vec<IrRewriteRule>,
}

impl IrModule {
    /// Creates a new IR module that contains the specified declaration IDs.
    pub fn new(name: String, decl_ids: Vec<IrDecl>) -> Self {
        IrModule {
            name,
            decl_ids,
            rewrite_rules: Vec::new(),
        }
    }
}

/// A (nameless) identifier that, within a given analysis context, refers to a
/// specific module.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ModuleId {
    pub(crate) index: usize,
}
