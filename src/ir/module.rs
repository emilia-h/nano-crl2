
use crate::ir::decl::IrDecl;
use crate::ir::expr::IrRewriteRule;
use crate::ir::translation_unit::TranslationUnit;

use std::cell::RefCell;
use std::rc::{Rc, Weak};

/// Essentially a single file of mCRL2 code.
/// 
/// It can optionally contain an "initial" process, which is where the model
/// begins execution. An IR module belongs to a [`TranslationUnit`], which it
/// holds a weak pointer to.
/// 
/// [`TranslationUnit`]: ../../translation_unit/struct.TranslationUnit.html
pub struct IrModule {
    pub context: Weak<RefCell<TranslationUnit>>,
    decls: Vec<IrDecl>,
    rewrite_rules: Vec<IrRewriteRule>,
}

impl IrModule {
    fn new_rc(context: Weak<RefCell<TranslationUnit>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(IrModule {
            context,
            decls: Vec::new(),
            rewrite_rules: Vec::new(),
        }))
    }

    pub fn push_decl(&mut self, decl: IrDecl) {
        self.decls.push(decl);
    }

    pub fn push_rewrite_rule(&mut self, rewrite_rule: IrRewriteRule) {
        self.rewrite_rules.push(rewrite_rule);
    }
}

/// Creates a new empty module and adds it to a given translation unit.
pub fn add_module(context: &Rc<RefCell<TranslationUnit>>) -> Rc<RefCell<IrModule>> {
    let ir_module = IrModule::new_rc(Rc::downgrade(context));
    context.borrow_mut().push_module(Rc::clone(&ir_module));
    ir_module
}
