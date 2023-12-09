//! This module contains definitions for mCRL2 models.
//! 
//! # See also
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/mcrl2.html).

use crate::ast::decl::Decl;
use crate::ast::expr::Expr;

use std::rc::Rc;

/// An mCRL2 model, which consists of a set of declarations along with an
/// initial process that may instantiate these declarations.
pub struct Model {
    pub decls: Vec<Rc<Decl>>,
    pub initial: Option<Rc<Expr>>,
}
