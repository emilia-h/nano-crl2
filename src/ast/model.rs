
use crate::ast::decl::Decl;
use crate::ast::expr::Expr;

use std::rc::Rc;

pub struct Model {
    pub decls: Vec<Rc<Decl>>,
    pub initial: Option<Rc<Expr>>,
}
