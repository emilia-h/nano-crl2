
use crate::ast::decl::Decl;
use crate::ast::expr::Expr;
use crate::ast::sort::Sort;

use std::rc::Rc;

pub trait AstNode {
    fn as_decl(&self) -> Option<Rc<Decl>>;
    fn as_expr(&self) -> Option<Rc<Expr>>;
    fn as_sort(&self) -> Option<Rc<Sort>>;
}
