//! This module contains definitions for a generic "node" trait.
//! 
//! This is sometimes necessary when a node has a pointer to a different node,
//! but does not know ahead of time if this is a `Decl`, an `Expr` etc.

use crate::ast::decl::Decl;
use crate::ast::expr::Expr;
use crate::ast::sort::Sort;

use std::rc::Rc;

pub trait AstNode {
    fn as_decl(&self) -> Option<Rc<Decl>>;
    fn as_expr(&self) -> Option<Rc<Expr>>;
    fn as_sort(&self) -> Option<Rc<Sort>>;
}
