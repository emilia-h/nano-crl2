
use crate::core::decl::Decl;
use crate::core::expr::Expr;

#[derive(Debug)]
pub struct Model {
    pub declarations: Vec<Decl>,
    pub initial: Option<Expr>,
}
