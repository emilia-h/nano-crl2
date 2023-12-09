
use crate::ast::node::AstNode;
use crate::core::syntax::SourceLocation;

use std::rc::{Rc, Weak};

pub struct Proc {
    pub value: ProcEnum,
    pub loc: SourceLocation,
    pub parent: Weak<dyn AstNode>,
}

pub enum ProcEnum {

}
