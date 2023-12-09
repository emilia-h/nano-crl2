
use crate::ast::node::AstNode;
use crate::core::syntax::{Identifier, SourceLocation};

use std::rc::{Rc, Weak};

pub struct Proc {
    pub value: ProcEnum,
    pub loc: SourceLocation,
    pub parent: Weak<dyn AstNode>,
}

pub enum ProcEnum {

}

pub struct Action {
    id: Identifier,
}
