
use crate::ast::expr::Expr;
use crate::ast::node::AstNode;
use crate::ast::sort::Sort;
use crate::core::syntax::{Identifier, SourceLocation};

use std::rc::{Rc, Weak};

pub struct Decl {
    pub value: DeclEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl Decl {
    pub fn new(value: DeclEnum, loc: SourceLocation) -> Self {
        Decl { value, loc, parent: None }
    }
}

#[derive(Debug)]
pub enum DeclEnum {
    ActionDecl {
        identifiers: Vec<Identifier>,
        sort: Rc<Sort>,
    },
    ConstructorDecl {

    },
    EquationDecl {

    },
    GlobalVariableDecl {

    },
    InitialDecl {
        value: Rc<Expr>,
    },
    MapDecl {
        identifier: Identifier,
    },
    ProcessDecl {

    },
    SortDecl {

    },
    VariableDecl {

    },
}
