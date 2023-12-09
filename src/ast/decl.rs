
use crate::ast::expr::Expr;
use crate::ast::node::AstNode;
use crate::ast::sort::Sort;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Formatter};
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

impl Debug for Decl {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "{:?}", self.value)?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum DeclEnum {
    ActionDecl {
        ids: Vec<Identifier>,
        sort: Rc<Sort>,
    },
    ConstructorDecl {
        ids: Vec<Identifier>,
        sort: Rc<Sort>,
    },
    EquationDecl {
    
    },
    GlobalVariableDecl {

    },
    InitialDecl {
        value: Rc<Expr>,
    },
    MapDecl {
        ids: Vec<Identifier>,
        sort: Rc<Sort>,
    },
    ProcessDecl {
        id: Identifier,
    },
    SortDecl {
        id: Identifier,
    },
}
pub struct VariableDecl {
    pub id: Identifier,
}
