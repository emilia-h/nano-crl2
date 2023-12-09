//! A declaration (also called spec/specification in the mCRL2 spec) defines
//! somethiing in a model, often with a name (an identifier).
//! 
//! Examples are action declarations written as `act name: Sort;` or process
//! declarations written as "proc Name(param: Sort) = process;"
//! 
//! # See also
//! 
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/mcrl2.html#specification-syntax).

use crate::ast::expr::Expr;
use crate::ast::node::AstNode;
use crate::ast::sort::Sort;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Formatter};
use std::rc::{Rc, Weak};

/// Describes a declaration in an mCRL2 model.
pub struct Decl {
    pub value: DeclEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl Decl {
    /// Creates a new declaration with `parent` set to `None`.
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

/// Contains the options for a declaration.
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
        // TODO
    },
    GlobalVariableDecl {
        // TODO
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

/// Describes a variable declaration.
/// 
/// Note that a variable declaration is not a top-level declaration like
/// [`Decl`](./struct.Decl). It is instead used to specify parameters for
/// equations for `map`s.
pub struct VariableDecl {
    pub id: Identifier,
}
