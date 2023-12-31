//! Defines AST types for declarations (also called spec/specification in the
//! mCRL2 spec), which defines something in a model, often with a name (an
//! identifier).
//! 
//! Examples are action declarations written as `act name: Sort;` or process
//! declarations written as "proc Name(param: Sort) = process;"
//! 
//! # See also
//! 
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/mcrl2.html#specification-syntax).

use crate::ast::display::display_pretty_default;
use crate::ast::expr::Expr;
use crate::ast::node::AstNode;
use crate::ast::proc::Proc;
use crate::ast::sort::Sort;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Display, Formatter};
use std::rc::{Rc, Weak};

/// A declaration in an mCRL2 model.
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
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.value)?;
        Ok(())
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

/// The options for a declaration.
#[derive(Debug)]
pub enum DeclEnum {
    ActionDecl {
        ids: Vec<Identifier>,
        sort: Option<Rc<Sort>>,
    },
    ConstructorDecl {
        ids: Vec<Identifier>,
        sort: Rc<Sort>,
    },
    EquationSetDecl {
        variables: Vec<VariableDecl>,
        equations: Vec<EquationDecl>,
    },
    GlobalVariableDecl {
        variables: Vec<VariableDecl>,
    },
    InitialDecl {
        value: Rc<Proc>,
    },
    MapDecl {
        id: Identifier,
        sort: Rc<Sort>,
    },
    /// Represents a process declaration of the form `proc Name(params) =
    /// process;`.
    /// 
    /// Note that parameters are of the form `(id11, ..., id1M: Sort1, ...,
    /// idN1, ..., idNM: SortN)` i.e. parameters can be grouped together, hence
    /// the complicated data representation of the parameters.
    ProcessDecl {
        id: Identifier,
        params: Vec<VariableDecl>,
        process: Rc<Proc>,
    },
    SortDecl {
        // can be either "sort a1, ..., aN;" or "sort a = S;"
        ids: Vec<Identifier>,
        value: Option<Rc<Sort>>,
    },
}

/// A variable declaration.
/// 
/// Note that a variable declaration is not a top-level declaration like
/// [`Decl`](./struct.Decl). It is instead used to specify parameters for
/// equations for `map`s.
#[derive(Debug)]
pub struct VariableDecl {
    pub ids: Vec<Identifier>,
    pub sort: Rc<Sort>,
}

/// A single equation declaration of the form `[condition ->] lhs = rhs`.
#[derive(Debug)]
pub struct EquationDecl {
    pub condition: Option<Rc<Expr>>,
    pub lhs: Rc<Expr>,
    pub rhs: Rc<Expr>,    
}
