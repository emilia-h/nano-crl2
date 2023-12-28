//! Defines AST types for sorts, which is the type of an [`Expr`].
//! 
//! There are basic sorts such as `Bool` and `Int`, user-defined sorts (defined
//! using a [`SortDecl`]) and composed sorts such as `Set(T)` or `List(T)`.
//! 
//! [`Expr`]: ../expr/struct.Expr.html
//! [`SortDecl`]: ../decl/enum.DeclEnum.html#variant.SortDecl

use crate::ast::node::AstNode;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Display, Formatter};
use std::rc::{Rc, Weak};

use super::display::display_pretty_default;

/// A sort in an mCRL2 model, AKA a type.
pub struct Sort {
    pub value: SortEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl Sort {
    /// Creates a new sort with `parent` set to `None`.
    pub fn new(value: SortEnum, loc: SourceLocation) -> Self {
        Sort { value, loc, parent: None }
    }
}

impl Debug for Sort {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.value)?;
        Ok(())
    }
}

impl Display for Sort {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

#[derive(Debug)]
pub enum SortEnum {
    Bool,
    Pos,
    Nat,
    Int,
    Real,
    List {
        subsort: Rc<Sort>,
    },
    Set {
        subsort: Rc<Sort>,
    },
    Bag {
        subsort: Rc<Sort>,
    },
    FSet {
        subsort: Rc<Sort>,
    },
    FBag {
        subsort: Rc<Sort>,
    },
    Id {
        id: Identifier,
    },
    Struct {
        constructors: Vec<Constructor>,
    },
    Carthesian {
        lhs: Rc<Sort>,
        rhs: Rc<Sort>,
    },
    Function {
        lhs: Rc<Sort>,
        rhs: Rc<Sort>,
    },
}

/// A constructor for a structured type.
/// 
/// This is written in mCRL2 as `id(name1: Sort1, ...) ? recognizer`
#[derive(Debug)]
pub struct Constructor {
    pub id: Identifier,
    pub properties: Vec<(Option<Identifier>, Rc<Sort>)>,
    pub recognizer_function_id: Option<Identifier>,
}
