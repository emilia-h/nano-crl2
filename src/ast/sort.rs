
use crate::ast::node::AstNode;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Formatter};
use std::rc::{Rc, Weak};

pub struct Sort {
    pub value: SortEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl Sort {
    pub fn new(value: SortEnum, loc: SourceLocation) -> Self {
        Sort { value, loc, parent: None }
    }
}

impl Debug for Sort {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "{:?}", self.value)?;
        Ok(())
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
