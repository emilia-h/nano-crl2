//! Defines AST types for data expressions.
//! 
//! # See also
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/data.html).

use crate::ast::decl::VariableDecl;
use crate::ast::display::display_pretty_default;
use crate::ast::node::AstNode;
use crate::ast::sort::Sort;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Display, Formatter};
use std::rc::{Rc, Weak};

/// A data expression in the mCRL2 model.
pub struct Expr {
    pub value: ExprEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl Expr {
    /// Creates a new expression with `parent` set to `None`.
    pub fn new(value: ExprEnum, loc: SourceLocation) -> Self {
        Expr { value, loc, parent: None }
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.value)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

/// The options for an expression.
#[derive(Debug)]
pub enum ExprEnum {
    Id {
        id: Identifier,
    },
    Number {
        value: u64,
    },
    Bool {
        value: bool,
    },
    List {
        values: Vec<Rc<Expr>>,
    },
    Set {
        values: Vec<Rc<Expr>>,
    },
    Bag {
        values: Vec<(Rc<Expr>, Rc<Expr>)>,
    },
    SetComprehension {
        id: Identifier,
        sort: Rc<Sort>,
        expr: Rc<Expr>,
    },
    FunctionUpdate {
        function: Rc<Expr>,
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Apply {
        callee: Rc<Expr>,
        args: Vec<Rc<Expr>>,
    },
    LogicalNot {
        value: Rc<Expr>,
    },
    Negate {
        value: Rc<Expr>,
    },
    Count {
        value: Rc<Expr>,
    },
    Forall {
        variables: Vec<VariableDecl>,
        expr: Rc<Expr>,
    },
    Exists {
        variables: Vec<VariableDecl>,
        expr: Rc<Expr>,
    },
    Lambda {
        variables: Vec<VariableDecl>,
        expr: Rc<Expr>,
    },
    Implies {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    LogicalOr {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    LogicalAnd {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Equals {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    NotEquals {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    LessThan {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    LessThanEquals {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    GreaterThan {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    GreaterThanEquals {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    In {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Cons {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Snoc {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Concat {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Add {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Subtract {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Divide {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    IntegerDivide {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Mod {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Multiply {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Index {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Where {
        expr: Rc<Expr>,
        assignments: Vec<(Identifier, Rc<Expr>)>,
    },
}
