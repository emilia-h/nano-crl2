//! ...
//! 
//! # See also
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/data.html).

use crate::ast::decl::VariableDecl;
use crate::ast::node::AstNode;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Formatter};
use std::rc::{Rc, Weak};

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
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "{:?}", self.value)?;
        Ok(())
    }
}

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
        // TODO
    },
    SetComprehension {
        // TODO
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
