
use crate::ast::node::AstNode;
use crate::ast::sort::Sort;
use crate::core::syntax::{Identifier, SourceLocation};

use std::rc::{Rc, Weak};

pub struct Expr {
    pub value: ExprEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl Expr {
    pub fn new(value: ExprEnum, loc: SourceLocation) -> Self {
        Expr { value, loc, parent: None }
    }
}

pub enum ExprEnum {
    Id {
        id: Identifier,
    },
    Number {
        value: NumberValue,
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
    BoolNot {

    },
    Negate {
        value: Rc<Expr>,
    },
    Count {
        value: Rc<Expr>,
    },
    Forall {
        ids: Vec<(Identifier, Rc<Sort>)>,
        expr: Rc<Expr>,
    },
    Exists {
        ids: Vec<(Identifier, Rc<Sort>)>,
        expr: Rc<Expr>,
    },
    Lambda {
        ids: Vec<(Identifier, Rc<Sort>)>,
        expr: Rc<Expr>,
    },
    Implies {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    BoolOr {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    BoolAnd {
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

pub struct NumberValue {
    // TODO
}
