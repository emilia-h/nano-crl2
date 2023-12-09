
use crate::ast::expr::Expr;
use crate::ast::node::AstNode;
use crate::ast::proc::Action;
use crate::core::syntax::{Identifier, SourceLocation};

use std::rc::{Rc, Weak};

pub struct StateFormula {
    pub value: StateFormulaEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

pub enum StateFormulaEnum {
    True,
    False,
    Id {
        id: Identifier,
    },
    Delay {
        expr: Rc<Expr>,
    },
    Yaled {
        expr: Rc<Expr>,
    },
    Mu {
        // TODO
    },
    Nu {
        // TODO
    },
    Forall {
        ids: Vec<(Identifier, Rc<StateFormula>)>,
        expr: Rc<Expr>,
    },
    Exists {
        ids: Vec<(Identifier, Rc<StateFormula>)>,
        expr: Rc<Expr>,
    },
    Implies {
        lhs: Rc<StateFormula>,
        rhs: Rc<StateFormula>,
    },
    Or {
        lhs: Rc<StateFormula>,
        rhs: Rc<StateFormula>,
    },
    And {
        lhs: Rc<StateFormula>,
        rhs: Rc<StateFormula>,
    },
    Box {
        regular_formula: Rc<RegularFormula>,
        state_formula: Rc<StateFormula>,
    },
    Diamond {
        regular_formula: Rc<RegularFormula>,
        state_formula: Rc<StateFormula>,
    },
    Not {
        value: Rc<StateFormula>,
    },
}

pub struct RegularFormula {
    pub value: RegularFormulaEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

pub enum RegularFormulaEnum {
    ActionFormula {
        value: Rc<ActionFormula>,
    },
    Add {
        lhs: Rc<RegularFormula>,
        rhs: Rc<RegularFormula>,
    },
    Concat {
        lhs: Rc<RegularFormula>,
        rhs: Rc<RegularFormula>,
    },
    Star {
        value: Rc<RegularFormula>,
    },
    Plus {
        value: Rc<RegularFormula>,
    },
}

pub struct ActionFormula {
    pub value: ActionFormulaEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

pub enum ActionFormulaEnum {
    Val {
        value: Rc<Expr>,
    },
    MultiAction {
        values: Vec<Action>,
    },
    True,
    False,
    //Forall {},
    //Exists {},
    Implies {
        lhs: Rc<ActionFormula>,
        rhs: Rc<ActionFormula>,
    },
    Or {
        lhs: Rc<ActionFormula>,
        rhs: Rc<ActionFormula>,
    },
    And {
        lhs: Rc<ActionFormula>,
        rhs: Rc<ActionFormula>,
    },
    Not {
        value: Rc<ActionFormula>,
    },
    Time {
        action_formula: Rc<ActionFormula>,
        time: Rc<Expr>,
    },
}
