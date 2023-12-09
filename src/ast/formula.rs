
use crate::ast::expr::Expr;
use crate::ast::node::AstNode;
use crate::ast::proc::Action;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Formatter};
use std::rc::{Rc, Weak};

pub struct StateFormula {
    pub value: StateFormulaEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl StateFormula {
    pub fn new(value: StateFormulaEnum, loc: SourceLocation) -> Self {
        StateFormula { value, loc, parent: None }
    }
}

impl Debug for StateFormula {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "{:?}", self.value)?;
        Ok(())
    }
}

#[derive(Debug)]
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
        id: Identifier,
        // TODO data
        formula: Rc<StateFormula>,
    },
    Nu {
        id: Identifier,
        // TODO data
        formula: Rc<StateFormula>,
    },
    Forall {
        ids: Vec<(Identifier, Rc<StateFormula>)>,
        formula: Rc<StateFormula>,
    },
    Exists {
        ids: Vec<(Identifier, Rc<StateFormula>)>,
        formula: Rc<StateFormula>,
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
        action: Action,
        // TODO regular_formula: Rc<RegularFormula>,
        state_formula: Rc<StateFormula>,
    },
    Diamond {
        action: Action,
        // TODO regular_formula: Rc<RegularFormula>,
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

impl RegularFormula {
    pub fn new(value: RegularFormulaEnum, loc: SourceLocation) -> Self {
        RegularFormula { value, loc, parent: None }
    }
}

impl Debug for RegularFormula {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "{:?}", self.value)?;
        Ok(())
    }
}

#[derive(Debug)]
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

impl ActionFormula {
    pub fn new(value: ActionFormulaEnum, loc: SourceLocation) -> Self {
        ActionFormula { value, loc, parent: None }
    }
}

impl Debug for ActionFormula {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "{:?}", self.value)?;
        Ok(())
    }
}

#[derive(Debug)]
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
