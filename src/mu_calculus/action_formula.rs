//! Defines AST types for mu-calculus action formulas, that are used inside the
//! modal operators `[...]` (box) and `<...>` (diamond).
//! 
//! Such a formula, given a context of a `Model`, encodes a (possibly infinite)
//! set of actions.

use crate::core::lexer::LexicalElement;
use crate::core::parser::{Parseable, ParseError, Parser};
use crate::core::syntax::SourceLocation;
use crate::model::decl::VariableDecl;
use crate::model::expr::Expr;
use crate::model::proc::{Action, parse_multi_action};
use crate::model::node::AstNode;

use std::fmt::{Debug, Formatter};
use std::rc::{Rc, Weak};

/// A formula that tests a single action, that can be used within a regular
/// formula and thus inside a box (`[...]`) or diamond (`<...>`) operator.
/// 
/// This is different from a regular formula in the sense that an action
/// formula is true or false for a single action, while a regular formula is
/// about 0 or more consecutive actions.
pub struct ActionFormula {
    pub value: ActionFormulaEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl ActionFormula {
    /// Creates a new action formula with `parent` set to `None`.
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
    Forall {
        ids: Vec<VariableDecl>,
        action_formula: Rc<ActionFormula>,
    },
    Exists {
        ids: Vec<VariableDecl>,
        action_formula: Rc<ActionFormula>,
    },
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

impl Parseable for ActionFormula {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_action_formula(parser)
    }
}

/// Parses an action formula.
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/mucalc.html#grammar-token-ActFrm
pub fn parse_action_formula(parser: &mut Parser) -> Result<ActionFormula, ParseError> {
    // forall, exists
    let loc = parser.get_loc();

    if parser.skip_if_equal(&LexicalElement::Forall) {
        let ids = parser.parse::<Vec<VariableDecl>>()?;
        parser.expect_token(&LexicalElement::Period)?;
        let action_formula = Rc::new(parser.parse::<ActionFormula>()?);
        Ok(ActionFormula::new(
            ActionFormulaEnum::Forall { ids, action_formula },
            parser.until_now(&loc),
        ))
    } else if parser.skip_if_equal(&LexicalElement::Exists) {
        let ids = parser.parse::<Vec<VariableDecl>>()?;
        parser.expect_token(&LexicalElement::Period)?;
        let action_formula = Rc::new(parser.parse::<ActionFormula>()?);
        Ok(ActionFormula::new(
            ActionFormulaEnum::Exists { ids, action_formula },
            parser.until_now(&loc),
        ))
    } else {
        parse_implies_action_formula(parser)
    }
}

fn parse_implies_action_formula(parser: &mut Parser) -> Result<ActionFormula, ParseError> {
    // => (associates to the right)
    let loc = parser.get_loc();
    let lhs = parse_or_action_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::ThickArrow) {
        let rhs = Rc::new(parser.parse::<ActionFormula>()?);
        Ok(ActionFormula::new(
            ActionFormulaEnum::Implies { lhs: Rc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_or_action_formula(parser: &mut Parser) -> Result<ActionFormula, ParseError> {
    // || (associates to the right)
    let loc = parser.get_loc();
    let lhs = parse_and_action_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::DoublePipe) {
        let rhs = Rc::new(parse_or_action_formula(parser)?);
        Ok(ActionFormula::new(
            ActionFormulaEnum::Or { lhs: Rc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_and_action_formula(parser: &mut Parser) -> Result<ActionFormula, ParseError> {
    // && (associates to the right)
    let loc = parser.get_loc();
    let lhs = parse_time_action_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::DoubleAmpersand) {
        let rhs = Rc::new(parse_and_action_formula(parser)?);
        Ok(ActionFormula::new(
            ActionFormulaEnum::And { lhs: Rc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_time_action_formula(parser: &mut Parser) -> Result<ActionFormula, ParseError> {
    // @ (associates to the left)
    let loc = parser.get_loc();
    let mut result = parse_basic_action_formula(parser)?;

    while parser.skip_if_equal(&LexicalElement::AtSign) {
        let time = Rc::new(parser.parse::<Expr>()?);
        result = ActionFormula::new(
            ActionFormulaEnum::Time {
                action_formula: Rc::new(result),
                time,
            },
            parser.until_now(&loc),
        );
    }

    Ok(result)
}

fn parse_basic_action_formula(parser: &mut Parser) -> Result<ActionFormula, ParseError> {
    let token = parser.get_token();
    let loc = token.loc;

    Ok(match &token.value {
        LexicalElement::Val => {
            parser.skip_token();
            parser.expect_token(&LexicalElement::OpeningParen)?;
            let value = Rc::new(parser.parse::<Expr>()?);
            parser.expect_token(&LexicalElement::ClosingParen)?;
            ActionFormula::new(ActionFormulaEnum::Val { value }, parser.until_now(&loc))
        },
        LexicalElement::Identifier(_) | LexicalElement::Tau => {
            let values = parse_multi_action(parser)?;
            ActionFormula::new(
                ActionFormulaEnum::MultiAction { values },
                parser.until_now(&loc),
            )
        },
        LexicalElement::OpeningParen => {
            parser.skip_token();
            let mut action_formula = parser.parse::<ActionFormula>()?;
            parser.expect_token(&LexicalElement::ClosingParen)?;
            action_formula.loc = parser.until_now(&loc);
            action_formula
        },
        LexicalElement::True => {
            parser.skip_token();
            ActionFormula::new(ActionFormulaEnum::True, loc)
        },
        LexicalElement::False => {
            parser.skip_token();
            ActionFormula::new(ActionFormulaEnum::False, loc)
        },
        LexicalElement::Forall => {
            parser.parse::<ActionFormula>()?
        },
        LexicalElement::Exists => {
            parser.parse::<ActionFormula>()?
        },
        LexicalElement::ExclamationMark => {
            parser.skip_token();
            let value = Rc::new(parse_basic_action_formula(parser)?);
            ActionFormula::new(
                ActionFormulaEnum::Not { value },
                parser.until_now(&loc),
            )
        },
        _ => {
            return Err(ParseError::expected("an action formula", token));
        },
    })
}

