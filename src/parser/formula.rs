//! Implements a mu-calculus formula parser.
//! 
//! The functions can be found [here].
//! 
//! [here]: ../parser/struct.Parser.html#method.parse_state_formula

use crate::ast::formula::{
    ActionFormula, ActionFormulaEnum, RegularFormula, RegularFormulaEnum,
    StateFormula, StateFormulaEnum
};
use crate::ast::proc::Action;
use crate::core::syntax::Identifier;
use crate::parser::lexer::LexicalElement;
use crate::parser::parser::{Parser, ParseError};

use std::rc::Rc;

impl<'a> Parser<'a> {
    /// Parses a state formula.
    /// 
    /// # Returns
    /// A [`StateFormula`] if the parser starts at a list of tokens that
    /// represent a state formula, a [parse error] otherwise.
    /// 
    /// [parse error]: ../parser/struct.ParseError.html
    /// [state formula]: ../ast/formula/struct.StateFormula.html
    pub fn parse_state_formula(&mut self) -> Result<StateFormula, ParseError> {
        // || (associative, treat as if it associates to the right)
        let loc = self.get_loc();
        let lhs = self.parse_and_state_formula()?;

        if self.skip_if_equal(&LexicalElement::DoublePipe) {
            let rhs = Rc::new(self.parse_state_formula()?);
            Ok(StateFormula::new(StateFormulaEnum::Or { lhs: Rc::new(lhs), rhs }, loc))
        } else {
            Ok(lhs)
        }
    }

    // TODO forall, exists, implies, not

    // || (associative, treat as if it associates to the right)
    fn parse_and_state_formula(&mut self) -> Result<StateFormula, ParseError> {
        let loc = self.get_loc();
        let lhs = self.parse_basic_state_formula()?;

        if self.skip_if_equal(&LexicalElement::LogicalAnd) {
            let rhs = Rc::new(self.parse_and_state_formula()?);
            Ok(StateFormula::new(StateFormulaEnum::And { lhs: Rc::new(lhs), rhs }, loc))
        } else {
            Ok(lhs)
        }
    }

    fn parse_basic_state_formula(&mut self) -> Result<StateFormula, ParseError> {
        if !self.has_token() {
            let loc = self.get_last_loc();
            return Err(ParseError::end_of_input("a state formula", loc));
        }

        let token = self.get_token();
        let loc = token.loc;

        match &token.value {
            LexicalElement::True => {
                self.skip_token();
                Ok(StateFormula::new(StateFormulaEnum::True, loc))
            },
            LexicalElement::False => {
                self.skip_token();
                Ok(StateFormula::new(StateFormulaEnum::False, loc))
            },
            LexicalElement::Identifier(id) => {
                let id = Identifier::new(id);
                self.skip_token();
                Ok(StateFormula::new(StateFormulaEnum::Id { id }, loc))
            },
            LexicalElement::OpeningBracket => {
                self.skip_token();
                // let regular_formula = Rc::new(self.parse_regular_formula()?);
                let id = self.parse_identifier()?;
                self.expect_token(&LexicalElement::ClosingBracket)?;
                let formula = Rc::new(self.parse_basic_state_formula()?);
                Ok(StateFormula::new(StateFormulaEnum::Box { action: Action { id }, formula }, loc))
            },
            LexicalElement::LessThan => {
                self.skip_token();
                // let regular_formula = Rc::new(self.parse_regular_formula()?);
                let id = self.parse_identifier()?;
                self.expect_token(&LexicalElement::GreaterThan)?;
                let formula = Rc::new(self.parse_basic_state_formula()?);
                Ok(StateFormula::new(StateFormulaEnum::Diamond { action: Action { id }, formula }, loc))
            },
            LexicalElement::Mu => {
                self.skip_token();
                let id = self.parse_identifier()?;
                self.expect_token(&LexicalElement::Period)?;
                let formula = Rc::new(self.parse_basic_state_formula()?);
                Ok(StateFormula::new(StateFormulaEnum::Mu { id, formula }, loc))
            },
            LexicalElement::Nu => {
                self.skip_token();
                let id = self.parse_identifier()?;
                self.expect_token(&LexicalElement::Period)?;
                let formula = Rc::new(self.parse_basic_state_formula()?);
                Ok(StateFormula::new(StateFormulaEnum::Nu { id, formula }, loc))
            },
            LexicalElement::OpeningParen => {
                self.skip_token();
                let formula = self.parse_state_formula()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                Ok(formula)
            },
            _ => {
                Err(ParseError::expected("a formula", token))
            },
        }
    }

    pub fn parse_regular_formula(&mut self) -> Result<RegularFormula, ParseError> {
        let loc = self.get_loc();

        let value = Rc::new(self.parse_action_formula()?);
        // TODO other regular formulas
        Ok(RegularFormula::new(RegularFormulaEnum::ActionFormula { value }, loc))
    }

    pub fn parse_action_formula(&mut self) -> Result<ActionFormula, ParseError> {
        let token = self.get_token();
        let loc = token.loc;

        if let LexicalElement::Identifier(id) = &token.value {
            let values = vec![Action { id: Identifier::new(id) }];
            self.skip_token();
            Ok(ActionFormula::new(ActionFormulaEnum::MultiAction { values }, loc))
        } else {
            // TODO other action formulas
            Err(ParseError::expected("an action formula", token))
        }
    }
}

#[cfg(test)]
use crate::unwrap_pattern;
#[cfg(test)]
use crate::parser::lexer::tokenize;
#[cfg(test)]
use crate::util::unwrap_result;

#[test]
fn test_state_formula_basic() {
    let tokens = tokenize("(([aa] false && mu X . <a>X) || nu Y . <a>Y)").unwrap();
    let formula = unwrap_result(Parser::new(&tokens).parse_state_formula());

    let (or_lhs, or_rhs) = unwrap_pattern!(&formula.value, StateFormulaEnum::Or { lhs, rhs } => (lhs, rhs));

    let (and_lhs, and_rhs) = unwrap_pattern!(&or_lhs.value, StateFormulaEnum::And { lhs, rhs } => (lhs, rhs));

    let (box_action, box_formula) =
        unwrap_pattern!(&and_lhs.value, StateFormulaEnum::Box { action, formula } => (action, formula));
    assert_eq!(box_action.id.get_value(), "aa");
    unwrap_pattern!(&box_formula.value, StateFormulaEnum::False => ());

    let (mu_id, mu_formula) = unwrap_pattern!(&and_rhs.value, StateFormulaEnum::Mu { id, formula } => (id, formula));
    assert_eq!(mu_id.get_value(), "X");

    let (diamond_action, diamond_formula) =
        unwrap_pattern!(&mu_formula.value, StateFormulaEnum::Diamond { action, formula } => (action, formula));
    assert_eq!(diamond_action.id.get_value(), "a");
    
    let x = unwrap_pattern!(&diamond_formula.value, StateFormulaEnum::Id { id } => id);
    assert_eq!(x.get_value(), "X");

    let (nu_id, nu_formula) = unwrap_pattern!(&or_rhs.value, StateFormulaEnum::Nu { id, formula } => (id, formula));
    assert_eq!(nu_id.get_value(), "Y");
    let (diamond_action, diamond_formula) =
        unwrap_pattern!(&nu_formula.value, StateFormulaEnum::Diamond { action, formula } => (action, formula));
    assert_eq!(diamond_action.id.get_value(), "a");
    
    let y = unwrap_pattern!(&diamond_formula.value, StateFormulaEnum::Id { id } => id);
    assert_eq!(y.get_value(), "Y");
}
