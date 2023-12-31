//! Implements a mu-calculus formula parser.
//! 
//! The functions can be found [here].
//! 
//! [here]: ../parser/struct.Parser.html#method.parse_state_formula

use crate::ast::formula::{
    ActionFormula, ActionFormulaEnum, RegularFormula, RegularFormulaEnum,
    StateFormula, StateFormulaEnum
};
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
    /// # See also
    /// The [mCRL2 specification on this].
    /// 
    /// [parse error]: ../parser/struct.ParseError.html
    /// [state formula]: ../ast/formula/struct.StateFormula.html
    /// [mCRL2 specification on this]: https://www.mcrl2.org/web/user_manual/language_reference/mucalc.html#state-formulas
    pub fn parse_state_formula(&mut self) -> Result<StateFormula, ParseError> {
        // => (associates to the right)
        let loc = self.get_loc();
        let lhs = self.parse_or_state_formula()?;

        if self.skip_if_equal(&LexicalElement::ThickArrow) {
            let rhs = Rc::new(self.parse_state_formula()?);
            Ok(StateFormula::new(
                StateFormulaEnum::Implies { lhs: Rc::new(lhs), rhs },
                self.until_now(&loc),
            ))
        } else {
            Ok(lhs)
        }
    }

    pub fn parse_or_state_formula(&mut self) -> Result<StateFormula, ParseError> {
        // || (associative, treat as if it associates to the right)
        let loc = self.get_loc();
        let lhs = self.parse_and_state_formula()?;

        if self.skip_if_equal(&LexicalElement::DoublePipe) {
            let rhs = Rc::new(self.parse_or_state_formula()?);
            Ok(StateFormula::new(
                StateFormulaEnum::Or { lhs: Rc::new(lhs), rhs },
                self.until_now(&loc),
            ))
        } else {
            Ok(lhs)
        }
    }

    // TODO forall, exists, implies, not

    fn parse_and_state_formula(&mut self) -> Result<StateFormula, ParseError> {
        // && (associative, treat as if it associates to the right)
        let loc = self.get_loc();
        let lhs = self.parse_basic_state_formula()?;

        if self.skip_if_equal(&LexicalElement::DoubleAmpersand) {
            let rhs = Rc::new(self.parse_and_state_formula()?);
            Ok(StateFormula::new(
                StateFormulaEnum::And { lhs: Rc::new(lhs), rhs },
                self.until_now(&loc),
            ))
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
                Ok(StateFormula::new(StateFormulaEnum::True, self.until_now(&loc)))
            },
            LexicalElement::False => {
                self.skip_token();
                Ok(StateFormula::new(StateFormulaEnum::False, self.until_now(&loc)))
            },
            LexicalElement::Identifier(id) => {
                let id = Identifier::new(id);
                self.skip_token();
                Ok(StateFormula::new(StateFormulaEnum::Id { id }, self.until_now(&loc)))
            },
            LexicalElement::OpeningBracket => {
                self.skip_token();
                let regular_formula = Rc::new(self.parse_regular_formula()?);
                self.expect_token(&LexicalElement::ClosingBracket)?;
                let formula = Rc::new(self.parse_basic_state_formula()?);
                Ok(StateFormula::new(
                    StateFormulaEnum::Box { regular_formula, formula },
                    self.until_now(&loc),
                ))
            },
            LexicalElement::LessThan => {
                self.skip_token();
                let regular_formula = Rc::new(self.parse_regular_formula()?);
                self.expect_token(&LexicalElement::GreaterThan)?;
                let formula = Rc::new(self.parse_basic_state_formula()?);
                Ok(StateFormula::new(
                    StateFormulaEnum::Diamond { regular_formula, formula },
                    self.until_now(&loc),
                ))
            },
            LexicalElement::Mu => {
                self.skip_token();
                let id = self.parse_identifier()?;
                self.expect_token(&LexicalElement::Period)?;
                let formula = Rc::new(self.parse_basic_state_formula()?);
                Ok(StateFormula::new(
                    StateFormulaEnum::Mu { id, formula },
                    self.until_now(&loc),
                ))
            },
            LexicalElement::Nu => {
                self.skip_token();
                let id = self.parse_identifier()?;
                self.expect_token(&LexicalElement::Period)?;
                let formula = Rc::new(self.parse_basic_state_formula()?);
                Ok(StateFormula::new(
                    StateFormulaEnum::Nu { id, formula },
                    self.until_now(&loc),
                ))
            },
            LexicalElement::OpeningParen => {
                self.skip_token();
                let mut formula = self.parse_state_formula()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                formula.loc = self.until_now(&loc);
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
        Ok(RegularFormula::new(
            RegularFormulaEnum::ActionFormula { value },
            self.until_now(&loc),
        ))
    }

    pub fn parse_action_formula(&mut self) -> Result<ActionFormula, ParseError> {
        // forall, exists
        let loc = self.get_loc();

        if self.skip_if_equal(&LexicalElement::Forall) {
            let ids = self.parse_var_decl_list()?;
            self.expect_token(&LexicalElement::Period)?;
            let action_formula = Rc::new(self.parse_action_formula()?);
            Ok(ActionFormula::new(
                ActionFormulaEnum::Forall { ids, action_formula },
                self.until_now(&loc),
            ))
        } else if self.skip_if_equal(&LexicalElement::Exists) {
            let ids = self.parse_var_decl_list()?;
            self.expect_token(&LexicalElement::Period)?;
            let action_formula = Rc::new(self.parse_action_formula()?);
            Ok(ActionFormula::new(
                ActionFormulaEnum::Exists { ids, action_formula },
                self.until_now(&loc),
            ))
        } else {
            self.parse_implies_action_formula()
        }
    }

    fn parse_implies_action_formula(&mut self) -> Result<ActionFormula, ParseError> {
        // => (associates to the right)
        let loc = self.get_loc();
        let lhs = self.parse_or_action_formula()?;

        if self.skip_if_equal(&LexicalElement::ThickArrow) {
            let rhs = Rc::new(self.parse_action_formula()?);
            Ok(ActionFormula::new(
                ActionFormulaEnum::Implies { lhs: Rc::new(lhs), rhs },
                self.until_now(&loc),
            ))
        } else {
            Ok(lhs)
        }
    }

    fn parse_or_action_formula(&mut self) -> Result<ActionFormula, ParseError> {
        // || (associates to the right)
        let loc = self.get_loc();
        let lhs = self.parse_and_action_formula()?;

        if self.skip_if_equal(&LexicalElement::DoublePipe) {
            let rhs = Rc::new(self.parse_or_action_formula()?);
            Ok(ActionFormula::new(
                ActionFormulaEnum::Or { lhs: Rc::new(lhs), rhs },
                self.until_now(&loc),
            ))
        } else {
            Ok(lhs)
        }
    }

    fn parse_and_action_formula(&mut self) -> Result<ActionFormula, ParseError> {
        // && (associates to the right)
        let loc = self.get_loc();
        let lhs = self.parse_time_action_formula()?;

        if self.skip_if_equal(&LexicalElement::DoubleAmpersand) {
            let rhs = Rc::new(self.parse_and_action_formula()?);
            Ok(ActionFormula::new(
                ActionFormulaEnum::And { lhs: Rc::new(lhs), rhs },
                self.until_now(&loc),
            ))
        } else {
            Ok(lhs)
        }
    }

    fn parse_time_action_formula(&mut self) -> Result<ActionFormula, ParseError> {
        // @ (associates to the left)
        let loc = self.get_loc();
        let mut result = self.parse_basic_action_formula()?;

        while self.skip_if_equal(&LexicalElement::AtSign) {
            let time = Rc::new(self.parse_expr()?);
            result = ActionFormula::new(
                ActionFormulaEnum::Time {
                    action_formula: Rc::new(result),
                    time,
                },
                self.until_now(&loc),
            );
        }

        Ok(result)
    }

    fn parse_basic_action_formula(&mut self) -> Result<ActionFormula, ParseError> {
        let token = self.get_token();
        let loc = token.loc;

        Ok(match &token.value {
            LexicalElement::Val => {
                self.skip_token();
                self.expect_token(&LexicalElement::OpeningParen)?;
                let value = Rc::new(self.parse_expr()?);
                self.expect_token(&LexicalElement::ClosingParen)?;
                ActionFormula::new(ActionFormulaEnum::Val { value }, self.until_now(&loc))
            },
            LexicalElement::Identifier(_) | LexicalElement::Tau => {
                let values = self.parse_multi_action()?;
                ActionFormula::new(
                    ActionFormulaEnum::MultiAction { values },
                    self.until_now(&loc),
                )
            },
            LexicalElement::OpeningParen => {
                self.skip_token();
                let mut action_formula = self.parse_action_formula()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                action_formula.loc = self.until_now(&loc);
                action_formula
            },
            LexicalElement::True => {
                self.skip_token();
                ActionFormula::new(ActionFormulaEnum::True, loc)
            },
            LexicalElement::False => {
                self.skip_token();
                ActionFormula::new(ActionFormulaEnum::False, loc)
            },
            LexicalElement::Forall => {
                self.parse_action_formula()?
            },
            LexicalElement::Exists => {
                self.parse_action_formula()?
            },
            LexicalElement::ExclamationMark => {
                self.skip_token();
                let value = Rc::new(self.parse_basic_action_formula()?);
                ActionFormula::new(
                    ActionFormulaEnum::Not { value },
                    self.until_now(&loc),
                )
            },
            _ => {
                return Err(ParseError::expected("an action formula", token));
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unwrap_pattern;
    use crate::parser::lexer::tokenize;

    fn is_single_action(regular_formula: &RegularFormula, id: &str) -> bool {
        if let RegularFormulaEnum::ActionFormula { value } = &regular_formula.value {
            if let ActionFormulaEnum::MultiAction { values } = &value.value {
                if id == "tau" {
                    values.len() == 0
                } else {
                    values.len() == 1 && values[0].args.len() == 0 && values[0].id.get_value() == id
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    #[test]
    fn test_state_formula_basic() {
        let tokens = tokenize("(([aa] false && mu X . <a>X) || nu Y . <a>Y)").unwrap();
        let formula = Parser::new(&tokens).parse_state_formula().unwrap();

        let (or_lhs, or_rhs) = unwrap_pattern!(&formula.value, StateFormulaEnum::Or { lhs, rhs } => (lhs, rhs));

        let (and_lhs, and_rhs) = unwrap_pattern!(&or_lhs.value, StateFormulaEnum::And { lhs, rhs } => (lhs, rhs));

        let (box_regular_formula, box_formula) =
            unwrap_pattern!(&and_lhs.value, StateFormulaEnum::Box { regular_formula, formula } => (regular_formula, formula));
        assert!(is_single_action(&box_regular_formula, "aa"));
        unwrap_pattern!(&box_formula.value, StateFormulaEnum::False => ());

        let (mu_id, mu_formula) = unwrap_pattern!(&and_rhs.value, StateFormulaEnum::Mu { id, formula } => (id, formula));
        assert_eq!(mu_id.get_value(), "X");

        let (diamond_regular_formula, diamond_formula) =
            unwrap_pattern!(&mu_formula.value, StateFormulaEnum::Diamond { regular_formula, formula } => (regular_formula, formula));
        assert!(is_single_action(&diamond_regular_formula, "a"));

        let x = unwrap_pattern!(&diamond_formula.value, StateFormulaEnum::Id { id } => id);
        assert_eq!(x.get_value(), "X");

        let (nu_id, nu_formula) = unwrap_pattern!(&or_rhs.value, StateFormulaEnum::Nu { id, formula } => (id, formula));
        assert_eq!(nu_id.get_value(), "Y");
        let (diamond_regular_formula, diamond_formula) =
            unwrap_pattern!(&nu_formula.value, StateFormulaEnum::Diamond { regular_formula, formula } => (regular_formula, formula));
        assert!(is_single_action(&diamond_regular_formula, "a"));
        
        let y = unwrap_pattern!(&diamond_formula.value, StateFormulaEnum::Id { id } => id);
        assert_eq!(y.get_value(), "Y");
    }
}
