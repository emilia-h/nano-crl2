//! Defines AST structures for mu-calculus formulas.
//! 
//! These are the "top-level" state formumlas; for the regular formulas and
//! action formulas that can be used inside of a box or diamond operator, see
//! the [regular formula module] and [action formula module] respectively.
//! 
//! # See also
//! The [mCRL2 spec on this].
//! 
//! [regular formula module]: ../regular_formula/index.html
//! [action formula module]: ../action_formula/index.html
//! [mCRL2 spec on this]: https://mcrl2.org/web/user_manual/language_reference/mucalc.html#state-formulas

use crate::core::lexer::LexicalElement;
use crate::core::parser::{Parseable, ParseError, Parser};
use crate::core::syntax::{Identifier, SourceLocation};
use crate::model::expr::Expr;
use crate::mu_calculus::regular_formula::RegularFormula;

use std::fmt::{Debug, Formatter};
use std::rc::Rc;

/// A mu-calculus formula that describes a property of an LTS or mCRL2 model.
pub struct StateFormula {
    pub value: StateFormulaEnum,
    pub loc: SourceLocation,
}

impl StateFormula {
    /// Creates a new state formula with `parent` set to `None`.
    pub fn new(value: StateFormulaEnum, loc: SourceLocation) -> Self {
        StateFormula { value, loc }
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
    Not {
        value: Rc<StateFormula>,
    },
    Box {
        regular_formula: Rc<RegularFormula>,
        formula: Rc<StateFormula>,
    },
    Diamond {
        regular_formula: Rc<RegularFormula>,
        formula: Rc<StateFormula>,
    },
}

impl Parseable for StateFormula {
    fn parse(parser: &mut Parser) -> Result<StateFormula, ParseError> {
        parse_state_formula(parser)
    }
}

/// Parses a state formula.
/// 
/// # Returns
/// A [`StateFormula`] if the parser starts at a list of tokens that
/// represent a state formula, a [parse error] otherwise.
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [parse error]: ../../core/parser/struct.ParseError.html
/// [`StateFormula`]: ./struct.StateFormula.html
/// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/mucalc.html#grammar-token-StateFrm
pub fn parse_state_formula(parser: &mut Parser) -> Result<StateFormula, ParseError> {
    // => (associates to the right)
    let loc = parser.get_loc();
    let lhs = parse_or_state_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::ThickArrow) {
        let rhs = Rc::new(parse_state_formula(parser)?);
        Ok(StateFormula::new(
            StateFormulaEnum::Implies { lhs: Rc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_or_state_formula(parser: &mut Parser) -> Result<StateFormula, ParseError> {
    // || (associative, treat as if it associates to the right)
    let loc = parser.get_loc();
    let lhs = parse_and_state_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::DoublePipe) {
        let rhs = Rc::new(parse_or_state_formula(parser)?);
        Ok(StateFormula::new(
            StateFormulaEnum::Or { lhs: Rc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

// TODO forall, exists, implies, not

fn parse_and_state_formula(parser: &mut Parser) -> Result<StateFormula, ParseError> {
    // && (associative, treat as if it associates to the right)
    let loc = parser.get_loc();
    let lhs = parse_basic_state_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::DoubleAmpersand) {
        let rhs = Rc::new(parse_and_state_formula(parser)?);
        Ok(StateFormula::new(
            StateFormulaEnum::And { lhs: Rc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_basic_state_formula(parser: &mut Parser) -> Result<StateFormula, ParseError> {
    if !parser.has_token() {
        let loc = parser.get_last_loc();
        return Err(ParseError::end_of_input("a state formula", loc));
    }

    let token = parser.get_token();
    let loc = token.loc;

    match &token.value {
        LexicalElement::True => {
            parser.skip_token();
            Ok(StateFormula::new(StateFormulaEnum::True, parser.until_now(&loc)))
        },
        LexicalElement::False => {
            parser.skip_token();
            Ok(StateFormula::new(StateFormulaEnum::False, parser.until_now(&loc)))
        },
        LexicalElement::Identifier(id) => {
            let id = Identifier::new(id);
            parser.skip_token();
            Ok(StateFormula::new(StateFormulaEnum::Id { id }, parser.until_now(&loc)))
        },
        LexicalElement::OpeningBracket => {
            parser.skip_token();
            let regular_formula = Rc::new(parser.parse::<RegularFormula>()?);
            parser.expect_token(&LexicalElement::ClosingBracket)?;
            let formula = Rc::new(parse_basic_state_formula(parser)?);
            Ok(StateFormula::new(
                StateFormulaEnum::Box { regular_formula, formula },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::LessThan => {
            parser.skip_token();
            let regular_formula = Rc::new(parser.parse::<RegularFormula>()?);
            parser.expect_token(&LexicalElement::GreaterThan)?;
            let formula = Rc::new(parse_basic_state_formula(parser)?);
            Ok(StateFormula::new(
                StateFormulaEnum::Diamond { regular_formula, formula },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::Mu => {
            parser.skip_token();
            let id = parser.parse_identifier()?;
            parser.expect_token(&LexicalElement::Period)?;
            let formula = Rc::new(parse_basic_state_formula(parser)?);
            Ok(StateFormula::new(
                StateFormulaEnum::Mu { id, formula },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::Nu => {
            parser.skip_token();
            let id = parser.parse_identifier()?;
            parser.expect_token(&LexicalElement::Period)?;
            let formula = Rc::new(parse_basic_state_formula(parser)?);
            Ok(StateFormula::new(
                StateFormulaEnum::Nu { id, formula },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::OpeningParen => {
            parser.skip_token();
            let mut formula = parse_state_formula(parser)?;
            parser.expect_token(&LexicalElement::ClosingParen)?;
            formula.loc = parser.until_now(&loc);
            Ok(formula)
        },
        _ => {
            Err(ParseError::expected("a formula", token))
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unwrap_pattern;
    use crate::core::lexer::tokenize;
    use crate::mu_calculus::action_formula::ActionFormulaEnum;
    use crate::mu_calculus::regular_formula::{RegularFormula, RegularFormulaEnum};

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
        let formula = Parser::new(&tokens).parse::<StateFormula>().unwrap();

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