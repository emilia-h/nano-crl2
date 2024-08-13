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
use crate::core::syntax::{Identifier, SourceRange};
use crate::model::expr::Expr;
use crate::mu_calculus::regular_formula::RegularFormula;

use std::fmt::{Debug, Formatter};
use std::sync::Arc;

/// A mu-calculus formula that describes a property of an LTS or mCRL2 model.
pub struct StateFormula {
    pub value: StateFormulaEnum,
    pub loc: SourceRange,
}

impl StateFormula {
    /// Creates a new state formula with `parent` set to `None`.
    pub fn new(value: StateFormulaEnum, loc: SourceRange) -> Self {
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
        expr: Arc<Expr>,
    },
    Yaled {
        expr: Arc<Expr>,
    },
    Mu {
        id: Identifier,
        id_loc: SourceRange,
        // TODO data
        formula: Arc<StateFormula>,
    },
    Nu {
        id: Identifier,
        id_loc: SourceRange,
        // TODO data
        formula: Arc<StateFormula>,
    },
    Forall {
        ids: Vec<(Identifier, SourceRange, Arc<StateFormula>)>,
        formula: Arc<StateFormula>,
    },
    Exists {
        ids: Vec<(Identifier, SourceRange, Arc<StateFormula>)>,
        formula: Arc<StateFormula>,
    },
    Implies {
        lhs: Arc<StateFormula>,
        rhs: Arc<StateFormula>,
    },
    Or {
        lhs: Arc<StateFormula>,
        rhs: Arc<StateFormula>,
    },
    And {
        lhs: Arc<StateFormula>,
        rhs: Arc<StateFormula>,
    },
    Not {
        value: Arc<StateFormula>,
    },
    Box {
        regular_formula: Arc<RegularFormula>,
        formula: Arc<StateFormula>,
    },
    Diamond {
        regular_formula: Arc<RegularFormula>,
        formula: Arc<StateFormula>,
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
    if !parser.has_token() {
        return parser.end_of_input("a state formula");
    }

    let loc = parser.get_loc();
    let lhs = parse_or_state_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::ThickArrow) {
        let rhs = Arc::new(parse_state_formula(parser)?);
        Ok(StateFormula::new(
            StateFormulaEnum::Implies { lhs: Arc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_or_state_formula(parser: &mut Parser) -> Result<StateFormula, ParseError> {
    // || (associative, treat as if it associates to the right)
    if !parser.has_token() {
        return parser.end_of_input("a state formula");
    }

    let loc = parser.get_loc();
    let lhs = parse_and_state_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::DoublePipe) {
        let rhs = Arc::new(parse_or_state_formula(parser)?);
        Ok(StateFormula::new(
            StateFormulaEnum::Or { lhs: Arc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

// TODO forall, exists, implies, not

fn parse_and_state_formula(parser: &mut Parser) -> Result<StateFormula, ParseError> {
    // && (associative, treat as if it associates to the right)
    if !parser.has_token() {
        return parser.end_of_input("a state formula");
    }

    let loc = parser.get_loc();
    let lhs = parse_basic_state_formula(parser)?;

    if parser.skip_if_equal(&LexicalElement::DoubleAmpersand) {
        let rhs = Arc::new(parse_and_state_formula(parser)?);
        Ok(StateFormula::new(
            StateFormulaEnum::And { lhs: Arc::new(lhs), rhs },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_basic_state_formula(parser: &mut Parser) -> Result<StateFormula, ParseError> {
    if !parser.has_token() {
        return parser.end_of_input("a state formula");
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
            let regular_formula = Arc::new(parser.parse::<RegularFormula>()?);
            parser.expect_token(&LexicalElement::ClosingBracket)?;
            let formula = Arc::new(parse_basic_state_formula(parser)?);
            Ok(StateFormula::new(
                StateFormulaEnum::Box { regular_formula, formula },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::LessThan => {
            parser.skip_token();
            let regular_formula = Arc::new(parser.parse::<RegularFormula>()?);
            parser.expect_token(&LexicalElement::GreaterThan)?;
            let formula = Arc::new(parse_basic_state_formula(parser)?);
            Ok(StateFormula::new(
                StateFormulaEnum::Diamond { regular_formula, formula },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::Mu => {
            parser.skip_token();
            let (id, id_loc) = parser.parse_identifier()?;
            parser.expect_token(&LexicalElement::Period)?;
            let formula = Arc::new(parse_basic_state_formula(parser)?);
            Ok(StateFormula::new(
                StateFormulaEnum::Mu { id, id_loc, formula },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::Nu => {
            parser.skip_token();
            let (id, id_loc) = parser.parse_identifier()?;
            parser.expect_token(&LexicalElement::Period)?;
            let formula = Arc::new(parse_basic_state_formula(parser)?);
            Ok(StateFormula::new(
                StateFormulaEnum::Nu { id, id_loc, formula },
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
                    values.len() == 1 &&
                    values[0].0.args.len() == 0 &&
                    values[0].0.id.get_value() == id
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

        let StateFormulaEnum::Box { regular_formula: box_regular_formula, formula: box_formula } =
            &and_lhs.value else { panic!() };
        assert!(is_single_action(&box_regular_formula, "aa"));
        unwrap_pattern!(&box_formula.value, StateFormulaEnum::False => ());

        let StateFormulaEnum::Mu { id: mu_id, formula: mu_formula, .. } = &and_rhs.value else { panic!() };
        assert_eq!(mu_id.get_value(), "X");

        let StateFormulaEnum::Diamond { regular_formula: diamond_regular_formula, formula: diamond_formula } =
            &mu_formula.value else { panic!() };
        assert!(is_single_action(&diamond_regular_formula, "a"));

        let x = unwrap_pattern!(&diamond_formula.value, StateFormulaEnum::Id { id } => id);
        assert_eq!(x.get_value(), "X");

        let StateFormulaEnum::Nu { id: nu_id, formula: nu_formula, .. } = &or_rhs.value else { panic!() };
        assert_eq!(nu_id.get_value(), "Y");
        let (diamond_regular_formula, diamond_formula) =
            unwrap_pattern!(&nu_formula.value, StateFormulaEnum::Diamond { regular_formula, formula } => (regular_formula, formula));
        assert!(is_single_action(&diamond_regular_formula, "a"));
        
        let y = unwrap_pattern!(&diamond_formula.value, StateFormulaEnum::Id { id } => id);
        assert_eq!(y.get_value(), "Y");
    }
}