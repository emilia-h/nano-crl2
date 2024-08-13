//! Defines AST structures for mu-calculus regular formulas, which are formulas
//! to encode a set of sequences of actions inside a box (`[...]`) or diamond
//! (`<...>`) operators.
//! 
//! # See also
//! The [mCRL2 spec on this].
//! 
//! [mCRL2 spec on this]: https://mcrl2.org/web/user_manual/language_reference/mucalc.html#regular-formulas

use crate::core::parser::{Parseable, ParseError, Parser};
use crate::core::syntax::SourceRange;
use crate::mu_calculus::action_formula::ActionFormula;

use std::fmt::{Debug, Formatter};
use std::sync::Arc;

/// A regular formula that can be used within the box (`[...]`) or diamond
/// (`<...>`) operator.
pub struct RegularFormula {
    pub value: RegularFormulaEnum,
    pub loc: SourceRange,
}

impl RegularFormula {
    /// Creates a new regular formula with `parent` set to `None`.
    pub fn new(value: RegularFormulaEnum, loc: SourceRange) -> Self {
        RegularFormula { value, loc }
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
        value: Arc<ActionFormula>,
    },
    Add {
        lhs: Arc<RegularFormula>,
        rhs: Arc<RegularFormula>,
    },
    Concat {
        lhs: Arc<RegularFormula>,
        rhs: Arc<RegularFormula>,
    },
    Star {
        value: Arc<RegularFormula>,
    },
    Plus {
        value: Arc<RegularFormula>,
    },
}

impl Parseable for RegularFormula {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_regular_formula(parser)
    }
}

/// Parses a regular formula.
/// 
/// # Returns
/// A [`RegularFormula`] if the parser starts at a list of tokens that
/// represent a state formula, a [parse error] otherwise.
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [parse error]: ../../core/parser/struct.ParseError.html
/// [`RegularFormula`]: ./struct.RegularFormula.html
/// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/mucalc.html#grammar-token-RegFrm
fn parse_regular_formula(parser: &mut Parser) -> Result<RegularFormula, ParseError> {
    if !parser.has_token() {
        return parser.end_of_input("a regular formula");
    }

    let loc = parser.get_loc();

    let value = Arc::new(parser.parse::<ActionFormula>()?);
    // TODO other regular formulas
    Ok(RegularFormula::new(
        RegularFormulaEnum::ActionFormula { value },
        parser.until_now(&loc),
    ))
}