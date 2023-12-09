
use crate::ast::formula::{StateFormula, StateFormulaEnum};
use crate::parser::parser::{Parser, ParseError};

impl<'a> Parser<'a> {
    pub fn parse_formula(&mut self) -> Result<StateFormula, ParseError> {
        todo!()
    }
}
