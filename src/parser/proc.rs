//! Implements an mCRL2 model process parser.
//! 
//! The functions can be found [here].
//! 
//! [here]: ../parser/struct.Parser.html#method.parse_proc

use crate::ast::proc::Proc;
use crate::parser::parser::{ParseError, Parser};

impl<'a> Parser<'a> {
    /// Parses a `Proc` (short for process).
    pub fn parse_proc(&mut self) -> Result<Proc, ParseError> {
        todo!()
    }
}
