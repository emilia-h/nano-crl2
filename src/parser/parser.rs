
use crate::core::error::Mcrl2Error;
use crate::core::model::Model;
use crate::parser::lexer::Token;

pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub character: usize,
}

impl Into<Mcrl2Error> for ParseError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::ModelSyntaxError {
            message: self.message,
            line: self.line,
            character: self.character,
        }
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &[Token]) -> Parser {
        Parser { tokens }
    }

    pub fn parse_model(&mut self) -> Result<Model, ParseError> {
        eprintln!("{:?}", &self.tokens);

        Ok(Model {
            declarations: Vec::new(),
        })
    }
}
