//! Defines a parser that can used for both parsing mCRL2 models and
//! mu-calculus formulas.
//! 
//! # Examples
//! ```
//! # use nano_crl2::parser::lexer::tokenize;
//! # use nano_crl2::parser::parser::Parser;
//! let tokens = tokenize("act a: Nat; proc Repeat = a(123).Repeat; init Repeat;")
//!     .expect("input was free of syntax errors");
//! let mut parser = Parser::new(&tokens);
//! let model = parser.parse_model()
//!     .expect("input was free of syntax errors");
//! 
//! assert!(model.initial.is_some());
//! assert_eq!(model.decls.len(), 2);
//! println!("{:?}", model);
//! ```

use crate::core::error::Mcrl2Error;
use crate::ast::decl::DeclEnum;
use crate::ast::model::Model;
use crate::core::syntax::Identifier;
use crate::core::syntax::SourceLocation;
use crate::parser::lexer::LexicalElement;
use crate::parser::lexer::Token;

use std::rc::Rc;

/// Indicates that there was a syntax error while parsing, which happens when
/// the input is not in a correct format.
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub character: usize,
}

impl ParseError {
    pub fn new(message: String, loc: SourceLocation) -> Self {
        ParseError {
            message,
            line: loc.get_line(),
            character: loc.get_char(),
        }
    }

    /// Creates a parse error with a message of the form "expected A but found
    /// B".
    pub fn expected(expectation: &str, token: &Token) -> Self {
        let mut message = String::from("expected ");
        message.push_str(expectation);
        message.push_str(&format!(" but found {}", token.value));
        ParseError {
            message,
            line: token.loc.get_line(),
            character: token.loc.get_char(),
        }
    }

    /// Creates a parse error with a message of the form "expected A but the
    /// end of the input was reached".
    pub fn end_of_input(expectation: &str, loc: SourceLocation) -> Self {
        let mut message = String::from("expected ");
        message.push_str(expectation);
        message.push_str(" but the end of the input was reached");
        ParseError {
            message,
            line: loc.get_line(),
            character: loc.get_char(),
        }
    }
}

impl Into<Mcrl2Error> for ParseError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::ModelError {
            message: self.message,
            line: self.line,
            character: self.character,
        }
    }
}

/// Represents the state of a parser that iterates over a list of tokens.
/// 
/// These tokens can be parsed from a string using the [tokenize()] function.
/// 
/// [tokenize()]: ../lexer/fn.tokenize.html
#[derive(Clone)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from a slice of tokens, that starts parsing from
    /// the first token.
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, index: 0 }
    }

    /// Parses a full mCRL2 model.
    /// 
    /// These mCRL2 models usually have the `.mcrl2` extension.
    pub fn parse_model(&mut self) -> Result<Model, ParseError> {
        let mut decls = Vec::new();
        let mut initial = None;

        while self.has_token() {
            for decl in self.parse_decl()? {
                if let DeclEnum::InitialDecl { value } = decl.value {
                    initial = Some(value);
                } else {
                    decls.push(Rc::new(decl));
                }
            }
        }

        Ok(Model {
            decls,
            initial,
        })
    }

    /// Parses a single identifier.
    pub fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        if !self.has_token() {
            return Err(ParseError::end_of_input("an identifier", self.get_last_loc()));
        }

        let token = self.get_token();
        let result = if let LexicalElement::Identifier(value) = &token.value {
            Ok(Identifier::new(value))
        } else {
            Err(ParseError::expected("an identifier", &token))
        };
        self.skip_token();
        result
    }

    /// Parses a list of identifiers, separated by commas.
    pub fn parse_identifier_list(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut ids = Vec::new();
        ids.push(self.parse_identifier()?);
        while self.skip_if_equal(&LexicalElement::Comma) {
            ids.push(self.parse_identifier()?);
        }
        Ok(ids)
    }

    // helper functions //

    pub fn get_token(&self) -> &Token {
        assert!(self.has_token());
        &self.tokens[self.index]
    }

    pub fn get_loc(&mut self) -> SourceLocation {
        assert!(self.has_token());
        self.tokens[self.index].loc
    }

    pub fn get_next_token(&self) -> Option<&Token> {
        assert!(self.has_token());
        self.tokens.get(self.index + 1)
    }

    pub fn get_last_loc(&self) -> SourceLocation {
        if self.tokens.len() >= 1 {
            self.tokens[self.tokens.len() - 1].loc
        } else {
            SourceLocation::new(0, 0)
        }
    }

    pub fn is_token(&self, e: &LexicalElement) -> bool {
        self.has_token() && &self.get_token().value == e
    }

    pub fn is_next_token(&self, e: &LexicalElement) -> bool {
        if let Some(x) = self.tokens.get(self.index + 1) {
            &x.value == e
        } else {
            false
        }
    }

    pub fn has_token(&self) -> bool {
        self.index < self.tokens.len()
    }

    pub fn skip_token(&mut self) {
        self.index += 1;
    }

    pub fn expect_token(&mut self, value: &LexicalElement) -> Result<(), ParseError> {
        let token = self.get_token();
        if &token.value == value {
            self.skip_token();
            Ok(())
        } else {
            Err(ParseError::expected(&format!("{}", value), token))
        }
    }

    pub fn skip_if_equal(&mut self, value: &LexicalElement) -> bool {
        if self.has_token() && &self.get_token().value == value {
            self.skip_token();
            true
        } else {
            false
        }
    }
}
