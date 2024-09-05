//! Defines a parser that can used for both parsing mCRL2 models and
//! mu-calculus formulas.
//! 
//! # Examples
//! ```
//! # use nano_crl2::core::lexer::tokenize;
//! # use nano_crl2::core::parser::Parser;
//! use nano_crl2::model::module::Module;
//! let tokens = tokenize("act a: Nat; proc Repeat = a(123).Repeat; init Repeat;")
//!     .expect("input was free of syntax errors");
//! let mut parser = Parser::new(&tokens);
//! let module = parser.parse::<Module>()
//!     .expect("input was free of syntax errors");
//! 
//! assert!(module.initial.is_some());
//! assert_eq!(module.decls.len(), 2);
//! ```

use crate::core::diagnostic::{Diagnostic, DiagnosticSeverity};
use crate::core::lexer::{LexicalElement, Token};
use crate::core::syntax::{Identifier, SourceRange};

/// A syntax error while parsing, which happens when the input is not in a
/// correct format.
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub loc: SourceRange,
}

impl ParseError {
    pub fn into_diagnostic(self, file: Option<String>) -> Diagnostic {
        Diagnostic {
            severity: DiagnosticSeverity::Error,
            file,
            loc: Some(self.loc),
            message: self.message,
        }
    }
}

impl ParseError {
    /// Creates a parse error with a given message and source location.
    pub fn new(message: String, loc: SourceRange) -> Self {
        ParseError { message, loc }
    }

    /// Creates a parse error with a message of the form "expected X but found
    /// Y".
    pub fn expected(expectation: &str, token: &Token) -> Self {
        let message = format!(
            "expected {} but found {}",
            expectation,
            token.value,
        );
        ParseError { message, loc: token.loc }
    }

    /// Creates a parse error with a message of the form "expected X but the
    /// end of the input was reached".
    pub fn end_of_input(expectation: &str, loc: SourceRange) -> Self {
        let message = format!(
            "expected {} but the end of the input was reached",
            expectation,
        );
        ParseError { message, loc }
    }

    /// Creates a parse error signaling that a mismatching parenthesis,
    /// bracket, or other delimiter was encountered, with a message of the form
    /// "found unmatched delimiter X".
    pub fn delimiter_mismatch(delimiter: &str, loc: SourceRange) -> Self {
        let message = format!(
            "found unmatched closing delimiter {}",
            delimiter,
        );
        ParseError { message, loc }
    }
}

/// The state of a parser that iterates over a list of tokens.
/// 
/// These tokens can be parsed from a string using the [tokenize()] function.
/// 
/// [tokenize()]: ../lexer/fn.tokenize.html
pub struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    errors: Vec<ParseError>, // TODO report as many errors as possible
}

impl<'a> Parser<'a> {
    /// Creates a new parser from a slice of tokens, that starts parsing from
    /// the first token.
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut parser = Parser {
            tokens,
            index: 0,
            errors: Vec::new(),
        };
        parser.skip_comments();
        parser
    }

    /// Returns a new parser with the same input that is also at the same point
    /// as the original parser. However, it resets the error state.
    /// 
    /// This function is meant for parts of the parser that need to do
    /// lookahead, which is necessary for about two elements in the mCRL2
    /// grammar.
    pub fn create_temp_copy(&self) -> Self {
        Parser {
            tokens: self.tokens,
            index: self.index,
            errors: Vec::new(),
        }
    }

    /// Parses an arbitrary type that has the `Parseable` trait implemented.
    pub fn parse<T: Parseable>(&mut self) -> Result<T, ParseError> {
        T::parse(self)
    }

    /// Parses a single identifier.
    pub fn parse_identifier(&mut self) -> Result<(Identifier, SourceRange), ParseError> {
        if !self.has_token() {
            return self.end_of_input("an identifier");
        }

        let token = self.get_token();
        let result = if let LexicalElement::Identifier(value) = &token.value {
            Ok((Identifier::new(value), token.loc))
        } else {
            Err(ParseError::expected("an identifier", &token))
        };
        self.skip_token();
        result
    }

    /// Parses a list of identifiers, separated by commas.
    pub fn parse_identifier_list(&mut self) -> Result<Vec<(Identifier, SourceRange)>, ParseError> {
        let mut ids = Vec::new();
        ids.push(self.parse_identifier()?);
        while self.skip_if_equal(&LexicalElement::Comma) {
            ids.push(self.parse_identifier()?);
        }
        Ok(ids)
    }

    /// Returns the token that the parser is currently at.
    /// 
    /// # Preconditions
    /// The parser must still have tokens left, i.e. `has_token()` must be
    /// true.
    pub fn get_token(&self) -> &Token {
        assert!(self.has_token());
        &self.tokens[self.index]
    }

    /// Returns the location of the token that the parser is currently at.
    /// 
    /// # Preconditions
    /// The parser must still have tokens left, i.e. `has_token()` must be
    /// true.
    pub fn get_loc(&mut self) -> SourceRange {
        assert!(self.has_token());
        self.tokens[self.index].loc
    }

    /// Looks ahead one token, returning it if it exists or `None` if the next
    /// token does not exist.
    /// 
    /// # Preconditions
    /// The parser must still have tokens left, i.e. `has_token()` must be
    /// true.
    pub fn get_next_token(&self) -> Option<&Token> {
        assert!(self.has_token());
        let mut i = self.index + 1;
        while i < self.tokens.len() && (
            matches!(&self.tokens[i].value, LexicalElement::Comment(_)) ||
            matches!(&self.tokens[i].value, LexicalElement::DocComment(_))
        ) {
            i += 1;
        }
        self.tokens.get(i)
    }

    /// Returns the location of the last token, or `(0, 0)` if there are no
    /// tokens.
    pub fn get_last_loc(&self) -> SourceRange {
        if self.tokens.len() >= 1 {
            self.tokens[self.tokens.len() - 1].loc
        } else {
            SourceRange::new(0, 0, 0, 0)
        }
    }

    /// Returns whether the current token equals a given token type, or false
    /// if there are no tokens left.
    pub fn is_token(&self, e: &LexicalElement) -> bool {
        self.has_token() && &self.get_token().value == e
    }

    /// Returns whether the next token equals a given token type, or false if
    /// there is no next token.
    pub fn is_next_token(&self, e: &LexicalElement) -> bool {
        if !self.has_token() {
            false
        } else if let Some(x) = self.get_next_token() {
            &x.value == e
        } else {
            false
        }
    }

    /// Returns whether the parser still has tokens left.
    pub fn has_token(&self) -> bool {
        self.index < self.tokens.len()
    }

    /// Advances the parser by one token.
    pub fn skip_token(&mut self) {
        assert!(self.has_token());
        self.index += 1;
        self.skip_comments();
    }

    /// Advances the parser by a token if the current token equals the given
    /// lexical element, or returns an error if there is no token or the
    /// current token is different.
    pub fn expect_token(&mut self, value: &LexicalElement) -> Result<(), ParseError> {
        if !self.has_token() {
            return self.end_of_input(&format!("{}", value));
        }
        let token = self.get_token();
        if &token.value == value {
            self.skip_token();
            Ok(())
        } else {
            Err(ParseError::expected(&format!("{}", value), token))
        }
    }

    /// Returns true and advances the parser if the current token equals the
    /// given lexical element, or returns false and does nothing if it is
    /// different.
    pub fn skip_if_equal(&mut self, value: &LexicalElement) -> bool {
        if self.has_token() && &self.get_token().value == value {
            self.skip_token();
            true
        } else {
            false
        }
    }

    /// Advances the parser as long as the current token is either
    /// `LexicalElement::Comment` or `LexicalElement::DocComment`.
    pub fn skip_comments(&mut self) {
        while self.index < self.tokens.len() && (
            matches!(&self.tokens[self.index].value, LexicalElement::Comment(_)) ||
            matches!(&self.tokens[self.index].value, LexicalElement::DocComment(_))
        ) {
            self.index += 1;
        }
    }

    /// Returns an `Err` because of unexpected end of input
    pub fn end_of_input<T>(&self, expectation: &str) -> Result<T, ParseError> {
        Err(ParseError::end_of_input(expectation, self.get_last_loc()))
    }

    /// Returns a new token that spans all tokens from `loc` up till (and
    /// excluding) the current token.
    pub fn until_now(&self, loc: &SourceRange) -> SourceRange {
        if self.index == 0 {
            SourceRange::new(0, 0, 0, 0)
        } else {
            loc.span(&self.tokens[self.index - 1].loc)
        }
    }
}

/// Parses an instance of a type from a stream of [`Token`]s.
/// 
/// [`Token`]: ../lexer/struct.Token.html
pub trait Parseable
where
    Self: Sized,
{
    /// Parses the implemented type from a stream of tokens, from the current
    /// state of the `Parser`.
    /// 
    /// # Returns
    /// An instance of `Self` if successful, a `ParseError` if the input is not
    /// in the correct format for this type.
    fn parse(parser: &mut Parser) -> Result<Self, ParseError>;
}
