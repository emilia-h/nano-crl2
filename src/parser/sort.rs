//! Implements an mCRL2 model sort parser.
//! 
//! The functions can be found [here].
//! 
//! [here]: ../parser/struct.Parser.html#method.parse_sort

use crate::ast::sort::{Constructor, Sort, SortEnum};
use crate::core::syntax::Identifier;
use crate::parser::lexer::LexicalElement;
use crate::parser::parser::{ParseError, Parser};

use std::rc::Rc;

impl<'a> Parser<'a> {
    /// Parses a sort.
    pub fn parse_sort(&mut self) -> Result<Sort, ParseError> {
        // the grammar is not very clear, but -> binds less strong than #
        // also note that -> is right-associative
        let loc = self.get_token().loc;
        let lhs = self.parse_carthesian_sort()?;

        if self.skip_if_equal(&LexicalElement::Arrow) {
            let rhs = self.parse_sort()?;
            Ok(Sort::new(SortEnum::Function { lhs: Rc::new(lhs), rhs: Rc::new(rhs) }, loc))
        } else {
            Ok(lhs)
        }
    }

    fn parse_carthesian_sort(&mut self) -> Result<Sort, ParseError> {
        // note that it's associative
        let loc = self.get_token().loc;
        let lhs = self.parse_basic_sort()?;

        if self.skip_if_equal(&LexicalElement::HashSign) {
            let rhs = self.parse_carthesian_sort()?;
            Ok(Sort::new(SortEnum::Carthesian {
                lhs: Rc::new(lhs), 
                rhs: Rc::new(rhs),
            }, loc))
        } else {
            Ok(lhs)
        }
    }

    fn parse_basic_sort(&mut self) -> Result<Sort, ParseError> {
        let token = self.get_token();
        let loc = token.loc;
        let result = match &token.value {
            LexicalElement::Bool => {
                self.skip_token();
                Ok(Sort::new(SortEnum::Bool, loc))
            },
            LexicalElement::Pos => {
                self.skip_token();
                Ok(Sort::new(SortEnum::Pos, loc))
            },
            LexicalElement::Nat => {
                self.skip_token();
                Ok(Sort::new(SortEnum::Nat, loc))
            },
            LexicalElement::Int => {
                self.skip_token();
                Ok(Sort::new(SortEnum::Int, loc))
            },
            LexicalElement::Real => {
                self.skip_token();
                Ok(Sort::new(SortEnum::Real, loc))
            },
            LexicalElement::List => {
                self.skip_token();
                self.expect_token(&LexicalElement::OpeningParen)?;
                let subsort = self.parse_sort()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                Ok(Sort::new(SortEnum::List { subsort: Rc::new(subsort) }, loc))
            },
            LexicalElement::Set => {
                self.skip_token();
                self.expect_token(&LexicalElement::OpeningParen)?;
                let subsort = self.parse_sort()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                Ok(Sort::new(SortEnum::Set { subsort: Rc::new(subsort) }, loc))
            },
            LexicalElement::Bag => {
                self.skip_token();
                self.expect_token(&LexicalElement::OpeningParen)?;
                let subsort = self.parse_sort()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                Ok(Sort::new(SortEnum::Bag { subsort: Rc::new(subsort) }, loc))
            },
            LexicalElement::FSet => {
                self.skip_token();
                self.expect_token(&LexicalElement::OpeningParen)?;
                let subsort = self.parse_sort()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                Ok(Sort::new(SortEnum::FSet { subsort: Rc::new(subsort) }, loc))
            },
            LexicalElement::FBag => {
                self.skip_token();
                self.expect_token(&LexicalElement::OpeningParen)?;
                let subsort = self.parse_sort()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                Ok(Sort::new(SortEnum::FBag { subsort: Rc::new(subsort) }, loc))
            },
            LexicalElement::Identifier(id) => {
                let s = Ok(Sort::new(SortEnum::Id { id: Identifier::new(id) }, loc));
                self.skip_token();
                s
            },
            LexicalElement::OpeningParen => {
                self.skip_token();
                let sort = self.parse_sort()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                Ok(sort)
            },
            LexicalElement::Struct => {
                self.skip_token();
                let constructors = self.parse_constructor_list()?;
                Ok(Sort::new(SortEnum::Struct { constructors }, loc))
            },
            _ => {
                let s = Err(ParseError::expected("a sort", token));
                self.skip_token();
                s
            }
        };
        result
    }

    fn parse_constructor_list(&mut self) -> Result<Vec<Constructor>, ParseError> {
        let mut constructors = Vec::new();

        constructors.push(self.parse_constructor()?);
        while self.skip_if_equal(&LexicalElement::Pipe) {
            constructors.push(self.parse_constructor()?);
        }

        Ok(constructors)
    }

    fn parse_constructor(&mut self) -> Result<Constructor, ParseError> {
        let id = self.parse_identifier()?;

        let mut properties = Vec::new();
        if self.skip_if_equal(&LexicalElement::OpeningParen) {
            while {
                let id = if self.is_next_token(&LexicalElement::Colon) {
                    let id = self.parse_identifier()?;
                    self.expect_token(&LexicalElement::Colon).unwrap();
                    Some(id)
                } else {
                    None
                };
                let sort = self.parse_sort()?;

                properties.push((id, Rc::new(sort)));

                self.skip_if_equal(&LexicalElement::Comma)
            } {}

            self.expect_token(&LexicalElement::ClosingParen)?;
        }

        let recognizer_function_id = if self.skip_if_equal(&LexicalElement::QuestionMark) {
            Some(self.parse_identifier()?)
        } else {
            None
        };

        Ok(Constructor { id, properties, recognizer_function_id })
    }
}

#[cfg(test)]
use crate::unwrap_pattern;
#[cfg(test)]
use crate::util::unwrap_result;
#[cfg(test)]
use crate::parser::lexer::tokenize;

#[test]
fn test_parse_sort_struct_basic() {
    let tokens = tokenize("struct a | x(g: Int, h: Bag(FBag(Nat))) | y | z(Int) ? is_z").unwrap();
    let sort = unwrap_result(Parser::new(&tokens).parse_sort());

    let constructors = unwrap_pattern!(sort.value, SortEnum::Struct { constructors } => constructors);
    assert_eq!(constructors.len(), 4);
    assert_eq!(constructors[0].id.get_value(), "a");
    assert_eq!(constructors[0].properties.len(), 0);
    assert!(constructors[0].recognizer_function_id.is_none());

    assert_eq!(constructors[1].id.get_value(), "x");
    assert_eq!(constructors[1].properties[0].0, Some(Identifier::new("g")));
}

#[test]
fn test_parse_sort_struct_empty() {
    // let tokens = tokenize("struct").unwrap();
    // let result = Parser::new(&tokens).parse_sort();
    // assert!(result.is_err());

    let tokens = tokenize("struct a()").unwrap();
    let result = Parser::new(&tokens).parse_sort();
    assert!(result.is_err());
}
