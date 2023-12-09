
use crate::core::error::Mcrl2Error;
use crate::ast::decl::{Decl, DeclEnum};
use crate::ast::expr::{Expr, ExprEnum};
use crate::ast::model::Model;
use crate::ast::proc::{Proc, ProcEnum};
use crate::ast::sort::{Sort, SortEnum, Constructor};
use crate::core::syntax::Identifier;
use crate::parser::lexer::LexicalElement;
use crate::parser::lexer::Token;

use std::rc::Rc;

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub character: usize,
}

impl ParseError {
    pub fn expected(expectation: &str, token: &Token) -> Self {
        let mut message = String::from("expected ");
        message.push_str(expectation);
        message.push_str(" but found ");
        message.push_str(&format!("{}", token.value));
        ParseError {
            message,
            line: token.loc.get_line(),
            character: token.loc.get_char(),
        }
    }
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
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, index: 0 }
    }

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

    pub fn parse_decl(&mut self) -> Result<Vec<Decl>, ParseError> {
        use LexicalElement::*;

        let mut decls = Vec::new();

        let mut current_decl_type = None;
        loop {
            let token = self.get_token();
            if let Some(new_decl_type) = match &token.value {
                Var => Some(Eqn),
                elem@(Act | Cons | Eqn | Glob | Init | Map | Proc | Sort) => Some(elem.clone()),
                _ => None,
            } {
                if current_decl_type.is_some() {
                    break; // next declaration "group" started, so we're done
                }

                current_decl_type = Some(new_decl_type);
            }
            if current_decl_type.is_none() {
                // got some completely other token that a declaration cannot start with
                return Err(ParseError::expected("a declaration", token));
            }

            self.skip_token();

            decls.push(match current_decl_type.as_ref().unwrap() {
                Act => self.parse_action_decl()?,
                Cons => self.parse_constructor_decl()?,
                Eqn => self.parse_equation_decl()?,
                Glob => self.parse_global_variable_decl()?,
                Init => self.parse_initial_decl()?,
                Map => self.parse_map_decl()?,
                Proc => self.parse_process_decl()?,
                Sort => self.parse_sort_decl()?,
                _ => unreachable!(),
            });
        }

        Ok(decls)
    }

    fn parse_action_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_token().loc;
        self.skip_if_equal(&LexicalElement::Act);

        // [act] a1, ..., an: Sort;
        let ids = self.parse_identifier_list()?;

        self.expect_token(&LexicalElement::Colon)?;
        let sort = Rc::new(self.parse_sort()?);

        self.expect_token(&LexicalElement::Semicolon)?;

        Ok(Decl::new(DeclEnum::ActionDecl { ids, sort }, loc))
    }

    fn parse_constructor_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_token().loc;
        self.skip_if_equal(&LexicalElement::Cons);

        // [cons] a1, ..., an: Sort;
        let ids = self.parse_identifier_list()?;

        self.expect_token(&LexicalElement::Colon)?;
        let sort = Rc::new(self.parse_sort()?);

        self.expect_token(&LexicalElement::Semicolon)?;

        Ok(Decl::new(DeclEnum::ConstructorDecl { ids, sort }, loc))
    }

    fn parse_equation_decl(&mut self) -> Result<Decl, ParseError> {
        //...
        todo!()
    }

    fn parse_global_variable_decl(&mut self) -> Result<Decl, ParseError> {
        self.skip_if_equal(&LexicalElement::Glob);
        todo!()
    }

    fn parse_initial_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_token().loc;
        self.expect_token(&LexicalElement::Init).unwrap();

        let value = Rc::new(self.parse_expr()?);
        Ok(Decl::new(DeclEnum::InitialDecl { value }, loc))
    }

    fn parse_map_decl(&mut self) -> Result<Decl, ParseError> {
        self.skip_if_equal(&LexicalElement::Map);
        todo!()
    }

    fn parse_process_decl(&mut self) -> Result<Decl, ParseError> {
        self.skip_if_equal(&LexicalElement::Proc);
        todo!()
    }

    // sort A = B;
    // sort A;
    fn parse_sort_decl(&mut self) -> Result<Decl, ParseError> {
        self.skip_if_equal(&LexicalElement::Sort);

        let loc = self.get_token().loc;

        if self.is_next_token(&LexicalElement::Equals) {
            let id = self.parse_identifier()?;
            self.expect_token(&LexicalElement::Equals).unwrap();
            Ok(Decl::new(DeclEnum::SortDecl { id }, loc))
        } else {
            todo!()
        }
    }

    // x1: S1, ..., xn: Sn
    pub fn parse_var_decl_list(&mut self) -> Result<Vec<(Identifier, Rc<Sort>)>, ParseError> {
        let mut result = Vec::new();

        let id = self.parse_identifier()?;
        self.expect_token(&LexicalElement::Colon)?;
        let sort = Rc::new(self.parse_sort()?);
        result.push((id, sort));

        while self.skip_if_equal(&LexicalElement::Comma) {
            let id = self.parse_identifier()?;
            self.expect_token(&LexicalElement::Colon)?;
            let sort = Rc::new(self.parse_sort()?);
            result.push((id, sort));
        }

        Ok(result)
    }

    pub fn parse_proc(&mut self) -> Result<Proc, ParseError> {
        todo!()
    }

    pub fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let token = self.get_token();
        let result = if let LexicalElement::Identifier(value) = &token.value {
            Ok(Identifier::new(value))
        } else {
            Err(ParseError::expected("an identifier", &token))
        };
        self.skip_token();
        result
    }

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

    pub fn get_next_token(&self) -> Option<&Token> {
        assert!(self.has_token());
        self.tokens.get(self.index + 1)
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

#[test]
fn test_parse_decl_sort_too_much() {
    // let tokens = tokenize("sort A = struct a b;").unwrap();
    // let result = Parser::new(&tokens).parse_decl();
    // assert!(result.is_err());

    // let tokens = tokenize("sort B = struct a ? b c;").unwrap();
    // let result = Parser::new(&tokens).parse_decl();
    // assert!(result.is_err());

    // let tokens = tokenize("sort C = struct a(id) b;").unwrap();
    // let result = Parser::new(&tokens).parse_decl();
    // assert!(result.is_err());
}
