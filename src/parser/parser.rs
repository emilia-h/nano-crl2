
use crate::core::error::Mcrl2Error;
use crate::ast::decl::{Decl, DeclEnum};
use crate::ast::expr::Expr;
use crate::ast::model::Model;
use crate::ast::sort::Sort;
use crate::core::syntax::Identifier;
use crate::parser::lexer::LexicalElement;
use crate::parser::lexer::Token;

use std::rc::Rc;

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
        // eprintln!("{:?}", &self.tokens);
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
                return Err(ParseError::expected("declaration", token));
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
        self.skip_if_equal(&LexicalElement::Act);

        let loc = self.get_token().loc;
        let identifiers = Vec::new();
        let sort = Rc::new(Sort {});

        Ok(Decl::new(DeclEnum::ActionDecl { identifiers, sort }, loc))
    }

    fn parse_constructor_decl(&mut self) -> Result<Decl, ParseError> {
        self.skip_if_equal(&LexicalElement::Cons);
        todo!()
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
        self.skip_if_equal(&LexicalElement::Init);
        todo!()
    }

    fn parse_map_decl(&mut self) -> Result<Decl, ParseError> {
        self.skip_if_equal(&LexicalElement::Map);
        todo!()
    }

    fn parse_process_decl(&mut self) -> Result<Decl, ParseError> {
        self.skip_if_equal(&LexicalElement::Proc);
        todo!()
    }

    fn parse_sort_decl(&mut self) -> Result<Decl, ParseError> {
        self.skip_if_equal(&LexicalElement::Sort);
        todo!()
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let token = self.get_token();
        if let LexicalElement::Identifier(value) = &token.value {
            Ok(Identifier::new(value))
        } else {
            Err(ParseError::expected("identifier", &token))
        }
    }

    fn get_token(&self) -> &Token {
        assert!(self.has_token());
        &self.tokens[self.index]
    }

    fn has_token(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn skip_token(&mut self) {
        self.index += 1;
    }

    fn skip_if_equal(&mut self, value: &LexicalElement) {
        if &self.get_token().value == value {
            self.skip_token();
        }
    }
}
