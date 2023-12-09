//! Implements an mCRL2 model declaration parser.
//! 
//! The functions can be found [here].
//! 
//! [here]: ../parser/struct.Parser.html#method.parse_decl

use crate::ast::decl::{Decl, DeclEnum};
use crate::ast::sort::Sort;
use crate::core::syntax::Identifier;
use crate::parser::lexer::LexicalElement;
use crate::parser::parser::{ParseError, Parser};

use std::rc::Rc;

impl<'a> Parser<'a> {
    /// Parses a declaration.
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

    /// Parses a list of variables with types of the form `name1: Sort1, ...,
    /// nameN: SortN`.
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
}
