
use crate::core::error::Mcrl2Error;
use crate::ast::decl::{Decl, DeclEnum};
use crate::ast::expr::{Expr, ExprEnum};
use crate::ast::model::Model;
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
    fn parse_var_decl_list(&mut self) -> Result<Vec<(Identifier, Rc<Sort>)>, ParseError> {
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

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        // the grammar is unclear but precedence seems to be (low to high):
        // whr
        // forall, exists, lambda
        // =>
        // ||
        // &&
        // ==, != (left associative (?))
        // <, <=, >, >= (left associative (?))
        // in
        // |>, <|, ++ (left associative)
        // +, -
        // /, *, div, mod (left associative)
        // .
        // - (negation), !, #
        let loc = self.get_token().loc;
        let expr = self.parse_binder_expr()?;

        if self.skip_if_equal(&LexicalElement::Whr) {
            todo!()
        } else {
            Ok(expr)
        }
    }

    // forall, exists, lambda
    fn parse_binder_expr(&mut self) -> Result<Expr, ParseError> {
        let loc = self.get_token().loc;

        match self.get_token().value {
            LexicalElement::Exists => {
                self.skip_token();
                let ids = self.parse_var_decl_list()?;
                self.expect_token(&LexicalElement::Period)?;
                let expr = Rc::new(self.parse_implies_expr()?);
                Ok(Expr::new(ExprEnum::Exists { ids, expr }, loc))
            },
            LexicalElement::Forall => {
                self.skip_token();
                let ids = self.parse_var_decl_list()?;
                self.expect_token(&LexicalElement::Period)?;
                let expr = Rc::new(self.parse_implies_expr()?);
                Ok(Expr::new(ExprEnum::Forall { ids, expr }, loc))
            },
            LexicalElement::Lambda => {
                self.skip_token();
                let ids = self.parse_var_decl_list()?;
                self.expect_token(&LexicalElement::Period)?;
                let expr = Rc::new(self.parse_implies_expr()?);
                Ok(Expr::new(ExprEnum::Lambda { ids, expr }, loc))
            },
            _ => {
                self.parse_implies_expr()
            },
        }
    }

    // =>
    fn parse_implies_expr(&mut self) -> Result<Expr, ParseError> {
        let loc = self.get_token().loc;
        let expr = self.parse_or_expr()?;

        todo!()
    }

    // ||
    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    // &&
    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    // ==, !=
    fn parse_equals_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    // <, <=, >, >=
    fn parse_comparison_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    fn parse_in_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    // |>, <|, ++
    fn parse_concat_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    // +, -
    fn parse_add_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    // *, /, div, mod
    fn parse_multiply_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    fn parse_index_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    // - (negation), !, #
    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    fn parse_basic_expr(&mut self) -> Result<Expr, ParseError> {
        let token = self.get_token();
        let loc = token.loc;

        let expr = match &token.value {
            LexicalElement::True => {
                self.skip_token();
                Expr::new(ExprEnum::Bool { value: true }, loc)
            },
            LexicalElement::False => {
                self.skip_token();
                Expr::new(ExprEnum::Bool { value: false }, loc)
            },
            LexicalElement::Integer(value) => {
                self.skip_token();
                todo!()
            },
            LexicalElement::Identifier(id) => {
                let id = Identifier::new(id);
                self.skip_token();
                Expr::new(ExprEnum::Id { id }, loc)
            }
            LexicalElement::OpeningBracket => { // list
                self.skip_token();
                todo!()
            },
            LexicalElement::OpeningBrace => { // set or bag
                self.skip_token();
                todo!()
            },
            LexicalElement::OpeningParen => {
                self.skip_token();
                let expr = self.parse_expr()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                expr
            },
            LexicalElement::Exists | LexicalElement::Forall | LexicalElement::Lambda => {
                self.parse_binder_expr()?
            },
            _ => {
                return Err(ParseError::expected("expectation", token));
            },
        };

        // TODO: function update, function apply

        Ok(expr)
    }

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
                let constructor_list = self.parse_constructor_list()?;
                Ok(Sort::new(SortEnum::Struct { constructor_list }, loc))
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

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let token = self.get_token();
        let result = if let LexicalElement::Identifier(value) = &token.value {
            Ok(Identifier::new(value))
        } else {
            Err(ParseError::expected("an identifier", &token))
        };
        self.skip_token();
        result
    }

    fn parse_identifier_list(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut ids = Vec::new();
        ids.push(self.parse_identifier()?);
        while self.skip_if_equal(&LexicalElement::Comma) {
            ids.push(self.parse_identifier()?);
        }
        Ok(ids)
    }

    // helper functions //

    fn get_token(&self) -> &Token {
        assert!(self.has_token());
        &self.tokens[self.index]
    }

    fn get_next_token(&self) -> Option<&Token> {
        assert!(self.has_token());
        self.tokens.get(self.index + 1)
    }

    fn is_token(&self, e: &LexicalElement) -> bool {
        self.has_token() && &self.get_token().value == e
    }

    fn is_next_token(&self, e: &LexicalElement) -> bool {
        if let Some(x) = self.tokens.get(self.index + 1) {
            &x.value == e
        } else {
            false
        }
    }

    fn has_token(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn skip_token(&mut self) {
        self.index += 1;
    }

    fn expect_token(&mut self, value: &LexicalElement) -> Result<(), ParseError> {
        let token = self.get_token();
        if &token.value == value {
            self.skip_token();
            Ok(())
        } else {
            Err(ParseError::expected(&format!("{}", value), token))
        }
    }

    fn skip_if_equal(&mut self, value: &LexicalElement) -> bool {
        if self.has_token() && &self.get_token().value == value {
            self.skip_token();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
use crate::parser::lexer::tokenize;

#[cfg(test)]
fn unwrap_result<T>(x: Result<T, ParseError>) -> T {
    match x {
        Ok(result) => result,
        Err(err) => panic!("{:?}", err),
    }
}

#[test]
fn test_parse_sort_struct_basic() {
    let tokens = tokenize("struct a | x(g: Int, h: Bag(FBag(Nat))) | y | z(Int) ? is_z").unwrap();
    let sort = unwrap_result(Parser::new(&tokens).parse_sort());
    if let SortEnum::Struct { constructor_list } = sort.value {
        assert_eq!(constructor_list.len(), 4);
        assert_eq!(constructor_list[0].id.get_value(), "a");
        assert_eq!(constructor_list[0].properties.len(), 0);
        assert!(constructor_list[0].recognizer_function_id.is_none());

        assert_eq!(constructor_list[1].id.get_value(), "x");
        assert_eq!(constructor_list[1].properties[0].0, Some(Identifier::new("g")));
    } else {
        panic!();
    }
}

#[test]
fn test_parse_sort_struct_empty() {
    let tokens = tokenize("struct").unwrap();
    // let result = Parser::new(&tokens).parse_sort();
    // assert!(result.is_err());

    let tokens = tokenize("struct a()").unwrap();
    let result = Parser::new(&tokens).parse_sort();
    assert!(result.is_err());
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
