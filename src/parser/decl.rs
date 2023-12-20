//! Implements an mCRL2 model declaration parser.
//! 
//! The functions can be found [here].
//! 
//! [here]: ../parser/struct.Parser.html#method.parse_decl

use crate::ast::decl::{Decl, DeclEnum, VariableDecl, EquationDecl};
use crate::parser::lexer::LexicalElement;
use crate::parser::parser::{ParseError, Parser};

use std::rc::Rc;

impl<'a> Parser<'a> {
    /// Parses a single block of declarations.
    /// 
    /// A block is for instance `map a: Nat; b: Nat;` i.e. a group of
    /// declarations preceded by the same declaration keyword.
    pub fn parse_decl(&mut self) -> Result<Vec<Decl>, ParseError> {
        use LexicalElement::*;

        let mut decls = Vec::new();

        let mut current_decl_type = None;
        while self.has_token() {
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
        let loc = self.get_loc();
        self.skip_if_equal(&LexicalElement::Act);

        // [act] a1, ..., an: Sort;
        let ids = self.parse_identifier_list()?;

        self.expect_token(&LexicalElement::Colon)?;
        let sort = Rc::new(self.parse_sort()?);

        self.expect_token(&LexicalElement::Semicolon)?;

        Ok(Decl::new(DeclEnum::ActionDecl { ids, sort }, loc))
    }

    fn parse_constructor_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_loc();
        self.skip_if_equal(&LexicalElement::Cons);

        // [cons] a1, ..., an: Sort;
        let ids = self.parse_identifier_list()?;

        self.expect_token(&LexicalElement::Colon)?;
        let sort = Rc::new(self.parse_sort()?);

        self.expect_token(&LexicalElement::Semicolon)?;

        Ok(Decl::new(DeclEnum::ConstructorDecl { ids, sort }, loc))
    }

    fn parse_equation_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_loc();

        // parse variables
        let mut variables = Vec::new();
        if self.skip_if_equal(&LexicalElement::Var) {
            while !self.is_token(&LexicalElement::Eqn) {
                let ids = self.parse_identifier_list()?;
                self.expect_token(&LexicalElement::Colon)?;
                let sort = Rc::new(self.parse_sort()?);
                self.expect_token(&LexicalElement::Semicolon)?;
                variables.push(VariableDecl { ids, sort });
            }
        }

        // parse equations
        self.expect_token(&LexicalElement::Eqn)?;
        let mut equations = Vec::new();
        while self.has_token() && !is_decl_keyword(&self.get_token().value) {
            let expr = Rc::new(self.parse_expr()?);
            if self.skip_if_equal(&LexicalElement::Equals) {
                let rhs = Rc::new(self.parse_expr()?);

                equations.push(EquationDecl { condition: None, lhs: expr, rhs });
            } else if self.skip_if_equal(&LexicalElement::Arrow) {
                let lhs = Rc::new(self.parse_expr()?);
                self.expect_token(&LexicalElement::Equals)?;
                let rhs = Rc::new(self.parse_expr()?);

                equations.push(EquationDecl { condition: Some(expr), lhs, rhs });
            } else {
                return Err(ParseError::expected("either = or -> in an 'eqn' declaration", self.get_token()));
            }

            self.expect_token(&LexicalElement::Semicolon)?;
        }

        Ok(Decl::new(DeclEnum::EquationSetDecl { variables, equations }, loc))
    }

    fn parse_global_variable_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_loc();
        self.skip_if_equal(&LexicalElement::Glob);

        let ids = self.parse_identifier_list()?;
        self.expect_token(&LexicalElement::Colon)?;
        let sort = Rc::new(self.parse_sort()?);
        self.expect_token(&LexicalElement::Semicolon)?;

        Ok(Decl::new(DeclEnum::GlobalVariableDecl { ids, sort }, loc))
    }

    fn parse_initial_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_loc();
        self.expect_token(&LexicalElement::Init).unwrap();

        let value = Rc::new(self.parse_proc()?);
        self.expect_token(&LexicalElement::Semicolon)?;

        Ok(Decl::new(DeclEnum::InitialDecl { value }, loc))
    }

    fn parse_map_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_loc();
        self.skip_if_equal(&LexicalElement::Map);

        let id = self.parse_identifier()?;
        self.expect_token(&LexicalElement::Colon)?;
        let sort = Rc::new(self.parse_sort()?);
        self.expect_token(&LexicalElement::Semicolon)?;

        Ok(Decl::new(DeclEnum::MapDecl { id, sort }, loc))
    }

    fn parse_process_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_loc();
        self.skip_if_equal(&LexicalElement::Proc);

        let id = self.parse_identifier()?;

        let mut params = Vec::new();
        if self.skip_if_equal(&LexicalElement::OpeningParen) {
            // (id11, .., id1M: Sort1, ..., idN1, .., idNM: SortN)
            //  ^^^^^^^^^^^^^^^^^^^^^
            //  one element in the vector
            while {
                let ids = self.parse_identifier_list()?;
                self.expect_token(&LexicalElement::Colon)?;
                let sort = Rc::new(self.parse_sort()?);
                params.push(VariableDecl { ids, sort });

                self.skip_if_equal(&LexicalElement::Comma)
            } {}
            self.expect_token(&LexicalElement::ClosingParen)?;
        }
        self.expect_token(&LexicalElement::Equals)?;

        let process = Rc::new(self.parse_proc()?);
        self.expect_token(&LexicalElement::Semicolon)?;

        Ok(Decl::new(DeclEnum::ProcessDecl { id, params, process }, loc))
    }

    fn parse_sort_decl(&mut self) -> Result<Decl, ParseError> {
        let loc = self.get_loc();
        self.skip_if_equal(&LexicalElement::Sort);

        if self.is_next_token(&LexicalElement::Equals) {
            // sort A = B;
            let id = self.parse_identifier()?;
            self.expect_token(&LexicalElement::Equals).unwrap();
            let value = Some(Rc::new(self.parse_sort()?));
            self.expect_token(&LexicalElement::Semicolon)?;

            Ok(Decl::new(DeclEnum::SortDecl { ids: vec![id], value }, loc))
        } else {
            // sort A;
            let ids = self.parse_identifier_list()?;
            self.expect_token(&LexicalElement::Semicolon)?;

            Ok(Decl::new(DeclEnum::SortDecl { ids, value: None }, loc))
        }
    }

    /// Parses a list of variables with types of the form `(id11, ..., id1M:
    /// Sort1, ..., idN1, ..., idNM: SortN)` i.e. parameters can be grouped
    /// together.
    pub fn parse_var_decl_list(&mut self) -> Result<Vec<VariableDecl>, ParseError> {
        let mut result = Vec::new();

        let ids = self.parse_identifier_list()?;
        self.expect_token(&LexicalElement::Colon)?;
        let sort = Rc::new(self.parse_sort()?);
        result.push(VariableDecl { ids, sort });

        while self.skip_if_equal(&LexicalElement::Comma) {
            let ids = self.parse_identifier_list()?;
            self.expect_token(&LexicalElement::Colon)?;
            let sort = Rc::new(self.parse_sort()?);
            result.push(VariableDecl { ids, sort });
        }

        Ok(result)
    }
}

fn is_decl_keyword(element: &LexicalElement) -> bool {
    use LexicalElement::*;

    matches!(element, Sort | Cons | Map | Var | Eqn | Glob | Act | Proc | Init)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::lexer::tokenize;
    use crate::util::unwrap_result;
    use crate::unwrap_pattern;

    #[test]
    fn test_parse_decl_eqn() {
        use crate::core::syntax::Identifier;
        use crate::ast::expr::ExprEnum;
        use crate::ast::sort::SortEnum;

        let tokens = tokenize("
            var y, z, a: Nat;
                b: Set(Int);
            eqn y == z = a in b;
                y == z -> y == z = true;
        ").unwrap();
        let decl = unwrap_result(Parser::new(&tokens).parse_decl());
        assert_eq!(decl.len(), 1);

        let (variables, equations) = unwrap_pattern!(&decl[0].value,
            DeclEnum::EquationSetDecl { variables, equations } => (variables, equations)
        );

        assert_eq!(variables.len(), 2);
        assert_eq!(equations.len(), 2);

        assert_eq!(&variables[0].ids, &vec![Identifier::new("y"), Identifier::new("z"), Identifier::new("a")]);
        unwrap_pattern!(&variables[0].sort.value, &SortEnum::Nat => ());
        assert_eq!(&variables[1].ids, &vec![Identifier::new("b")]);

        // first equation
        let EquationDecl { condition, lhs, rhs } = &equations[0];
        assert!(condition.is_none());
        
        let (equals_lhs, equals_rhs) = unwrap_pattern!(&lhs.value, ExprEnum::Equals { lhs, rhs } => (lhs, rhs));
        let y = unwrap_pattern!(&equals_lhs.value, ExprEnum::Id { id } => id);
        assert_eq!(y.get_value(), "y");
        let z = unwrap_pattern!(&equals_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(z.get_value(), "z");

        let (in_lhs, in_rhs) = unwrap_pattern!(&rhs.value, ExprEnum::In { lhs, rhs } => (lhs, rhs));
        let a = unwrap_pattern!(&in_lhs.value, ExprEnum::Id { id } => id);
        assert_eq!(a.get_value(), "a");
        let b = unwrap_pattern!(&in_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(b.get_value(), "b");

        // second equation
        let EquationDecl { condition, lhs, rhs } = &equations[1];

        let condition = condition.as_ref().unwrap();
        let (equals_lhs, equals_rhs) = unwrap_pattern!(&condition.value, ExprEnum::Equals { lhs, rhs } => (lhs, rhs));
        let y = unwrap_pattern!(&equals_lhs.value, ExprEnum::Id { id } => id);
        assert_eq!(y.get_value(), "y");
        let z = unwrap_pattern!(&equals_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(z.get_value(), "z");

        let (equals_lhs, equals_rhs) = unwrap_pattern!(&lhs.value, ExprEnum::Equals { lhs, rhs } => (lhs, rhs));
        let y = unwrap_pattern!(&equals_lhs.value, ExprEnum::Id { id } => id);
        assert_eq!(y.get_value(), "y");
        let z = unwrap_pattern!(&equals_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(z.get_value(), "z");

        unwrap_pattern!(&rhs.value, ExprEnum::Bool { value } => assert!(value));
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
}
