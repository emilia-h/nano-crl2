//! Implements an mCRL2 model expression parser.
//! 
//! The functions can be found [here].
//! 
//! [here]: ../parser/struct.Parser.html#method.parse_expr

use crate::ast::expr::{Expr, ExprEnum};
use crate::core::syntax::Identifier;
use crate::parser::lexer::LexicalElement;
use crate::parser::parser::{ParseError, Parser};

use std::rc::Rc;

impl<'a> Parser<'a> {
    /// Parses an `Expr` (short for expression).
    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        // the grammar is unclear but precedence seems to be (low to high):
        // whr
        // forall, exists, lambda
        // => (associative)
        // || (associative)
        // && (associative)
        // ==, != (left associative (?))
        // <, <=, >, >= (left associative (?))
        // in
        // |> (right associative)
        // <| (left associative)
        // ++ (associative)
        // +, - (left associative)
        // /, *, div, mod (left associative)
        // . (left associative)
        // - (negation), !, #
        let loc = self.get_loc();
        let expr = self.parse_binder_expr()?;

        if self.skip_if_equal(&LexicalElement::Whr) {
            let mut assignments = Vec::new();
            loop {
                if self.skip_if_equal(&LexicalElement::End) {
                    break;
                }

                let id = self.parse_identifier()?;
                self.expect_token(&LexicalElement::Equals)?;
                let value = Rc::new(self.parse_expr()?);
                assignments.push((id, value));
            }
            Ok(Expr::new(
                ExprEnum::Where { expr: Rc::new(expr), assignments },
                self.until_now(&loc),
            ))
        } else {
            Ok(expr)
        }
    }

    // forall, exists, lambda
    fn parse_binder_expr(&mut self) -> Result<Expr, ParseError> {
        if !self.has_token() {
            let loc = self.get_last_loc();
            return Err(ParseError::end_of_input("an expression", loc));
        }

        let loc = self.get_loc();

        match self.get_token().value {
            LexicalElement::Exists => {
                self.skip_token();
                let variables = self.parse_var_decl_list()?;
                self.expect_token(&LexicalElement::Period)?;
                let expr = Rc::new(self.parse_implies_expr()?);
                Ok(Expr::new(
                    ExprEnum::Exists { variables, expr },
                    self.until_now(&loc),
                ))
            },
            LexicalElement::Forall => {
                self.skip_token();
                let variables = self.parse_var_decl_list()?;
                self.expect_token(&LexicalElement::Period)?;
                let expr = Rc::new(self.parse_implies_expr()?);
                Ok(Expr::new(
                    ExprEnum::Forall { variables, expr },
                    self.until_now(&loc),
                ))
            },
            LexicalElement::Lambda => {
                self.skip_token();
                let variables = self.parse_var_decl_list()?;
                self.expect_token(&LexicalElement::Period)?;
                let expr = Rc::new(self.parse_implies_expr()?);
                Ok(Expr::new(
                    ExprEnum::Lambda { variables, expr },
                    self.until_now(&loc),
                ))
            },
            _ => {
                self.parse_implies_expr()
            },
        }
    }

    // => (associative to the right, i.e. a => b => c is a => (b => c))
    fn parse_implies_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_right_associative_expr(
            &|parser| parser.parse_or_expr(),
            &LexicalElement::ThickArrow, 
            &|lhs, rhs| ExprEnum::Implies { lhs, rhs }
        )
    }

    // || (associative, so we pretend it associates to the right)
    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_right_associative_expr(
            &|parser| parser.parse_and_expr(),
            &LexicalElement::DoublePipe,
            &|lhs, rhs| ExprEnum::LogicalOr { lhs, rhs },
        )
    }

    // && (associative, so we pretend it associates to the right)
    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_right_associative_expr(
            &|parser| parser.parse_equals_expr(),
            &LexicalElement::DoubleAmpersand,
            &|lhs, rhs| ExprEnum::LogicalAnd { lhs, rhs },
        )
    }

    // ==, != (associates to the left! so a == b != c is (a == b) != c)
    fn parse_equals_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            &|parser| parser.parse_comparison_expr(),
            &[
                (&LexicalElement::DoubleEquals, &|lhs, rhs| ExprEnum::Equals { lhs, rhs }),
                (&LexicalElement::NotEquals, &|lhs, rhs| ExprEnum::NotEquals { lhs, rhs }),
            ],
        )
    }

    // <, <=, >, >= (associate to the left)
    fn parse_comparison_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            &|parser| parser.parse_in_expr(),
            &[
                (&LexicalElement::LessThan, &|lhs, rhs| ExprEnum::LessThan { lhs, rhs }),
                (&LexicalElement::LessThanEquals, &|lhs, rhs| ExprEnum::LessThanEquals { lhs, rhs }),
                (&LexicalElement::GreaterThan, &|lhs, rhs| ExprEnum::GreaterThan { lhs, rhs }),
                (&LexicalElement::GreaterThanEquals, &|lhs, rhs| ExprEnum::GreaterThanEquals { lhs, rhs }),
            ],
        )
    }

    // in (associates to the left)
    fn parse_in_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            &|parser| parser.parse_cons_expr(),
            &[(&LexicalElement::In, &|lhs, rhs| ExprEnum::In { lhs, rhs })],
        )
    }

    // |> (associates to the right)
    fn parse_cons_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_right_associative_expr(
            &|parser| parser.parse_snoc_expr(),
            &LexicalElement::ConsOperator,
            &|lhs, rhs| ExprEnum::Cons { lhs, rhs },
        )
    }

    // <| (associates to the left)
    fn parse_snoc_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            &|parser| parser.parse_concat_expr(),
            &[(&LexicalElement::SnocOperator, &|lhs, rhs| ExprEnum::Snoc { lhs, rhs })],
        )
    }

    // ++ (associative, treat as if it associates to the left)
    fn parse_concat_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            &|parser| parser.parse_add_expr(),
            &[(&LexicalElement::Concat, &|lhs, rhs| ExprEnum::Concat { lhs, rhs })],
        )
    }

    // +, - (associative & associates to the left, respectively)
    fn parse_add_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            &|parser| parser.parse_multiply_expr(),
            &[
                (&LexicalElement::Plus, &|lhs, rhs| ExprEnum::Add { lhs, rhs }),
                (&LexicalElement::Dash, &|lhs, rhs| ExprEnum::Subtract { lhs, rhs }),
            ],
        )
    }

    // *, /, div, mod (associate to the left)
    fn parse_multiply_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            &|parser| parser.parse_index_expr(),
            &[
                (&LexicalElement::Asterisk, &|lhs, rhs| ExprEnum::Multiply { lhs, rhs }),
                (&LexicalElement::Slash, &|lhs, rhs| ExprEnum::Divide { lhs, rhs }),
                (&LexicalElement::Div, &|lhs, rhs| ExprEnum::IntegerDivide { lhs, rhs }),
                (&LexicalElement::Mod, &|lhs, rhs| ExprEnum::Mod { lhs, rhs }),
            ],
        )
    }

    // . (associates to the left)
    fn parse_index_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            &|parser| parser.parse_unit_expr(),
            &[(&LexicalElement::Period, &|lhs, rhs| ExprEnum::Index { lhs, rhs })],
        )
    }

    // - (negation), !, #
    // https://mcrl2.org/web/user_manual/language_reference/process.html#grammar-token-DataExprUnit
    // NOTE: this is slightly different from DataExprUnit in the sense that it
    // allows expressions of the form x[p -> q].
    pub fn parse_unit_expr(&mut self) -> Result<Expr, ParseError> {
        if !self.has_token() {
            let loc = self.get_last_loc();
            return Err(ParseError::end_of_input("an expression", loc));
        }

        let token = self.get_token();
        let loc = token.loc;

        let mut result = match &token.value {
            // NOTE: unary operators should `return`, because expressions like
            // `-f(a)` need to be parsed as `-(f(a))`, not as `(-f)(a)`
            LexicalElement::Dash => {
                self.skip_token();
                let expr = self.parse_unit_expr()?;
                return Ok(Expr::new(
                    ExprEnum::Negate { value: Rc::new(expr) },
                    self.until_now(&loc),
                ));
            },
            LexicalElement::ExclamationMark => {
                self.skip_token();
                let expr = self.parse_unit_expr()?;
                return Ok(Expr::new(
                    ExprEnum::LogicalNot { value: Rc::new(expr) },
                    self.until_now(&loc),
                ))
            },
            LexicalElement::HashSign => {
                self.skip_token();
                let expr = self.parse_unit_expr()?;
                return Ok(Expr::new(
                    ExprEnum::Count { value: Rc::new(expr) },
                    self.until_now(&loc),
                ))
            },
            LexicalElement::True => {
                self.skip_token();
                Expr::new(ExprEnum::Bool { value: true }, loc)
            },
            LexicalElement::False => {
                self.skip_token();
                Expr::new(ExprEnum::Bool { value: false }, loc)
            },
            &LexicalElement::Integer(value) => {
                self.skip_token();
                Expr::new(ExprEnum::Number { value }, loc)
            },
            LexicalElement::Identifier(id) => {
                let id = Identifier::new(id);
                self.skip_token();
                Expr::new(ExprEnum::Id { id }, loc)
            },
            LexicalElement::OpeningBracket => { // list
                self.skip_token();

                if self.skip_if_equal(&LexicalElement::ClosingBracket) {
                    Expr::new(
                        ExprEnum::List { values: Vec::new() },
                        self.until_now(&loc),
                    )
                } else {
                    let values = self.parse_expr_list()?;
                    self.expect_token(&LexicalElement::ClosingBracket)?;
                    Expr::new(ExprEnum::List { values }, self.until_now(&loc))
                }
            },
            LexicalElement::OpeningBrace => { // set or bag
                self.skip_token();

                if self.skip_if_equal(&LexicalElement::Colon) {
                    // {:}
                    self.expect_token(&LexicalElement::ClosingBrace)?;
                    Expr::new(
                        ExprEnum::Bag { values: vec![] },
                        self.until_now(&loc),
                    )
                } else if self.skip_if_equal(&LexicalElement::ClosingBrace) {
                    // {}
                    Expr::new(
                        ExprEnum::Set { values: vec![] },
                        self.until_now(&loc),
                    )
                } else if self.is_next_token(&LexicalElement::Colon) {
                    // { a: B | ... } (set comprehension) or
                    // { a: 1, b: 2, ... } (bag)
                    todo!()
                    // let id = self.parse_identifier()?;
                    // self.expect_token(&LexicalElement::Colon).unwrap();
                    // let sort = Rc::new(self.parse_sort()?);
                    // self.expect_token(&LexicalElement::Pipe)?;
                    // let expr = Rc::new(self.parse_expr()?);
                    // Expr::new(
                    //     ExprEnum::SetComprehension { id, sort, expr },
                    //     self.until_now(&loc),
                    // )
                } else {
                    // { a, ... } (set) or { a + b : 1, ... } (bag)
                    let expr = Rc::new(self.parse_expr()?);
                    if self.skip_if_equal(&LexicalElement::Colon) { // bag
                        todo!()
                    } else { // set
                        let mut values = vec![expr];
                        while self.skip_if_equal(&LexicalElement::Comma) {
                            values.push(Rc::new(self.parse_expr()?));
                        }
                        self.expect_token(&LexicalElement::ClosingBrace)?;
                        Expr::new(ExprEnum::Set { values }, self.until_now(&loc))
                    }
                }
            },
            LexicalElement::OpeningParen => {
                self.skip_token();
                let mut expr = self.parse_expr()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                expr.loc = self.until_now(&loc);
                expr
            },
            LexicalElement::Exists | LexicalElement::Forall | LexicalElement::Lambda => {
                self.parse_binder_expr()?
            },
            _ => {
                return Err(ParseError::expected("an expression", token));
            },
        };

        // function update, function apply
        loop {
            if self.skip_if_equal(&LexicalElement::OpeningParen) {
                let args = self.parse_expr_list()?;
                self.expect_token(&LexicalElement::ClosingParen)?;

                result = Expr::new(
                    ExprEnum::Apply { callee: Rc::new(result), args },
                    self.until_now(&loc),
                );
            } else if self.skip_if_equal(&LexicalElement::OpeningBracket) {
                let lhs = Rc::new(self.parse_expr()?);
                self.expect_token(&LexicalElement::Arrow)?;
                let rhs = Rc::new(self.parse_expr()?);
                self.expect_token(&LexicalElement::ClosingBracket)?;

                result = Expr::new(ExprEnum::FunctionUpdate {
                    function: Rc::new(result),
                    lhs, rhs,
                }, self.until_now(&loc));
            } else {
                break Ok(result);
            }
        }
    }

    /// Parses comma-separated list of expressions
    pub fn parse_expr_list(&mut self) -> Result<Vec<Rc<Expr>>, ParseError> {
        let mut result = Vec::new();
        result.push(Rc::new(self.parse_expr()?));
        while self.skip_if_equal(&LexicalElement::Comma) {
            result.push(Rc::new(self.parse_expr()?));
        }
        Ok(result)
    }

    // generic function for parsing left-associative expressions such as for -,
    // ++, <|, ., etc.
    fn parse_left_associative_expr<F>(
        &mut self,
        sub_parser: &F,
        options: &[(&LexicalElement, &dyn Fn(Rc<Expr>, Rc<Expr>) -> ExprEnum)],
    ) -> Result<Expr, ParseError>
    where
        F: Fn(&mut Parser<'a>) -> Result<Expr, ParseError>
    {
        let loc = self.get_loc();
        let mut result = sub_parser(self)?;

        'outer: loop {
            for (lexical_element, constructor) in options {
                if self.skip_if_equal(lexical_element) {
                    let rhs = Rc::new(sub_parser(self)?);
                    result = Expr::new(
                        constructor(Rc::new(result), rhs),
                        self.until_now(&loc),
                    );
                    continue 'outer;
                }
            }

            break Ok(result);
        }
    }

    fn parse_right_associative_expr<F>(
        &mut self,
        sub_parser: &F,
        lexical_element: &LexicalElement,
        constructor: &dyn Fn(Rc<Expr>, Rc<Expr>) -> ExprEnum,
    ) -> Result<Expr, ParseError>
    where
        F: Fn(&mut Parser<'a>) -> Result<Expr, ParseError>
    {
        let loc = self.get_loc();
        let lhs = sub_parser(self)?;

        if self.skip_if_equal(lexical_element) {
            let rhs = Rc::new(self.parse_right_associative_expr(
                sub_parser,
                lexical_element,
                constructor,
            )?);

            Ok(Expr::new(
                constructor(Rc::new(lhs), rhs),
                self.until_now(&loc),
            ))
        } else {
            Ok(lhs)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unwrap_pattern;
    use crate::parser::lexer::tokenize;

    #[test]
    fn test_parse_expr_binary() {
        let tokens = tokenize("a + b + c").unwrap();
        let expr = Parser::new(&tokens).parse_expr().unwrap();

        let (lhs, rhs) = unwrap_pattern!(expr.value, ExprEnum::Add { lhs, rhs } => (lhs, rhs));

        let rhs_id = unwrap_pattern!(&rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(rhs_id.get_value(), "c");

        let (llhs, rlhs) = unwrap_pattern!(&lhs.value, ExprEnum::Add { lhs, rhs } => (lhs, rhs));
        let llhs_id = unwrap_pattern!(&llhs.value, ExprEnum::Id { id } => id);
        assert_eq!(llhs_id.get_value(), "a");
        let rlhs_id = unwrap_pattern!(&rlhs.value, ExprEnum::Id { id } => id);
        assert_eq!(rlhs_id.get_value(), "b");

        // ((((((af321 + fa123) mod de) / a) + (b * c)) |> (([1] ++ [3, 5]) <| 4)
        let tokens = tokenize("(af321 + fa123) mod de / a + b * c |> [1] ++ [3, 5] <| 4").unwrap();
        let expr = Parser::new(&tokens).parse_expr().unwrap();

        let (cons_lhs, cons_rhs) = unwrap_pattern!(expr.value, ExprEnum::Cons { lhs, rhs } => (lhs, rhs));

        let (add_lhs, add_rhs) = unwrap_pattern!(&cons_lhs.value, ExprEnum::Add { lhs, rhs } => (lhs, rhs));

        let (bc_lhs, bc_rhs) = unwrap_pattern!(&add_rhs.value, ExprEnum::Multiply { lhs, rhs } => (lhs, rhs));
        let b = unwrap_pattern!(&bc_lhs.value, ExprEnum::Id { id } => id);
        assert_eq!(b.get_value(), "b");
        let c = unwrap_pattern!(&bc_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(c.get_value(), "c");

        let (div_lhs, div_rhs) = unwrap_pattern!(&add_lhs.value, ExprEnum::Divide { lhs, rhs } => (lhs, rhs));

        let a = unwrap_pattern!(&div_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(a.get_value(), "a");

        let (mod_lhs, mod_rhs) = unwrap_pattern!(&div_lhs.value, ExprEnum::Mod { lhs, rhs } => (lhs, rhs));

        let de = unwrap_pattern!(&mod_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(de.get_value(), "de");

        let (add_lhs, add_rhs) = unwrap_pattern!(&mod_lhs.value, ExprEnum::Add { lhs, rhs } => (lhs, rhs));
        let af321 = unwrap_pattern!(&add_lhs.value, ExprEnum::Id { id } => id);
        assert_eq!(af321.get_value(), "af321");
        let fa123 = unwrap_pattern!(&add_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(fa123.get_value(), "fa123");

        let (snoc_lhs, snoc_rhs) = unwrap_pattern!(&cons_rhs.value, ExprEnum::Snoc { lhs, rhs } => (lhs, rhs));

        let number = unwrap_pattern!(snoc_rhs.value, ExprEnum::Number { value } => value);
        assert_eq!(number, 4);

        let (concat_lhs, concat_rhs) = unwrap_pattern!(&snoc_lhs.value, ExprEnum::Concat { lhs, rhs } => (lhs, rhs));

        let values = unwrap_pattern!(&concat_lhs.value, ExprEnum::List { values } => values);
        assert_eq!(values.len(), 1);
        let value1 = unwrap_pattern!(&values[0].value, ExprEnum::Number { value } => value);
        assert_eq!(value1, &1);
        let values = unwrap_pattern!(&concat_rhs.value, ExprEnum::List { values } => values);
        assert_eq!(values.len(), 2);
        let value1 = unwrap_pattern!(&values[0].value, ExprEnum::Number { value } => value);
        assert_eq!(value1, &3);
        let value2 = unwrap_pattern!(&values[1].value, ExprEnum::Number { value } => value);
        assert_eq!(value2, &5);
    }

    #[test]
    fn test_parse_expr_logical() {
        // ((a < b) == c) || ((d == e) && f)
        let tokens = tokenize("a < b == c || d == e && f").unwrap();
        let expr = Parser::new(&tokens).parse_expr().unwrap();

        eprintln!("{:#?}", expr);
        let (or_lhs, or_rhs) = unwrap_pattern!(&expr.value, ExprEnum::LogicalOr { lhs, rhs } => (lhs, rhs));
        let (and_lhs, and_rhs) = unwrap_pattern!(&or_rhs.value, ExprEnum::LogicalAnd { lhs, rhs } => (lhs, rhs));

        let (equals_lhs, equals_rhs) = unwrap_pattern!(&or_lhs.value, ExprEnum::Equals { lhs, rhs } => (lhs, rhs));
        let (lt_lhs, lt_rhs) = unwrap_pattern!(&equals_lhs.value, ExprEnum::LessThan { lhs, rhs } => (lhs, rhs));
        let a = unwrap_pattern!(&lt_lhs.value, ExprEnum::Id { id } => id);
        assert_eq!(a.get_value(), "a");
        let b = unwrap_pattern!(&lt_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(b.get_value(), "b");
        let c = unwrap_pattern!(&equals_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(c.get_value(), "c");

        let (equals_lhs, equals_rhs) = unwrap_pattern!(&and_lhs.value, ExprEnum::Equals { lhs, rhs } => (lhs, rhs));
        let d = unwrap_pattern!(&equals_lhs.value, ExprEnum::Id { id } => id);
        assert_eq!(d.get_value(), "d");
        let e = unwrap_pattern!(&equals_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(e.get_value(), "e");
        let f = unwrap_pattern!(&and_rhs.value, ExprEnum::Id { id } => id);
        assert_eq!(f.get_value(), "f");
    }
}
