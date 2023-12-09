
use crate::ast::expr::{Expr, ExprEnum};
use crate::core::syntax::Identifier;
use crate::parser::lexer::LexicalElement;
use crate::parser::parser::{ParseError, Parser};

use std::rc::Rc;

impl<'a> Parser<'a> {
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

    // => (associative to the right, i.e. a => b => c is a => (b => c))
    fn parse_implies_expr(&mut self) -> Result<Expr, ParseError> {
        let loc = self.get_token().loc;
        let lhs = self.parse_or_expr()?;

        if self.skip_if_equal(&LexicalElement::ThickArrow) {
            let rhs = Rc::new(self.parse_implies_expr()?);
            Ok(Expr::new(ExprEnum::Implies { lhs: Rc::new(lhs), rhs }, loc))
        } else {
            Ok(lhs)
        }
    }

    // || (associative, so we pretend it associates to the right)
    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        let loc = self.get_token().loc;
        let lhs = self.parse_and_expr()?;

        if self.skip_if_equal(&LexicalElement::LogicalOr) {
            let rhs = Rc::new(self.parse_or_expr()?);
            Ok(Expr::new(ExprEnum::LogicalOr { lhs: Rc::new(lhs), rhs }, loc))
        } else {
            Ok(lhs)
        }
    }

    // && (associative, so we pretend it associates to the right)
    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        let loc = self.get_token().loc;
        let lhs = self.parse_equals_expr()?;

        if self.skip_if_equal(&LexicalElement::LogicalOr) {
            let rhs = Rc::new(self.parse_and_expr()?);
            Ok(Expr::new(ExprEnum::LogicalOr { lhs: Rc::new(lhs), rhs }, loc))
        } else {
            Ok(lhs)
        }
    }

    // ==, != (associates to the left! so a == b != c is (a == b) != c)
    fn parse_equals_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            |parser| parser.parse_comparison_expr(),
            &[
                (&LexicalElement::Equals, &|lhs, rhs| ExprEnum::Equals { lhs, rhs }),
                (&LexicalElement::NotEquals, &|lhs, rhs| ExprEnum::NotEquals { lhs, rhs }),
            ],
        )
    }

    // <, <=, >, >= (associate to the left)
    fn parse_comparison_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            |parser| parser.parse_in_expr(),
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
            |parser| parser.parse_concat_expr(),
            &[(&LexicalElement::In, &|lhs, rhs| ExprEnum::In { lhs, rhs })],
        )
    }

    // |>, <|, ++ (associate to the left)
    fn parse_concat_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            |parser| parser.parse_add_expr(),
            &[
                (&LexicalElement::ConsOperator, &|lhs, rhs| ExprEnum::Cons { lhs, rhs }),
                (&LexicalElement::SnocOperator, &|lhs, rhs| ExprEnum::Snoc { lhs, rhs }),
                (&LexicalElement::Concat, &|lhs, rhs| ExprEnum::Concat { lhs, rhs }),
            ],
        )
    }

    // +, - (associative & associates to the left, respectively)
    fn parse_add_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            |parser| parser.parse_multiply_expr(),
            &[
                (&LexicalElement::Plus, &|lhs, rhs| ExprEnum::Add { lhs, rhs }),
                (&LexicalElement::Dash, &|lhs, rhs| ExprEnum::Subtract { lhs, rhs }),
            ],
        )
    }

    // *, /, div, mod (associate to the left)
    fn parse_multiply_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_left_associative_expr(
            |parser| parser.parse_index_expr(),
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
            |parser| parser.parse_unary_expr(),
            &[(&LexicalElement::Period, &|lhs, rhs| ExprEnum::Index { lhs, rhs })],
        )
    }

    // - (negation), !, #
    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        let loc = self.get_token().loc;

        if self.skip_if_equal(&LexicalElement::Dash) {
            let expr = self.parse_unary_expr()?;
            Ok(Expr::new(ExprEnum::Negate { value: Rc::new(expr) }, loc))
        } else if self.skip_if_equal(&LexicalElement::ExclamationMark) {
            let expr = self.parse_unary_expr()?;
            Ok(Expr::new(ExprEnum::LogicalNot { value: Rc::new(expr) }, loc))
        } else if self.skip_if_equal(&LexicalElement::HashSign) {
            let expr = self.parse_unary_expr()?;
            Ok(Expr::new(ExprEnum::Count { value: Rc::new(expr) }, loc))
        } else {
            self.parse_basic_expr()
        }
    }

    fn parse_basic_expr(&mut self) -> Result<Expr, ParseError> {
        let token = self.get_token();
        let loc = token.loc;

        let mut result = match &token.value {
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
            }
            LexicalElement::OpeningBracket => { // list
                self.skip_token();

                if self.skip_if_equal(&LexicalElement::ClosingBracket) {
                    Expr::new(ExprEnum::List { values: Vec::new() }, loc)
                } else {
                    let result = Expr::new(ExprEnum::List { values: self.parse_expr_list()? }, loc);
                    self.expect_token(&LexicalElement::ClosingBracket)?;
                    result
                }
            },
            LexicalElement::OpeningBrace => { // set or bag
                self.skip_token();

                if self.skip_if_equal(&LexicalElement::ClosingBrace) {
                    Expr::new(ExprEnum::Bag {}, loc)
                } else {
                    todo!()
                }
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

        // function update, function apply
        loop {
            if self.skip_if_equal(&LexicalElement::OpeningParen) {
                let args = self.parse_expr_list()?;
                self.expect_token(&LexicalElement::ClosingParen)?;

                result = Expr::new(ExprEnum::Apply { callee: Rc::new(result), args }, loc);
            } else if self.skip_if_equal(&LexicalElement::OpeningBracket) {
                let lhs = Rc::new(self.parse_expr()?);
                self.expect_token(&LexicalElement::Arrow)?;
                let rhs = Rc::new(self.parse_expr()?);
                self.expect_token(&LexicalElement::ClosingBracket)?;

                result = Expr::new(ExprEnum::FunctionUpdate {
                    function: Rc::new(result),
                    lhs,
                    rhs
                }, loc);
            } else {
                break Ok(result);
            }
        }
    }

    fn parse_expr_list(&mut self) -> Result<Vec<Rc<Expr>>, ParseError> {
        let mut result = Vec::new();
        result.push(Rc::new(self.parse_expr()?));
        while self.skip_if_equal(&LexicalElement::Comma) {
            result.push(Rc::new(self.parse_expr()?));
        }
        Ok(result)
    }

    // generic function for parsing left-associative expressions such as for -,
    // ++, |>, ., etc.
    fn parse_left_associative_expr<F>(
        &mut self,
        mut sub_parser: F,
        options: &[(&LexicalElement, &dyn Fn(Rc<Expr>, Rc<Expr>) -> ExprEnum)],
    ) -> Result<Expr, ParseError>
    where
        F: FnMut(&mut Parser<'a>) -> Result<Expr, ParseError>
    {
        let loc = self.get_token().loc;
        let mut result = sub_parser(self)?;

        'outer: loop {
            for (lexical_element, constructor) in options {
                if self.skip_if_equal(lexical_element) {
                    let rhs = Rc::new(sub_parser(self)?);
                    result = Expr::new(constructor(Rc::new(result), rhs), loc);
                    continue 'outer;
                }
            }

            break Ok(result);
        }
    }
}

#[cfg(test)]
use crate::unwrap_pattern;
#[cfg(test)]
use crate::util::unwrap_result;
#[cfg(test)]
use crate::parser::lexer::tokenize;

#[test]
fn test_parse_expr_binary() {
    let tokens = tokenize("a + b + c").unwrap();
    let expr = unwrap_result(Parser::new(&tokens).parse_expr());

    let (lhs, rhs) = unwrap_pattern!(expr.value, ExprEnum::Add { lhs, rhs } => (lhs, rhs));

    let rhs_id = unwrap_pattern!(&rhs.value, ExprEnum::Id { id } => id);
    assert_eq!(rhs_id.get_value(), "c");

    let (llhs, rlhs) = unwrap_pattern!(&lhs.value, ExprEnum::Add { lhs, rhs } => (lhs, rhs));
    let llhs_id = unwrap_pattern!(&llhs.value, ExprEnum::Id { id } => id);
    assert_eq!(llhs_id.get_value(), "a");
    let rlhs_id = unwrap_pattern!(&rlhs.value, ExprEnum::Id { id } => id);
    assert_eq!(rlhs_id.get_value(), "b");

    // ((((((af321 + fa123) mod de) / a) + (b * c)) |> [1]) ++ [3, 5]) <| 4
    let tokens = tokenize("(af321 + fa123) mod de / a + b * c |> [1] ++ [3, 5] <| 4").unwrap();
    let expr = unwrap_result(Parser::new(&tokens).parse_expr());

    let (expr, snoc_rhs) = unwrap_pattern!(expr.value, ExprEnum::Snoc { lhs, rhs } => (lhs, rhs));

    let number = unwrap_pattern!(snoc_rhs.value, ExprEnum::Number { value } => value);
    assert_eq!(number, 4);

    let (expr, concat_rhs) = unwrap_pattern!(&expr.value, ExprEnum::Concat { lhs, rhs } => (lhs, rhs));

    let values = unwrap_pattern!(&concat_rhs.value, ExprEnum::List { values } => values);
    assert_eq!(values.len(), 2);
    let value1 = unwrap_pattern!(&values[0].value, ExprEnum::Number { value } => value);
    assert_eq!(value1, &3);
    let value2 = unwrap_pattern!(&values[1].value, ExprEnum::Number { value } => value);
    assert_eq!(value2, &5);

    let (expr, cons_rhs) = unwrap_pattern!(&expr.value, ExprEnum::Cons { lhs, rhs } => (lhs, rhs));

    let values = unwrap_pattern!(&cons_rhs.value, ExprEnum::List { values } => values);
    assert_eq!(values.len(), 1);
    let value1 = unwrap_pattern!(&values[0].value, ExprEnum::Number { value } => value);
    assert_eq!(value1, &1);

    let (add_lhs, add_rhs) = unwrap_pattern!(&expr.value, ExprEnum::Add { lhs, rhs } => (lhs, rhs));

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
}