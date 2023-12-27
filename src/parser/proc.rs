//! Implements an mCRL2 model process parser.
//! 
//! The functions can be found [here].
//! 
//! [here]: ../parser/struct.Parser.html#method.parse_proc

use crate::ast::proc::{Action, CommMapping, Proc, ProcEnum, RenameMapping};
use crate::core::syntax::Identifier;
use crate::parser::lexer::LexicalElement;
use crate::parser::parser::{ParseError, Parser};

use std::rc::Rc;

impl<'a> Parser<'a> {
    /// Parses a `Proc` (short for process).
    /// 
    /// # See also
    /// The [mCRL2 grammar on this].
    /// 
    /// [mCRL2 grammar on this]: https://mcrl2.org/web/user_manual/language_reference/process.html#grammar-token-ProcExpr
    pub fn parse_proc(&mut self) -> Result<Proc, ParseError> {
        if !self.has_token() {
            let loc = self.get_last_loc();
            return Err(ParseError::end_of_input("a process", loc));
        }

        // + (associative, so treat it as if it associates to the right)
        self.parse_right_associative_proc(
            &|parser| parser.parse_parallel_proc(),
            &LexicalElement::Plus,
            &|lhs, rhs| ProcEnum::Add { lhs, rhs },
        )
    }

    /// Parses a multi-action of the form `tau` or `a(...) | b | ... | c(...)`.
    /// 
    /// # See also
    /// The [mCRL2 grammar on this].
    /// 
    /// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/mucalc.html#grammar-token-MultAct
    pub fn parse_multi_action(&mut self) -> Result<Vec<Action>, ParseError> {
        if !self.has_token() {
            let loc = self.get_last_loc();
            return Err(ParseError::end_of_input("a multi-action", loc));
        }

        if self.skip_if_equal(&LexicalElement::Tau) {
            // tau = empty multi-action
            Ok(Vec::new())
        } else {
            let mut actions = Vec::new();
            while {
                let id = self.parse_identifier()?;

                let args = if self.skip_if_equal(&LexicalElement::OpeningParen) {
                    let a = self.parse_expr_list()?;
                    self.expect_token(&LexicalElement::ClosingParen)?;
                    a
                } else {
                    Vec::new()
                };

                actions.push(Action { id, args });

                self.skip_if_equal(&LexicalElement::Pipe)
            } {}
            Ok(actions)
        }
    }

    fn parse_parallel_proc(&mut self) -> Result<Proc, ParseError> {
        // || (associative, so treat it as if it associates to the right)
        self.parse_right_associative_proc(
            &|parser| parser.parse_right_parallel_proc(),
            &LexicalElement::DoublePipe,
            &|lhs, rhs| ProcEnum::Parallel { lhs, rhs },
        )
    }

    fn parse_right_parallel_proc(&mut self) -> Result<Proc, ParseError> {
        // ||_
        self.parse_right_associative_proc(
            &|parser| parser.parse_conditional_proc(),
            &LexicalElement::DoublePipeUnderscore,
            &|lhs, rhs| ProcEnum::RightParallel { lhs, rhs },
        )
    }

    fn parse_conditional_proc(&mut self) -> Result<Proc, ParseError> {
        // a -> b and a -> b <> c
        // when encountering an identifier or '(', it could be either an
        // expression followed by ->, or it could be a process
        let token = self.get_token();
        let loc = token.loc;

        let mut parser_copy = self.clone();
        Ok(if parser_copy.is_unit_data_expr()? {
            let condition = Rc::new(self.parse_unit_expr()?);
            self.expect_token(&LexicalElement::Arrow)?;
            let then_proc = Rc::new(self.parse_conditional_proc()?);
            let else_proc = if self.skip_if_equal(&LexicalElement::Diamond) {
                Some(Rc::new(self.parse_conditional_proc()?))
            } else {
                None
            };

            Proc::new(ProcEnum::IfThenElse { condition, then_proc, else_proc }, loc)
        } else {
            self.parse_concat_proc()?
        })
    }

    fn parse_concat_proc(&mut self) -> Result<Proc, ParseError> {
        // . (associative, so treat it as if it associates to the right)
        self.parse_right_associative_proc(
            &|parser| parser.parse_time_proc(),
            &LexicalElement::Period,
            &|lhs, rhs| ProcEnum::Concat { lhs, rhs },
        )
    }

    fn parse_time_proc(&mut self) -> Result<Proc, ParseError> {
        // @
        let loc = self.get_loc();
        let proc = self.parse_multi_proc()?;
        if self.skip_if_equal(&LexicalElement::AtSign) {
            let time = Rc::new(self.parse_unit_expr()?);
            Ok(Proc::new(ProcEnum::Time { proc: Rc::new(proc), time }, loc))
        } else {
            Ok(proc)
        }
    }

    fn parse_multi_proc(&mut self) -> Result<Proc, ParseError> {
        // |
        self.parse_right_associative_proc(
            &|parser| parser.parse_basic_proc(),
            &LexicalElement::Pipe,
            &|lhs, rhs| ProcEnum::Multi { lhs, rhs },
        )
    }

    fn parse_basic_proc(&mut self) -> Result<Proc, ParseError> {
        let token = self.get_token();
        let loc = token.loc;
        Ok(match &token.value {
            LexicalElement::Identifier(id) => {
                let id = Identifier::new(id);
                self.skip_token();
                let args = if self.skip_if_equal(&LexicalElement::OpeningParen) {
                    let args = self.parse_expr_list()?;
                    self.expect_token(&LexicalElement::ClosingParen)?;
                    args
                } else {
                    Vec::new()
                };
                Proc::new(ProcEnum::Action { value: Action { id, args } }, loc)
            },
            LexicalElement::Delta => {
                self.skip_token();
                Proc::new(ProcEnum::Delta, loc)
            },
            LexicalElement::Tau => {
                self.skip_token();
                Proc::new(ProcEnum::Tau, loc)
            },
            LexicalElement::Block => {
                let (ids, proc) = self.parse_unary_process_operator(|parser| {
                    parser.parse_identifier_list()
                })?;
                Proc::new(ProcEnum::Block { ids, proc }, loc)
            },
            LexicalElement::Allow => {
                let (multi_ids, proc) = self.parse_unary_process_operator(|parser| {
                    let mut multi_ids = Vec::new();
                    while {
                        multi_ids.push(parser.parse_multi_id()?);

                        parser.skip_if_equal(&LexicalElement::Comma)
                    } {}
                    // a|b|c, d|e, f, ..., g|h
                    todo!()
                })?;
                Proc::new(ProcEnum::Allow { multi_ids, proc }, loc)
            },
            LexicalElement::Hide => {
                let (ids, proc) = self.parse_unary_process_operator(|parser| {
                    parser.parse_identifier_list()
                })?;
                Proc::new(ProcEnum::Hide { ids, proc }, loc)
            },
            LexicalElement::Rename => {
                let (mappings, proc) = self.parse_unary_process_operator(|parser| {
                    // a -> b, c -> d, ..., e -> f
                    let mut mappings = Vec::new();
                    while {
                        let lhs = parser.parse_identifier()?;
                        parser.expect_token(&LexicalElement::Arrow)?;
                        let rhs = parser.parse_identifier()?;
                        mappings.push(RenameMapping { lhs, rhs });

                        parser.skip_if_equal(&LexicalElement::Comma)
                    } {}
                    Ok(mappings)
                })?;
                Proc::new(ProcEnum::Rename { mappings, proc }, loc)
            },
            LexicalElement::Comm => {
                let (mappings, proc) = self.parse_unary_process_operator(|parser| {
                    // a|b|c -> d, ..., e|f -> g
                    let mut mappings = Vec::new();
                    while {
                        let loc = parser.get_loc();
                        let lhs = parser.parse_multi_id()?;
                        parser.expect_token(&LexicalElement::Arrow)?;
                        let rhs = parser.parse_identifier()?;

                        if lhs.len() < 2 {
                            let mut message = String::new();
                            message.push_str("The left-hand side of a mapping in a `comm` ");
                            message.push_str("clause must be a multi-action with >= 2 actions; ");
                            message.push_str("consider using a `rename` otherwise");
                            return Err(ParseError::new(message, loc));
                        }

                        mappings.push(CommMapping { lhs, rhs });

                        parser.skip_if_equal(&LexicalElement::Comma)
                    } {}
                    Ok(mappings)
                })?;
                Proc::new(ProcEnum::Comm { mappings, proc }, loc)
            },
            LexicalElement::OpeningParen => {
                self.skip_token();
                let proc = self.parse_proc()?;
                self.expect_token(&LexicalElement::ClosingParen)?;
                proc
            },
            LexicalElement::Sum => {
                self.skip_token();
                let variables = self.parse_var_decl_list()?;
                self.expect_token(&LexicalElement::Period)?;
                // NOTE: `sum` has lower precedence than . but higher than +
                let proc = Rc::new(self.parse_concat_proc()?);
                Proc::new(ProcEnum::Sum { variables, proc }, loc)
            },
            _ => {
                return Err(ParseError::expected("a process", token));
            },
            // LexicalElement::Dist => {
            //     unimplemented!()
            // },
        })
    }

    fn parse_multi_id(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut ids = Vec::new();
        ids.push(self.parse_identifier()?);
        while self.skip_if_equal(&LexicalElement::Pipe) {
            ids.push(self.parse_identifier()?);
        }
        Ok(ids)
    }

    fn parse_right_associative_proc<F, C>(
        &mut self,
        sub_parser: &F,
        operator: &LexicalElement,
        constructor: &C,
    ) -> Result<Proc, ParseError>
    where
        F: Fn(&mut Parser) -> Result<Proc, ParseError>,
        C: Fn(Rc<Proc>, Rc<Proc>) -> ProcEnum,
    {
        let loc = self.get_loc();
        let lhs = sub_parser(self)?;
        if self.skip_if_equal(operator) {
            let rhs = self.parse_right_associative_proc(sub_parser, operator, constructor)?;
            Ok(Proc::new(constructor(Rc::new(lhs), Rc::new(rhs)), loc))
        } else {
            Ok(lhs)
        }
    }

    // Parses processes of the form `OP({ ... }, process)`.
    fn parse_unary_process_operator<F, T>(
        &mut self,
        set_parser: F,
    ) -> Result<(T, Rc<Proc>), ParseError>
    where F: Fn(&mut Parser) -> Result<T, ParseError> {
        self.skip_token();
        self.expect_token(&LexicalElement::OpeningParen)?;
        self.expect_token(&LexicalElement::OpeningBrace)?;
        let ids = set_parser(self)?;
        self.expect_token(&LexicalElement::ClosingBrace)?;
        self.expect_token(&LexicalElement::Comma)?;
        let proc = Rc::new(self.parse_proc()?);
        self.expect_token(&LexicalElement::ClosingParen)?;
        Ok((ids, proc))
    }

    fn is_unit_data_expr(&mut self) -> Result<bool, ParseError> {
        use LexicalElement::*;

        let token = self.get_token();

        Ok(match &token.value {
            True | False | ExclamationMark | Dash | HashSign | Integer(_) => true,
            OpeningParen => {
                advance_nested_parentheses(self) == 0 && self.is_token(&Arrow)
            },
            Identifier(_) => {
                self.skip_token();
                if self.is_token(&Arrow) {
                    true
                } else if self.is_token(&OpeningParen) {
                    advance_nested_parentheses(self) == 0 && self.is_token(&Arrow)
                } else {
                    false
                }
            },
            _ => false,
        })
    }
}

fn advance_nested_parentheses(parser: &mut Parser) -> u32 {
    parser.expect_token(&LexicalElement::OpeningParen).unwrap();
    let mut parentheses = 1u32;
    while parentheses > 0 && parser.has_token() {
        if parser.is_token(&LexicalElement::OpeningParen) {
            parentheses += 1;
        } else if parser.is_token(&LexicalElement::ClosingParen) {
            parentheses -= 1;
        }
        parser.skip_token();
    }
    parentheses
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unwrap_pattern;
    use crate::ast::expr::{Expr, ExprEnum};
    use crate::parser::lexer::tokenize;

    fn parse_ite(input: &str) -> Option<(Rc<Expr>, Rc<Proc>, Option<Rc<Proc>>)> {
        let tokens = tokenize(input).unwrap();
        let proc = Parser::new(&tokens).parse_proc().unwrap();
        if let ProcEnum::IfThenElse { condition, then_proc, else_proc } = proc.value {
            Some((condition, then_proc, else_proc))
        } else {
            None
        }
    }

    #[test]
    fn test_parse_proc_basic() {
        let tokens = tokenize("a(123) . b").unwrap();
        let _proc = Parser::new(&tokens).parse_proc().unwrap();

        // TODO
    }

    #[test]
    fn test_parse_proc_conditional() {
        let (_, _, else_proc) = parse_ite("a(1) -> b(1)").unwrap();
        assert!(else_proc.is_none());

        let (_, _, else_proc) = parse_ite("a(1) -> b <> c(3)").unwrap();
        assert!(else_proc.is_some());

        let (_, _, else_proc) = parse_ite("(a + b) -> c").unwrap();
        assert!(else_proc.is_none());

        assert!(parse_ite("a(1) -> b + c <> d(3) . e").is_none());

        let (_, _, else_proc) = parse_ite("a -> (b + c) <> d(3) . e").unwrap();
        assert!(else_proc.is_some());

        assert!(parse_ite("a").is_none());
        assert!(parse_ite("-a -> b").is_some());
        assert!(parse_ite("a + b -> c").is_none());
        assert!(parse_ite("(a + b)").is_none());
        assert!(parse_ite("a(1) -> b + c <> d(3) + e").is_none());

        let tokens = tokenize("-a").unwrap();
        assert!(Parser::new(&tokens).parse_proc().is_err());
    }

    #[test]
    fn test_parse_proc_conditional_nested() {
        // should be parsed as `a -> (b -> c <> d)`
        let (_, then_proc, else_proc) = parse_ite("a -> -b -> c <> d").unwrap();
        assert!(else_proc.is_none());
        let (condition, then_proc2, else_proc2) = unwrap_pattern!(
            &then_proc.value,
            ProcEnum::IfThenElse { condition, then_proc, else_proc } => (condition, then_proc, else_proc)
        );
        let neg = unwrap_pattern!(&condition.value, ExprEnum::Negate { value } => value);
        let b = unwrap_pattern!(&neg.value, ExprEnum::Id { id } => id);
        assert_eq!(b.get_value(), "b");
        let c = unwrap_pattern!(&then_proc2.value, ProcEnum::Action { value } => value);
        assert_eq!(c.id.get_value(), "c");
        let d = unwrap_pattern!(&else_proc2.as_ref().unwrap().value, ProcEnum::Action { value } => value);
        assert_eq!(d.id.get_value(), "d");

        // should be parsed as `a -> (b -> c <> d) <> e`
        let (_, then_proc, else_proc) = parse_ite("a -> b -> c <> d <> e").unwrap();
        let (then_proc2, else_proc2) = unwrap_pattern!(
            &then_proc.value,
            ProcEnum::IfThenElse { condition: _, then_proc, else_proc } => (then_proc, else_proc)
        );
        let c = unwrap_pattern!(&then_proc2.value, ProcEnum::Action { value } => value);
        assert_eq!(c.id.get_value(), "c");
        let d = unwrap_pattern!(&else_proc2.as_ref().unwrap().value, ProcEnum::Action { value } => value);
        assert_eq!(d.id.get_value(), "d");
        let e = unwrap_pattern!(&else_proc.as_ref().unwrap().value, ProcEnum::Action { value } => value);
        assert_eq!(e.id.get_value(), "e");

        // should be parsed as `a -> (b -> c <> (d -> e))`
        let (_, then_proc, else_proc) = parse_ite("a -> b -> c <> d -> e").unwrap();
        assert!(else_proc.is_none());

        let (condition, then_proc2, else_proc2) = unwrap_pattern!(
            &then_proc.value,
            ProcEnum::IfThenElse { condition, then_proc, else_proc } => (condition, then_proc, else_proc)
        );
        let b = unwrap_pattern!(&condition.value, ExprEnum::Id { id } => id);
        assert_eq!(b.get_value(), "b");
        let c = unwrap_pattern!(&then_proc2.value, ProcEnum::Action { value } => value);
        assert_eq!(c.id.get_value(), "c");

        let (then_proc3, else_proc3) = unwrap_pattern!(
            &else_proc2.as_ref().unwrap().value,
            ProcEnum::IfThenElse { then_proc, else_proc, .. } => (then_proc, else_proc)
        );
        let e = unwrap_pattern!(&then_proc3.value, ProcEnum::Action { value } => value);
        assert_eq!(e.id.get_value(), "e");
        assert!(else_proc3.is_none());
    }
}
