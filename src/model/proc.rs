//! Defines AST types for processes, which are essentially programs that are
//! defined in terms of states (which is a tuple of data elements such as
//! numbers, lists and sets) and steps (actions) that move from one state to
//! another.
//! 
//! These steps can be either deterministic or non-deterministic.
//! 
//! # See also
//! The [mCRL2 spec on this].
//! 
//! [mCRL2 spec on this]: https://mcrl2.org/web/user_manual/language_reference/process.html

use crate::core::lexer::LexicalElement;
use crate::core::parser::{Parseable, ParseError, Parser};
use crate::core::syntax::{Identifier, SourceRange};
use crate::model::decl::VariableDecl;
use crate::model::expr::{Expr, parse_unit_expr};

use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

use super::display::display_pretty_default;

/// A process expression in an mCRL2 model.
pub struct Proc {
    pub value: ProcEnum,
    pub loc: SourceRange,
}

impl Proc {
    /// Creates a new process with `parent` set to `None`.
    pub fn new(value: ProcEnum, loc: SourceRange) -> Self {
        Proc { value, loc }
    }
}

impl Debug for Proc {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.value)?;
        Ok(())
    }
}

impl Display for Proc {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

#[derive(Debug)]
pub enum ProcEnum {
    Action {
        id: Identifier,
        args: Vec<Arc<Expr>>,
    },
    // NOTE: at parse time, it's not really possible to distinguish between
    // actions and named processes
    // Id {
    //     id: Identifier,
    //     args: Vec<Arc<Expr>>,
    // },
    Delta,
    Tau,
    Block {
        ids: Vec<(Identifier, SourceRange)>,
        proc: Arc<Proc>,
    },
    Allow {
        multi_ids: Vec<Vec<(Identifier, SourceRange)>>,
        proc: Arc<Proc>,
    },
    Hide {
        ids: Vec<(Identifier, SourceRange)>,
        proc: Arc<Proc>,
    },
    Rename {
        mappings: Vec<RenameMapping>,
        proc: Arc<Proc>,
    },
    Comm {
        mappings: Vec<CommMapping>,
        proc: Arc<Proc>,
    },
    Add {
        lhs: Arc<Proc>,
        rhs: Arc<Proc>,
    },
    Sum {
        variables: Vec<VariableDecl>,
        proc: Arc<Proc>,
    },
    Parallel {
        lhs: Arc<Proc>,
        rhs: Arc<Proc>,
    },
    RightParallel {
        lhs: Arc<Proc>,
        rhs: Arc<Proc>,
    },
    Multi { // a | b
        lhs: Arc<Proc>,
        rhs: Arc<Proc>,
    },
    IfThenElse {
        condition: Arc<Expr>,
        then_proc: Arc<Proc>,
        else_proc: Option<Arc<Proc>>,
    },
    // TODO what is "<<" in the spec???
    LeftShift {
        lhs: Arc<Proc>,
        rhs: Arc<Proc>,
    },
    Concat {
        lhs: Arc<Proc>,
        rhs: Arc<Proc>,
    },
    Time {
        proc: Arc<Proc>,
        time: Arc<Expr>,
    },
}

#[derive(Clone, Debug)]
pub struct Action {
    pub id: Identifier,
    pub args: Vec<Arc<Expr>>,
}

#[derive(Debug)]
pub struct CommMapping {
    pub lhs: Vec<(Identifier, SourceRange)>,
    pub rhs: Identifier,
    pub rhs_loc: SourceRange,
}

#[derive(Debug)]
pub struct RenameMapping {
    pub lhs: Identifier,
    pub lhs_loc: SourceRange,
    pub rhs: Identifier,
    pub rhs_loc: SourceRange,
}

impl Parseable for Proc {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_proc(parser)
    }
}

/// Parses a `Proc` (short for process).
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [mCRL2 grammar on this]: https://mcrl2.org/web/user_manual/language_reference/process.html#grammar-token-ProcExpr
pub fn parse_proc(parser: &mut Parser) -> Result<Proc, ParseError> {
    if !parser.has_token() {
        return parser.end_of_input("a process");
    }

    // + (associative, so treat it as if it associates to the right)
    parse_right_associative_proc(
        parser,
        &|parser| parse_parallel_proc(parser),
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
pub fn parse_multi_action(parser: &mut Parser) -> Result<Vec<(Action, SourceRange)>, ParseError> {
    if !parser.has_token() {
        return parser.end_of_input("a multi-action");
    }

    if parser.skip_if_equal(&LexicalElement::Tau) {
        // tau = empty multi-action
        Ok(Vec::new())
    } else {
        let mut actions = Vec::new();
        while {
            let (id, id_loc) = parser.parse_identifier()?;

            let args = if parser.skip_if_equal(&LexicalElement::OpeningParen) {
                let a = parser.parse::<Vec<Arc<Expr>>>()?;
                parser.expect_token(&LexicalElement::ClosingParen)?;
                a
            } else {
                Vec::new()
            };

            actions.push((Action { id, args }, id_loc));

            parser.skip_if_equal(&LexicalElement::Pipe)
        } {}
        Ok(actions)
    }
}

fn parse_parallel_proc(parser: &mut Parser) -> Result<Proc, ParseError> {
    // || (associative, so treat it as if it associates to the right)
    parse_right_associative_proc(
        parser,
        &|parser| parse_right_parallel_proc(parser),
        &LexicalElement::DoublePipe,
        &|lhs, rhs| ProcEnum::Parallel { lhs, rhs },
    )
}

fn parse_right_parallel_proc(parser: &mut Parser) -> Result<Proc, ParseError> {
    // ||_
    parse_right_associative_proc(
        parser,
        &|parser| parse_conditional_proc(parser),
        &LexicalElement::DoublePipeUnderscore,
        &|lhs, rhs| ProcEnum::RightParallel { lhs, rhs },
    )
}

fn parse_conditional_proc(parser: &mut Parser) -> Result<Proc, ParseError> {
    // a -> b and a -> b <> c
    // when encountering an identifier or '(', it could be either an
    // expression followed by ->, or it could be a process
    if !parser.has_token() {
        return parser.end_of_input("a process");
    }

    let loc = parser.get_loc();

    let mut parser_copy = parser.clone();
    if is_unit_data_expr(&mut parser_copy)? {
        let condition = Arc::new(parse_unit_expr(parser)?);
        parser.expect_token(&LexicalElement::Arrow)?;
        let then_proc = Arc::new(parse_conditional_proc(parser)?);
        let else_proc = if parser.skip_if_equal(&LexicalElement::Diamond) {
            Some(Arc::new(parse_conditional_proc(parser)?))
        } else {
            None
        };

        Ok(Proc::new(
            ProcEnum::IfThenElse { condition, then_proc, else_proc },
            parser.until_now(&loc),
        ))
    } else {
        Ok(parse_concat_proc(parser)?)
    }
}

fn parse_concat_proc(parser: &mut Parser) -> Result<Proc, ParseError> {
    // . (associative, so treat it as if it associates to the right)
    parse_right_associative_proc(
        parser,
        &|parser| parse_time_proc(parser),
        &LexicalElement::Period,
        &|lhs, rhs| ProcEnum::Concat { lhs, rhs },
    )
}

fn parse_time_proc(parser: &mut Parser) -> Result<Proc, ParseError> {
    if !parser.has_token() {
        return parser.end_of_input("an expression");
    }

    // @
    let loc = parser.get_loc();
    let proc = parse_multi_proc(parser)?;
    if parser.skip_if_equal(&LexicalElement::AtSign) {
        let time = Arc::new(parse_unit_expr(parser)?);
        Ok(Proc::new(
            ProcEnum::Time { proc: Arc::new(proc), time },
            parser.until_now(&loc),
        ))
    } else {
        Ok(proc)
    }
}

fn parse_multi_proc(parser: &mut Parser) -> Result<Proc, ParseError> {
    // |
    parse_right_associative_proc(
        parser,
        &|parser| parse_basic_proc(parser),
        &LexicalElement::Pipe,
        &|lhs, rhs| ProcEnum::Multi { lhs, rhs },
    )
}

fn parse_basic_proc(parser: &mut Parser) -> Result<Proc, ParseError> {
    let token = parser.get_token();
    let loc = token.loc;
    Ok(match &token.value {
        LexicalElement::Identifier(id) => {
            let id = Identifier::new(id);
            parser.skip_token();
            let args = if parser.skip_if_equal(&LexicalElement::OpeningParen) {
                let args = parser.parse::<Vec<Arc<Expr>>>()?;
                parser.expect_token(&LexicalElement::ClosingParen)?;
                args
            } else {
                Vec::new()
            };
            Proc::new(
                ProcEnum::Action { id, args },
                parser.until_now(&loc),
            )
        },
        LexicalElement::Delta => {
            parser.skip_token();
            Proc::new(ProcEnum::Delta, loc)
        },
        LexicalElement::Tau => {
            parser.skip_token();
            Proc::new(ProcEnum::Tau, loc)
        },
        LexicalElement::Block => {
            let (ids, proc) = parse_unary_process_operator(
                parser,
                |parser| parser.parse_identifier_list(),
            )?;
            Proc::new(ProcEnum::Block { ids, proc }, parser.until_now(&loc))
        },
        LexicalElement::Allow => {
            let (multi_ids, proc) = parse_unary_process_operator(
                parser,
                |parser| {
                    let mut multi_ids = Vec::new();
                    while {
                        multi_ids.push(parse_multi_id(parser)?);

                        parser.skip_if_equal(&LexicalElement::Comma)
                    } {}
                    // a|b|c, d|e, f, ..., g|h
                    Ok(multi_ids)
                },
            )?;
            Proc::new(ProcEnum::Allow { multi_ids, proc }, parser.until_now(&loc))
        },
        LexicalElement::Hide => {
            let (ids, proc) = parse_unary_process_operator(
                parser,
                |parser| parser.parse_identifier_list(),
            )?;
            Proc::new(ProcEnum::Hide { ids, proc }, loc)
        },
        LexicalElement::Rename => {
            let (mappings, proc) = parse_unary_process_operator(
                parser,
                |parser| {
                    // a -> b, c -> d, ..., e -> f
                    let mut mappings = Vec::new();
                    while {
                        let (lhs, lhs_loc) = parser.parse_identifier()?;
                        parser.expect_token(&LexicalElement::Arrow)?;
                        let (rhs, rhs_loc) = parser.parse_identifier()?;
                        mappings.push(RenameMapping { lhs, lhs_loc, rhs, rhs_loc });

                        parser.skip_if_equal(&LexicalElement::Comma)
                    } {}
                    Ok(mappings)
                },
            )?;
            Proc::new(ProcEnum::Rename { mappings, proc }, parser.until_now(&loc))
        },
        LexicalElement::Comm => {
            let (mappings, proc) = parse_unary_process_operator(
                parser,
                |parser| {
                    // a|b|c -> d, ..., e|f -> g
                    let mut mappings = Vec::new();
                    while {
                        if !parser.has_token() {
                            return parser.end_of_input("a communication mapping");
                        }

                        let loc = parser.get_loc();
                        let lhs = parse_multi_id(parser)?;
                        parser.expect_token(&LexicalElement::Arrow)?;
                        let (rhs, rhs_loc) = parser.parse_identifier()?;

                        if lhs.len() < 2 {
                            let mut message = String::new();
                            message.push_str("The left-hand side of a mapping in a `comm` ");
                            message.push_str("clause must be a multi-action with >= 2 actions; ");
                            message.push_str("consider using a `rename` otherwise");
                            return Err(ParseError::new(message, loc));
                        }

                        mappings.push(CommMapping { lhs, rhs, rhs_loc });

                        parser.skip_if_equal(&LexicalElement::Comma)
                    } {}
                    Ok(mappings)
                },
            )?;
            Proc::new(ProcEnum::Comm { mappings, proc }, parser.until_now(&loc))
        },
        LexicalElement::OpeningParen => {
            parser.skip_token();
            let mut proc = parser.parse::<Proc>()?;
            parser.expect_token(&LexicalElement::ClosingParen)?;
            proc.loc = parser.until_now(&loc);
            proc
        },
        LexicalElement::Sum => {
            parser.skip_token();
            let variables = parser.parse::<Vec<VariableDecl>>()?;
            parser.expect_token(&LexicalElement::Period)?;
            // NOTE: `sum` has lower precedence than . but higher than +
            let proc = Arc::new(parse_conditional_proc(parser)?);
            Proc::new(ProcEnum::Sum { variables, proc }, parser.until_now(&loc))
        },
        _ => {
            return Err(ParseError::expected("a process", token));
        },
        // LexicalElement::Dist => {
        //     unimplemented!()
        // },
    })
}

fn parse_multi_id(parser: &mut Parser) -> Result<Vec<(Identifier, SourceRange)>, ParseError> {
    let mut ids = Vec::new();
    ids.push(parser.parse_identifier()?);
    while parser.skip_if_equal(&LexicalElement::Pipe) {
        ids.push(parser.parse_identifier()?);
    }
    Ok(ids)
}

fn parse_right_associative_proc<F, C>(
    parser: &mut Parser,
    sub_parser: &F,
    operator: &LexicalElement,
    constructor: &C,
) -> Result<Proc, ParseError>
where
    F: Fn(&mut Parser) -> Result<Proc, ParseError>,
    C: Fn(Arc<Proc>, Arc<Proc>) -> ProcEnum,
{
    if !parser.has_token() {
        return parser.end_of_input("a process");
    }

    let loc = parser.get_loc();
    let lhs = sub_parser(parser)?;
    if parser.skip_if_equal(operator) {
        let rhs = parse_right_associative_proc(parser, sub_parser, operator, constructor)?;

        Ok(Proc::new(
            constructor(Arc::new(lhs), Arc::new(rhs)),
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

// Parses processes of the form `OP({ ... }, process)`.
fn parse_unary_process_operator<F, T>(
    parser: &mut Parser,
    set_parser: F,
) -> Result<(T, Arc<Proc>), ParseError>
where F: Fn(&mut Parser) -> Result<T, ParseError> {
    parser.skip_token();
    parser.expect_token(&LexicalElement::OpeningParen)?;
    parser.expect_token(&LexicalElement::OpeningBrace)?;
    let ids = set_parser(parser)?;
    parser.expect_token(&LexicalElement::ClosingBrace)?;
    parser.expect_token(&LexicalElement::Comma)?;
    let proc = Arc::new(parser.parse::<Proc>()?);
    parser.expect_token(&LexicalElement::ClosingParen)?;
    Ok((ids, proc))
}

/// A function I did not want to write, but is necessary to resolve the parsing
/// difficulties with `->`.
/// 
/// The `->` process expects a UnitExpr on the left-hand side which can be a
/// parenthesised expression (example: `(a + b == c) -> d`), while a process
/// expression can also start with a parenthesis (example: `(a(2) + b(3)) .
/// delta`).
/// 
/// As a result, some lookahead is necessary, because the first token alone
/// does not determine whether we are looking at an expression or a process.
fn is_unit_data_expr(parser: &mut Parser) -> Result<bool, ParseError> {
    use LexicalElement::*;

    assert!(parser.has_token());
    let token = parser.get_token();

    Ok(match &token.value {
        True | False | ExclamationMark | Dash | HashSign | Integer(_) => true,
        OpeningParen => {
            advance_nested_parentheses(parser) == 0 && parser.is_token(&Arrow)
        },
        Identifier(_) => {
            parser.skip_token();
            if parser.is_token(&Arrow) {
                true
            } else if parser.is_token(&OpeningParen) {
                advance_nested_parentheses(parser) == 0 && parser.is_token(&Arrow)
            } else {
                false
            }
        },
        _ => false,
    })
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

    if parser.is_token(&LexicalElement::OpeningParen) {
        advance_nested_parentheses(parser)
    } else {
        parentheses
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unwrap_pattern;
    use crate::core::lexer::tokenize;
    use crate::model::expr::{Expr, ExprEnum};

    fn parse_ite(input: &str) -> Option<(Arc<Expr>, Arc<Proc>, Option<Arc<Proc>>)> {
        let tokens = tokenize(input).unwrap();
        let proc = Parser::new(&tokens).parse::<Proc>().unwrap();
        if let ProcEnum::IfThenElse { condition, then_proc, else_proc } = proc.value {
            Some((condition, then_proc, else_proc))
        } else {
            None
        }
    }

    #[test]
    fn test_parse_proc_basic() {
        let tokens = tokenize("a(123) . b").unwrap();
        let _proc = Parser::new(&tokens).parse::<Proc>().unwrap();

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
        assert!(Parser::new(&tokens).parse::<Proc>().is_err());

        // TODO: test a(0)(1) -> b
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
        let ProcEnum::Action { id: c_id, .. } = &then_proc2.value else { panic!(); };
        assert_eq!(c_id.get_value(), "c");
        let ProcEnum::Action { id: d_id, .. } = &else_proc2.as_ref().unwrap().value else { panic!(); };
        assert_eq!(d_id.get_value(), "d");

        // should be parsed as `a -> (b -> c <> d) <> e`
        let (_, then_proc, else_proc) = parse_ite("a -> b -> c <> d <> e").unwrap();
        let (then_proc2, else_proc2) = unwrap_pattern!(
            &then_proc.value,
            ProcEnum::IfThenElse { condition: _, then_proc, else_proc } => (then_proc, else_proc)
        );
        let ProcEnum::Action { id: c_id, .. } = &then_proc2.value else { panic!(); };
        assert_eq!(c_id.get_value(), "c");
        let ProcEnum::Action { id: d_id, .. } = &else_proc2.as_ref().unwrap().value else { panic!(); };
        assert_eq!(d_id.get_value(), "d");
        let ProcEnum::Action { id: e_id, .. } = &else_proc.unwrap().value else { panic!(); };
        assert_eq!(e_id.get_value(), "e");

        // should be parsed as `a -> (b -> c <> (d -> e))`
        let (_, then_proc, else_proc) = parse_ite("a -> b -> c <> d -> e").unwrap();
        assert!(else_proc.is_none());

        let (condition, then_proc2, else_proc2) = unwrap_pattern!(
            &then_proc.value,
            ProcEnum::IfThenElse { condition, then_proc, else_proc } => (condition, then_proc, else_proc)
        );
        let b = unwrap_pattern!(&condition.value, ExprEnum::Id { id } => id);
        assert_eq!(b.get_value(), "b");
        let ProcEnum::Action { id: c_id, .. } = &then_proc2.value else { panic!(); };
        assert_eq!(c_id.get_value(), "c");

        let (then_proc3, else_proc3) = unwrap_pattern!(
            &else_proc2.as_ref().unwrap().value,
            ProcEnum::IfThenElse { then_proc, else_proc, .. } => (then_proc, else_proc)
        );
        let ProcEnum::Action { id: e_id, .. } = &then_proc3.value else { panic!(); };
        assert_eq!(e_id.get_value(), "e");
        assert!(else_proc3.is_none());
    }
}
