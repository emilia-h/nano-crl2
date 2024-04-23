//! Defines AST types for data expressions.
//! 
//! # See also
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/data.html).

use crate::core::lexer::LexicalElement;
use crate::core::parser::{Parseable, ParseError, Parser};
use crate::core::syntax::{Identifier, SourceLocation};
use crate::model::decl::VariableDecl;
use crate::model::display::display_pretty_default;
use crate::model::sort::Sort;

use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

/// A data expression in an mCRL2 model.
pub struct Expr {
    pub value: ExprEnum,
    pub loc: SourceLocation,
}

impl Expr {
    /// Creates a new expression with `parent` set to `None`.
    pub fn new(value: ExprEnum, loc: SourceLocation) -> Self {
        Expr { value, loc }
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.value)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

/// The possible syntactic forms of an expression.
#[derive(Debug)]
pub enum ExprEnum {
    Id {
        id: Identifier,
    },
    Number {
        value: u64,
    },
    Bool {
        value: bool,
    },
    List {
        values: Vec<Rc<Expr>>,
    },
    Set {
        values: Vec<Rc<Expr>>,
    },
    Bag {
        values: Vec<(Rc<Expr>, Rc<Expr>)>,
    },
    SetComprehension {
        id: Identifier,
        sort: Rc<Sort>,
        expr: Rc<Expr>,
    },
    FunctionUpdate {
        function: Rc<Expr>,
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Apply {
        callee: Rc<Expr>,
        args: Vec<Rc<Expr>>,
    },
    LogicalNot {
        value: Rc<Expr>,
    },
    Negate {
        value: Rc<Expr>,
    },
    Count {
        value: Rc<Expr>,
    },
    Forall {
        variables: Vec<VariableDecl>,
        expr: Rc<Expr>,
    },
    Exists {
        variables: Vec<VariableDecl>,
        expr: Rc<Expr>,
    },
    Lambda {
        variables: Vec<VariableDecl>,
        expr: Rc<Expr>,
    },
    Implies {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    LogicalOr {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    LogicalAnd {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Equals {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    NotEquals {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    LessThan {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    LessThanEquals {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    GreaterThan {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    GreaterThanEquals {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    In {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Cons {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Snoc {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Concat {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Add {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Subtract {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Divide {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    IntegerDivide {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Mod {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Multiply {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Index {
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Where {
        expr: Rc<Expr>,
        assignments: Vec<(Identifier, Rc<Expr>)>,
    },
}

impl Parseable for Expr {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_expr(parser)
    }
}

impl Parseable for Vec<Rc<Expr>> {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_expr_list(parser)
    }
}

/// Parses an `Expr` (short for expression).
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/data.html#grammar-token-DataExpr
pub fn parse_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
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
    let loc = parser.get_loc();
    let expr = parse_binder_expr(parser)?;

    if parser.skip_if_equal(&LexicalElement::Whr) {
        let mut assignments = Vec::new();
        loop {
            if parser.skip_if_equal(&LexicalElement::End) {
                break;
            }

            let id = parser.parse_identifier()?;
            parser.expect_token(&LexicalElement::Equals)?;
            let value = Rc::new(parser.parse::<Expr>()?);
            assignments.push((id, value));
        }
        Ok(Expr::new(
            ExprEnum::Where { expr: Rc::new(expr), assignments },
            parser.until_now(&loc),
        ))
    } else {
        Ok(expr)
    }
}

// forall, exists, lambda
fn parse_binder_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    if !parser.has_token() {
        let loc = parser.get_last_loc();
        return Err(ParseError::end_of_input("an expression", loc));
    }

    let loc = parser.get_loc();

    match parser.get_token().value {
        LexicalElement::Exists => {
            parser.skip_token();
            let variables = parser.parse::<Vec<VariableDecl>>()?;
            parser.expect_token(&LexicalElement::Period)?;
            let expr = Rc::new(parse_implies_expr(parser)?);
            Ok(Expr::new(
                ExprEnum::Exists { variables, expr },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::Forall => {
            parser.skip_token();
            let variables = parser.parse::<Vec<VariableDecl>>()?;
            parser.expect_token(&LexicalElement::Period)?;
            let expr = Rc::new(parse_implies_expr(parser)?);
            Ok(Expr::new(
                ExprEnum::Forall { variables, expr },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::Lambda => {
            parser.skip_token();
            let variables = parser.parse::<Vec<VariableDecl>>()?;
            parser.expect_token(&LexicalElement::Period)?;
            let expr = Rc::new(parse_implies_expr(parser)?);
            Ok(Expr::new(
                ExprEnum::Lambda { variables, expr },
                parser.until_now(&loc),
            ))
        },
        _ => {
            parse_implies_expr(parser)
        },
    }
}

// => (associative to the right, i.e. a => b => c is a => (b => c))
fn parse_implies_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_right_associative_expr(
        parser,
        &|parser| parse_or_expr(parser),
        &LexicalElement::ThickArrow, 
        &|lhs, rhs| ExprEnum::Implies { lhs, rhs }
    )
}

// || (associative, so we pretend it associates to the right)
fn parse_or_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_right_associative_expr(
        parser,
        &|parser| parse_and_expr(parser),
        &LexicalElement::DoublePipe,
        &|lhs, rhs| ExprEnum::LogicalOr { lhs, rhs },
    )
}

// && (associative, so we pretend it associates to the right)
fn parse_and_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_right_associative_expr(
        parser,
        &|parser| parse_equals_expr(parser),
        &LexicalElement::DoubleAmpersand,
        &|lhs, rhs| ExprEnum::LogicalAnd { lhs, rhs },
    )
}

// ==, != (associates to the left! so a == b != c is (a == b) != c)
fn parse_equals_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_left_associative_expr(
        parser,
        &|parser| parse_comparison_expr(parser),
        &[
            (&LexicalElement::DoubleEquals, &|lhs, rhs| ExprEnum::Equals { lhs, rhs }),
            (&LexicalElement::NotEquals, &|lhs, rhs| ExprEnum::NotEquals { lhs, rhs }),
        ],
    )
}

// <, <=, >, >= (associate to the left)
fn parse_comparison_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_left_associative_expr(
        parser,
        &|parser| parse_in_expr(parser),
        &[
            (&LexicalElement::LessThan, &|lhs, rhs| ExprEnum::LessThan { lhs, rhs }),
            (&LexicalElement::LessThanEquals, &|lhs, rhs| ExprEnum::LessThanEquals { lhs, rhs }),
            (&LexicalElement::GreaterThan, &|lhs, rhs| ExprEnum::GreaterThan { lhs, rhs }),
            (&LexicalElement::GreaterThanEquals, &|lhs, rhs| ExprEnum::GreaterThanEquals { lhs, rhs }),
        ],
    )
}

// in (associates to the left)
fn parse_in_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_left_associative_expr(
        parser,
        &|parser| parse_cons_expr(parser),
        &[(&LexicalElement::In, &|lhs, rhs| ExprEnum::In { lhs, rhs })],
    )
}

// |> (associates to the right)
fn parse_cons_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_right_associative_expr(
        parser,
        &|parser| parse_snoc_expr(parser),
        &LexicalElement::ConsOperator,
        &|lhs, rhs| ExprEnum::Cons { lhs, rhs },
    )
}

// <| (associates to the left)
fn parse_snoc_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_left_associative_expr(
        parser,
        &|parser| parse_concat_expr(parser),
        &[(&LexicalElement::SnocOperator, &|lhs, rhs| ExprEnum::Snoc { lhs, rhs })],
    )
}

// ++ (associative, treat as if it associates to the left)
fn parse_concat_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_left_associative_expr(
        parser,
        &|parser| parse_add_expr(parser),
        &[(&LexicalElement::Concat, &|lhs, rhs| ExprEnum::Concat { lhs, rhs })],
    )
}

// +, - (associative & associates to the left, respectively)
fn parse_add_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_left_associative_expr(
        parser,
        &|parser| parse_multiply_expr(parser),
        &[
            (&LexicalElement::Plus, &|lhs, rhs| ExprEnum::Add { lhs, rhs }),
            (&LexicalElement::Dash, &|lhs, rhs| ExprEnum::Subtract { lhs, rhs }),
        ],
    )
}

// *, /, div, mod (associate to the left)
fn parse_multiply_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_left_associative_expr(
        parser,
        &|parser| parse_index_expr(parser),
        &[
            (&LexicalElement::Asterisk, &|lhs, rhs| ExprEnum::Multiply { lhs, rhs }),
            (&LexicalElement::Slash, &|lhs, rhs| ExprEnum::Divide { lhs, rhs }),
            (&LexicalElement::Div, &|lhs, rhs| ExprEnum::IntegerDivide { lhs, rhs }),
            (&LexicalElement::Mod, &|lhs, rhs| ExprEnum::Mod { lhs, rhs }),
        ],
    )
}

// . (associates to the left)
fn parse_index_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_left_associative_expr(
        parser,
        &|parser| parse_unit_expr(parser),
        &[(&LexicalElement::Period, &|lhs, rhs| ExprEnum::Index { lhs, rhs })],
    )
}

// - (negation), !, #
// https://mcrl2.org/web/user_manual/language_reference/process.html#grammar-token-DataExprUnit
// NOTE: this is slightly different from DataExprUnit in the sense that it
// allows expressions of the form x[p -> q].
pub fn parse_unit_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    if !parser.has_token() {
        let loc = parser.get_last_loc();
        return Err(ParseError::end_of_input("an expression", loc));
    }

    let token = parser.get_token();
    let loc = token.loc;

    let mut result = match &token.value {
        // NOTE: unary operators should `return`, because expressions like
        // `-f(a)` need to be parsed as `-(f(a))`, not as `(-f)(a)`
        LexicalElement::Dash => {
            parser.skip_token();
            let expr = parse_unit_expr(parser)?;
            return Ok(Expr::new(
                ExprEnum::Negate { value: Rc::new(expr) },
                parser.until_now(&loc),
            ));
        },
        LexicalElement::ExclamationMark => {
            parser.skip_token();
            let expr = parse_unit_expr(parser)?;
            return Ok(Expr::new(
                ExprEnum::LogicalNot { value: Rc::new(expr) },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::HashSign => {
            parser.skip_token();
            let expr = parse_unit_expr(parser)?;
            return Ok(Expr::new(
                ExprEnum::Count { value: Rc::new(expr) },
                parser.until_now(&loc),
            ))
        },
        LexicalElement::True => {
            parser.skip_token();
            Expr::new(ExprEnum::Bool { value: true }, loc)
        },
        LexicalElement::False => {
            parser.skip_token();
            Expr::new(ExprEnum::Bool { value: false }, loc)
        },
        &LexicalElement::Integer(value) => {
            parser.skip_token();
            Expr::new(ExprEnum::Number { value }, loc)
        },
        LexicalElement::Identifier(id) => {
            let id = Identifier::new(id);
            parser.skip_token();
            Expr::new(ExprEnum::Id { id }, loc)
        },
        LexicalElement::OpeningBracket => { // list
            parser.skip_token();

            if parser.skip_if_equal(&LexicalElement::ClosingBracket) {
                Expr::new(
                    ExprEnum::List { values: Vec::new() },
                    parser.until_now(&loc),
                )
            } else {
                let values = parse_expr_list(parser)?;
                parser.expect_token(&LexicalElement::ClosingBracket)?;
                Expr::new(ExprEnum::List { values }, parser.until_now(&loc))
            }
        },
        LexicalElement::OpeningBrace => { // set or bag
            parser.skip_token();

            if parser.skip_if_equal(&LexicalElement::Colon) {
                // {:}
                parser.expect_token(&LexicalElement::ClosingBrace)?;
                Expr::new(
                    ExprEnum::Bag { values: vec![] },
                    parser.until_now(&loc),
                )
            } else if parser.skip_if_equal(&LexicalElement::ClosingBrace) {
                // {}
                Expr::new(
                    ExprEnum::Set { values: vec![] },
                    parser.until_now(&loc),
                )
            } else if parser.is_next_token(&LexicalElement::Colon) {
                // { a: B | ... } (set comprehension) or
                // { a: 1, b: 2, ... } (bag)
                todo!()
                // let id = parser.parse_identifier()?;
                // parser.expect_token(&LexicalElement::Colon).unwrap();
                // let sort = Rc::new(parser.parse_sort()?);
                // parser.expect_token(&LexicalElement::Pipe)?;
                // let expr = Rc::new(parser.parse_expr()?);
                // Expr::new(
                //     ExprEnum::SetComprehension { id, sort, expr },
                //     parser.until_now(&loc),
                // )
            } else {
                // { a, ... } (set) or { a + b : 1, ... } (bag)
                let expr = Rc::new(parser.parse::<Expr>()?);
                if parser.skip_if_equal(&LexicalElement::Colon) { // bag
                    todo!()
                } else { // set
                    let mut values = vec![expr];
                    while parser.skip_if_equal(&LexicalElement::Comma) {
                        values.push(Rc::new(parser.parse::<Expr>()?));
                    }
                    parser.expect_token(&LexicalElement::ClosingBrace)?;
                    Expr::new(ExprEnum::Set { values }, parser.until_now(&loc))
                }
            }
        },
        LexicalElement::OpeningParen => {
            parser.skip_token();
            let mut expr = parser.parse::<Expr>()?;
            parser.expect_token(&LexicalElement::ClosingParen)?;
            expr.loc = parser.until_now(&loc);
            expr
        },
        LexicalElement::Exists | LexicalElement::Forall | LexicalElement::Lambda => {
            parse_binder_expr(parser)?
        },
        _ => {
            return Err(ParseError::expected("an expression", token));
        },
    };

    // function update, function apply
    loop {
        if parser.skip_if_equal(&LexicalElement::OpeningParen) {
            let args = parse_expr_list(parser)?;
            parser.expect_token(&LexicalElement::ClosingParen)?;

            result = Expr::new(
                ExprEnum::Apply { callee: Rc::new(result), args },
                parser.until_now(&loc),
            );
        } else if parser.skip_if_equal(&LexicalElement::OpeningBracket) {
            let lhs = Rc::new(parser.parse::<Expr>()?);
            parser.expect_token(&LexicalElement::Arrow)?;
            let rhs = Rc::new(parser.parse::<Expr>()?);
            parser.expect_token(&LexicalElement::ClosingBracket)?;

            result = Expr::new(ExprEnum::FunctionUpdate {
                function: Rc::new(result),
                lhs, rhs,
            }, parser.until_now(&loc));
        } else {
            break Ok(result);
        }
    }
}

/// Parses comma-separated list of expressions
pub fn parse_expr_list(parser: &mut Parser) -> Result<Vec<Rc<Expr>>, ParseError> {
    let mut result = Vec::new();
    result.push(Rc::new(parser.parse::<Expr>()?));
    while parser.skip_if_equal(&LexicalElement::Comma) {
        result.push(Rc::new(parser.parse::<Expr>()?));
    }
    Ok(result)
}

// generic function for parsing left-associative expressions such as for -,
// ++, <|, ., etc.
fn parse_left_associative_expr<'a, F>(
    parser: &mut Parser<'a>,
    sub_parser: &F,
    options: &[(&LexicalElement, &dyn Fn(Rc<Expr>, Rc<Expr>) -> ExprEnum)],
) -> Result<Expr, ParseError>
where
    F: Fn(&mut Parser<'a>) -> Result<Expr, ParseError>,
{
    let loc = parser.get_loc();
    let mut result = sub_parser(parser)?;

    'outer: loop {
        for (lexical_element, constructor) in options {
            if parser.skip_if_equal(lexical_element) {
                let rhs = Rc::new(sub_parser(parser)?);
                result = Expr::new(
                    constructor(Rc::new(result), rhs),
                    parser.until_now(&loc),
                );
                continue 'outer;
            }
        }

        break Ok(result);
    }
}

fn parse_right_associative_expr<'a, F>(
    parser: &mut Parser<'a>,
    sub_parser: &F,
    lexical_element: &LexicalElement,
    constructor: &dyn Fn(Rc<Expr>, Rc<Expr>) -> ExprEnum,
) -> Result<Expr, ParseError>
where
    F: Fn(&mut Parser<'a>) -> Result<Expr, ParseError>,
{
    let loc = parser.get_loc();
    let lhs = sub_parser(parser)?;

    if parser.skip_if_equal(lexical_element) {
        let rhs = Rc::new(parse_right_associative_expr(
            parser, sub_parser, lexical_element, constructor,
        )?);

        Ok(Expr::new(
            constructor(Rc::new(lhs), rhs),
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unwrap_pattern;
    use crate::core::lexer::tokenize;

    #[test]
    fn test_parse_expr_binary() {
        let tokens = tokenize("a + b + c").unwrap();
        let expr = Parser::new(&tokens).parse::<Expr>().unwrap();

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
        let expr = Parser::new(&tokens).parse::<Expr>().unwrap();

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
        let expr = Parser::new(&tokens).parse::<Expr>().unwrap();

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
