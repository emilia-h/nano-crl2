//! Defines AST types for declarations (also called spec/specification in the
//! mCRL2 spec), which defines something in the model, often with a name (an
//! identifier).
//! 
//! Examples are action declarations written as `act name: Sort;` or process
//! declarations written as "proc Name(param: Sort) = process;"
//! 
//! # See also
//! 
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/mcrl2.html#specification-syntax).

use crate::core::lexer::LexicalElement;
use crate::core::parser::{Parseable, ParseError, Parser};
use crate::core::syntax::{Identifier, SourceRange};
use crate::model::display::display_pretty_default;
use crate::model::expr::Expr;
use crate::model::proc::Proc;
use crate::model::sort::Sort;

use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

/// A declaration in an mCRL2 model.
pub struct Decl {
    pub value: DeclEnum,
    pub loc: SourceRange,
}

impl Decl {
    /// Creates a new declaration.
    pub fn new(value: DeclEnum, loc: SourceRange) -> Self {
        Decl { value, loc }
    }
}

impl Debug for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.value)?;
        Ok(())
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

/// The options for a declaration.
#[derive(Debug)]
pub enum DeclEnum {
    Action {
        ids: Vec<(Identifier, SourceRange)>,
        sort: Option<Arc<Sort>>,
    },
    Constructor {
        ids: Vec<(Identifier, SourceRange)>,
        sort: Arc<Sort>,
    },
    EquationSet {
        variables: Vec<VariableDecl>,
        equations: Vec<EquationDecl>,
    },
    GlobalVariable {
        variables: Vec<VariableDecl>,
    },
    Initial {
        value: Arc<Proc>,
    },
    Map {
        id: Identifier,
        id_loc: SourceRange,
        sort: Arc<Sort>,
    },
    /// Represents a process declaration of the form `proc Name(params) =
    /// process;`.
    /// 
    /// Note that parameters are of the form `(id11, ..., id1M: Sort1, ...,
    /// idN1, ..., idNM: SortN)` i.e. parameters can be grouped together, hence
    /// the complicated data representation of the parameters.
    Process {
        id: Identifier,
        id_loc: SourceRange,
        params: Vec<VariableDecl>,
        proc: Arc<Proc>,
    },
    Sort {
        // can be either "sort a1, ..., aN;" or "sort a = S;"
        ids: Vec<(Identifier, SourceRange)>,
        sort: Option<Arc<Sort>>,
    },
}

/// A variable declaration.
/// 
/// Note that a variable declaration is not a top-level declaration like
/// [`Decl`](./struct.Decl). It is instead used to specify parameters for
/// equations for `map`s, for parameters of named processes, and others.
#[derive(Debug)]
pub struct VariableDecl {
    pub ids: Vec<(Identifier, SourceRange)>,
    pub sort: Arc<Sort>,
    pub loc: SourceRange,
}

/// A single equation declaration of the form `[condition ->] lhs = rhs`.
#[derive(Debug)]
pub struct EquationDecl {
    pub condition: Option<Arc<Expr>>,
    pub lhs: Arc<Expr>,
    pub rhs: Arc<Expr>,
    pub loc: SourceRange,
}

impl Parseable for Vec<Decl> {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_decl_block(parser)
    }
}

impl Parseable for Vec<VariableDecl> {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_var_decl_list(parser)
    }
}

/// Parses a single block of declarations.
/// 
/// A block is for instance `map a: Nat; b: Nat;` i.e. a group of
/// declarations preceded by the same declaration keyword.
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/mcrl2.html#grammar-token-mCRL2SpecElt
pub fn parse_decl_block(parser: &mut Parser) -> Result<Vec<Decl>, ParseError> {
    use LexicalElement::*;

    let mut decls = Vec::new();

    let mut current_decl_type = None;
    while parser.has_token() {
        let token = parser.get_token();
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
        if current_decl_type == Some(Init) {
            // can only have one `init` declaration
            return Ok(vec![parse_initial_decl(parser)?]);
        }

        decls.push(match current_decl_type.as_ref().unwrap() {
            Act => parse_action_decl(parser)?,
            Cons => parse_constructor_decl(parser)?,
            Eqn => parse_equation_decl(parser)?,
            Glob => parse_global_variable_decl(parser)?,
            Init => unreachable!(),
            Map => parse_map_decl(parser)?,
            Proc => parse_process_decl(parser)?,
            Sort => parse_sort_decl(parser)?,
            _ => unreachable!(),
        });
    }

    Ok(decls)
}

/// Parses a list of variables with types of the form `(id11, ..., id1M:
/// Sort1, ..., idN1, ..., idNM: SortN)` i.e. parameters can be grouped
/// together.
pub fn parse_var_decl_list(parser: &mut Parser) -> Result<Vec<VariableDecl>, ParseError> {
    let mut result = Vec::new();

    while {
        result.push(parse_variable_decl(parser)?);

        parser.skip_if_equal(&LexicalElement::Comma)
    } {}

    Ok(result)
}

fn parse_variable_decl(parser: &mut Parser) -> Result<VariableDecl, ParseError> {
    let loc = parser.get_loc();

    let ids = parser.parse_identifier_list()?;
    parser.expect_token(&LexicalElement::Colon)?;
    let sort = Arc::new(parser.parse::<Sort>()?);
    Ok(VariableDecl { ids, sort, loc: parser.until_now(&loc) })
}

fn parse_action_decl(parser: &mut Parser) -> Result<Decl, ParseError> {
    assert!(parser.has_token());

    let loc = parser.get_loc();
    parser.skip_if_equal(&LexicalElement::Act);

    // [act] a1, ..., an: Sort;
    let ids = parser.parse_identifier_list()?;

    let sort = if parser.skip_if_equal(&LexicalElement::Colon) {
        Some(Arc::new(parser.parse::<Sort>()?))
    } else {
        None
    };

    parser.expect_token(&LexicalElement::Semicolon)?;

    Ok(Decl::new(
        DeclEnum::Action { ids, sort },
        parser.until_now(&loc),
    ))
}

fn parse_constructor_decl(parser: &mut Parser) -> Result<Decl, ParseError> {
    assert!(parser.has_token());

    let loc = parser.get_loc();
    parser.skip_if_equal(&LexicalElement::Cons);

    // [cons] a1, ..., an: Sort;
    let ids = parser.parse_identifier_list()?;

    parser.expect_token(&LexicalElement::Colon)?;
    let sort = Arc::new(parser.parse::<Sort>()?);

    parser.expect_token(&LexicalElement::Semicolon)?;

    Ok(Decl::new(
        DeclEnum::Constructor { ids, sort },
        parser.until_now(&loc),
    ))
}

fn parse_equation_decl(parser: &mut Parser) -> Result<Decl, ParseError> {
    assert!(parser.has_token());

    let loc = parser.get_loc();

    // parse variables
    let mut variables = Vec::new();
    if parser.skip_if_equal(&LexicalElement::Var) {
        while !parser.is_token(&LexicalElement::Eqn) {
            variables.extend(parse_var_decl_list(parser)?);
            parser.expect_token(&LexicalElement::Semicolon)?;
        }
        if variables.len() == 0 {
            let message = "a `var` declaration must have at least one variable";
            return Err(ParseError::new(String::from(message), loc));
        }
    }

    // parse equations
    parser.expect_token(&LexicalElement::Eqn)?;
    let mut equations = Vec::new();
    while parser.has_token() && !is_decl_keyword(&parser.get_token().value) {
        let eqn_loc = parser.get_loc();

        let expr = Arc::new(parser.parse::<Expr>()?);
        if !parser.has_token() {
            return parser.end_of_input("either = or ->");
        } else if parser.skip_if_equal(&LexicalElement::Equals) {
            let rhs = Arc::new(parser.parse::<Expr>()?);

            equations.push(EquationDecl {
                condition: None,
                lhs: expr,
                rhs,
                loc: parser.until_now(&eqn_loc),
            });
        } else if parser.skip_if_equal(&LexicalElement::Arrow) {
            let lhs = Arc::new(parser.parse::<Expr>()?);
            parser.expect_token(&LexicalElement::Equals)?;
            let rhs = Arc::new(parser.parse::<Expr>()?);

            equations.push(EquationDecl {
                condition: Some(expr),
                lhs,
                rhs,
                loc: parser.until_now(&eqn_loc),
            });
        } else {
            return Err(ParseError::expected(
                "either = or -> in an 'eqn' declaration",
                parser.get_token(),
            ));
        }

        parser.expect_token(&LexicalElement::Semicolon)?;
    }

    Ok(Decl::new(
        DeclEnum::EquationSet { variables, equations },
        parser.until_now(&loc),
    ))
}

fn parse_global_variable_decl(parser: &mut Parser) -> Result<Decl, ParseError> {
    assert!(parser.has_token());

    let loc = parser.get_loc();
    parser.skip_if_equal(&LexicalElement::Glob);

    let variables = parse_var_decl_list(parser)?;
    parser.expect_token(&LexicalElement::Semicolon)?;

    Ok(Decl::new(
        DeclEnum::GlobalVariable { variables },
        parser.until_now(&loc),
    ))
}

fn parse_initial_decl(parser: &mut Parser) -> Result<Decl, ParseError> {
    assert!(parser.has_token());

    let loc = parser.get_loc();
    parser.expect_token(&LexicalElement::Init).unwrap();

    let value = Arc::new(parser.parse::<Proc>()?);
    parser.expect_token(&LexicalElement::Semicolon)?;

    Ok(Decl::new(
        DeclEnum::Initial { value },
        parser.until_now(&loc),
    ))
}

fn parse_map_decl(parser: &mut Parser) -> Result<Decl, ParseError> {
    assert!(parser.has_token());

    let loc = parser.get_loc();
    parser.skip_if_equal(&LexicalElement::Map);

    let (id, id_loc) = parser.parse_identifier()?;
    parser.expect_token(&LexicalElement::Colon)?;
    let sort = Arc::new(parser.parse::<Sort>()?);
    parser.expect_token(&LexicalElement::Semicolon)?;

    Ok(Decl::new(
        DeclEnum::Map { id, id_loc, sort },
        parser.until_now(&loc),
    ))
}

fn parse_process_decl(parser: &mut Parser) -> Result<Decl, ParseError> {
    assert!(parser.has_token());

    let loc = parser.get_loc();
    parser.skip_if_equal(&LexicalElement::Proc);

    let (id, id_loc) = parser.parse_identifier()?;

    let mut params = Vec::new();
    if parser.skip_if_equal(&LexicalElement::OpeningParen) {
        // (id11, .., id1M: Sort1, ..., idN1, .., idNM: SortN)
        //  ^^^^^^^^^^^^^^^^^^^^^
        //  one element in the vector
        params = parse_var_decl_list(parser)?;
        parser.expect_token(&LexicalElement::ClosingParen)?;
    }
    parser.expect_token(&LexicalElement::Equals)?;

    let process = Arc::new(parser.parse::<Proc>()?);
    parser.expect_token(&LexicalElement::Semicolon)?;

    Ok(Decl::new(
        DeclEnum::Process { id, id_loc, params, proc: process },
        parser.until_now(&loc),
    ))
}

fn parse_sort_decl(parser: &mut Parser) -> Result<Decl, ParseError> {
    assert!(parser.has_token());

    let loc = parser.get_loc();
    parser.skip_if_equal(&LexicalElement::Sort);

    let (ids, value) = if parser.is_next_token(&LexicalElement::Equals) {
        // sort A = B;
        let id = parser.parse_identifier()?;
        parser.expect_token(&LexicalElement::Equals).unwrap();
        let value = Some(Arc::new(parser.parse::<Sort>()?));
        parser.expect_token(&LexicalElement::Semicolon)?;

        (vec![id], value)
    } else {
        // sort A;
        let ids = parser.parse_identifier_list()?;
        parser.expect_token(&LexicalElement::Semicolon)?;

        (ids, None)
    };

    Ok(Decl::new(DeclEnum::Sort { ids, sort: value }, parser.until_now(&loc)))
}

fn is_decl_keyword(element: &LexicalElement) -> bool {
    use LexicalElement::*;

    matches!(element, Sort | Cons | Map | Var | Eqn | Glob | Act | Proc | Init)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::unwrap_pattern;
    use crate::core::lexer::tokenize;
    use crate::core::syntax::Identifier;
    use crate::model::expr::ExprEnum;
    use crate::model::sort::SortEnum;

    #[test]
    fn test_parse_decl_eqn() {
        let tokens = tokenize("
            var y, z, a: Nat;
                b: Set(Int);
            eqn y == z = a in b;
                y == z -> y == z = true;
        ").unwrap();
        let decl = Parser::new(&tokens).parse::<Vec<Decl>>().unwrap();
        assert_eq!(decl.len(), 1);

        let DeclEnum::EquationSet { variables, equations } = &decl[0].value else {
            panic!();
        };

        assert_eq!(variables.len(), 2);
        assert_eq!(equations.len(), 2);

        assert_eq!(&variables[0].ids, &vec![
            (Identifier::new("y"), SourceRange::new(1, 16, 1, 17)),
            (Identifier::new("z"), SourceRange::new(1, 19, 1, 20)),
            (Identifier::new("a"), SourceRange::new(1, 22, 1, 23)),
        ]);
        assert!(matches!(&variables[0].sort.value, &SortEnum::Nat));
        assert_eq!(&variables[1].ids, &vec![
            (Identifier::new("b"), SourceRange::new(2, 16, 2, 17)),
        ]);

        // first equation
        let EquationDecl { condition, lhs, rhs, loc } = &equations[0];
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
        let EquationDecl { condition, lhs, rhs, loc } = &equations[1];

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
        // let result = Parser::new(&tokens).parse::<Vec<Decl>>();
        // assert!(result.is_err());
    
        // let tokens = tokenize("sort B = struct a ? b c;").unwrap();
        // let result = Parser::new(&tokens).parse::<Vec<Decl>>();
        // assert!(result.is_err());
    
        // let tokens = tokenize("sort C = struct a(id) b;").unwrap();
        // let result = Parser::new(&tokens).parse::<Vec<Decl>>();
        // assert!(result.is_err());
    }
}
