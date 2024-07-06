//! Defines AST types for sorts, which is the type of an [`Expr`].
//! 
//! There are basic sorts such as `Bool` and `Int`, user-defined sorts (defined
//! using a [`SortDecl`]) and composed sorts such as `Set(T)` or `List(T)`.
//! 
//! [`Expr`]: ../expr/struct.Expr.html
//! [`SortDecl`]: ../decl/enum.DeclEnum.html#variant.SortDecl

use crate::core::lexer::LexicalElement;
use crate::core::parser::{Parseable, ParseError, Parser};
use crate::core::syntax::{Identifier, SourceRange};
use crate::model::display::display_pretty_default;

use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

/// A sort in an mCRL2 model, AKA a type.
pub struct Sort {
    pub value: SortEnum,
    pub loc: SourceRange,
}

impl Sort {
    /// Creates a new sort with `parent` set to `None`.
    pub fn new(value: SortEnum, loc: SourceRange) -> Self {
        Sort { value, loc }
    }
}

impl Debug for Sort {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.value)?;
        Ok(())
    }
}

impl Display for Sort {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

#[derive(Debug)]
pub enum SortEnum {
    Bool,
    Pos,
    Nat,
    Int,
    Real,
    List {
        subsort: Arc<Sort>,
    },
    Set {
        subsort: Arc<Sort>,
    },
    Bag {
        subsort: Arc<Sort>,
    },
    FSet {
        subsort: Arc<Sort>,
    },
    FBag {
        subsort: Arc<Sort>,
    },
    Id {
        id: Identifier,
    },
    Struct {
        constructors: Vec<Constructor>,
    },
    Carthesian {
        lhs: Arc<Sort>,
        rhs: Arc<Sort>,
    },
    Function {
        lhs: Arc<Sort>,
        rhs: Arc<Sort>,
    },
}

/// A constructor for a structured type.
/// 
/// This is written in mCRL2 as `id(name1: Sort1, ...) ? recognizer`
#[derive(Debug)]
pub struct Constructor {
    pub id: Identifier,
    pub properties: Vec<(Option<Identifier>, Arc<Sort>)>,
    pub recognizer_function_id: Option<Identifier>,
}

impl Parseable for Sort {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_sort(parser)
    }
}

/// Parses a sort.
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/data.html#grammar-token--3
pub fn parse_sort(parser: &mut Parser) -> Result<Sort, ParseError> {
    // the grammar is not very clear, but -> binds less strong than #
    // also note that -> is right-associative
    let loc = parser.get_loc();
    let lhs = parse_carthesian_sort(parser)?;

    if parser.skip_if_equal(&LexicalElement::Arrow) {
        let rhs = parser.parse::<Sort>()?;
        Ok(Sort::new(
            SortEnum::Function { lhs: Arc::new(lhs), rhs: Arc::new(rhs) },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_carthesian_sort(parser: &mut Parser) -> Result<Sort, ParseError> {
    // note that it's associative
    let loc = parser.get_loc();
    let lhs = parse_basic_sort(parser)?;

    if parser.skip_if_equal(&LexicalElement::HashSign) {
        let rhs = parse_carthesian_sort(parser)?;
        Ok(Sort::new(
            SortEnum::Carthesian { lhs: Arc::new(lhs), rhs: Arc::new(rhs) },
            parser.until_now(&loc),
        ))
    } else {
        Ok(lhs)
    }
}

fn parse_basic_sort(parser: &mut Parser) -> Result<Sort, ParseError> {
    let token = parser.get_token();
    let loc = token.loc;

    let result = match &token.value {
        LexicalElement::Bool => {
            parser.skip_token();
            Ok(Sort::new(SortEnum::Bool, loc))
        },
        LexicalElement::Pos => {
            parser.skip_token();
            Ok(Sort::new(SortEnum::Pos, loc))
        },
        LexicalElement::Nat => {
            parser.skip_token();
            Ok(Sort::new(SortEnum::Nat, loc))
        },
        LexicalElement::Int => {
            parser.skip_token();
            Ok(Sort::new(SortEnum::Int, loc))
        },
        LexicalElement::Real => {
            parser.skip_token();
            Ok(Sort::new(SortEnum::Real, loc))
        },
        LexicalElement::List => {
            parser.skip_token();
            parser.expect_token(&LexicalElement::OpeningParen)?;
            let subsort = Arc::new(parser.parse::<Sort>()?);
            parser.expect_token(&LexicalElement::ClosingParen)?;
            Ok(Sort::new(SortEnum::List { subsort }, parser.until_now(&loc)))
        },
        LexicalElement::Set => {
            parser.skip_token();
            parser.expect_token(&LexicalElement::OpeningParen)?;
            let subsort = Arc::new(parser.parse::<Sort>()?);
            parser.expect_token(&LexicalElement::ClosingParen)?;
            Ok(Sort::new(SortEnum::Set { subsort }, parser.until_now(&loc)))
        },
        LexicalElement::Bag => {
            parser.skip_token();
            parser.expect_token(&LexicalElement::OpeningParen)?;
            let subsort = Arc::new(parser.parse::<Sort>()?);
            parser.expect_token(&LexicalElement::ClosingParen)?;
            Ok(Sort::new(SortEnum::Bag { subsort }, parser.until_now(&loc)))
        },
        LexicalElement::FSet => {
            parser.skip_token();
            parser.expect_token(&LexicalElement::OpeningParen)?;
            let subsort = Arc::new(parser.parse::<Sort>()?);
            parser.expect_token(&LexicalElement::ClosingParen)?;
            Ok(Sort::new(SortEnum::FSet { subsort }, parser.until_now(&loc)))
        },
        LexicalElement::FBag => {
            parser.skip_token();
            parser.expect_token(&LexicalElement::OpeningParen)?;
            let subsort = Arc::new(parser.parse::<Sort>()?);
            parser.expect_token(&LexicalElement::ClosingParen)?;
            Ok(Sort::new(SortEnum::FBag { subsort }, parser.until_now(&loc)))
        },
        LexicalElement::Identifier(id) => {
            let s = Ok(Sort::new(
                SortEnum::Id { id: Identifier::new(id) },
                parser.until_now(&loc),
            ));
            parser.skip_token();
            s
        },
        LexicalElement::OpeningParen => {
            parser.skip_token();
            let mut sort = parser.parse::<Sort>()?;
            parser.expect_token(&LexicalElement::ClosingParen)?;
            sort.loc = parser.until_now(&loc);
            Ok(sort)
        },
        LexicalElement::Struct => {
            parser.skip_token();
            let constructors = parse_constructor_list(parser)?;
            Ok(Sort::new(
                SortEnum::Struct { constructors },
                parser.until_now(&loc),
            ))
        },
        _ => {
            let s = Err(ParseError::expected("a sort", token));
            parser.skip_token();
            s
        }
    };
    result
}

fn parse_constructor_list(parser: &mut Parser) -> Result<Vec<Constructor>, ParseError> {
    let mut constructors = Vec::new();

    constructors.push(parse_constructor(parser)?);
    while parser.skip_if_equal(&LexicalElement::Pipe) {
        constructors.push(parse_constructor(parser)?);
    }

    Ok(constructors)
}

fn parse_constructor(parser: &mut Parser) -> Result<Constructor, ParseError> {
    let id = parser.parse_identifier()?;

    let mut properties = Vec::new();
    if parser.skip_if_equal(&LexicalElement::OpeningParen) {
        while {
            let id = if parser.is_next_token(&LexicalElement::Colon) {
                let id = parser.parse_identifier()?;
                parser.expect_token(&LexicalElement::Colon).unwrap();
                Some(id)
            } else {
                None
            };
            let sort = parser.parse::<Sort>()?;

            properties.push((id, Arc::new(sort)));

            parser.skip_if_equal(&LexicalElement::Comma)
        } {}

        parser.expect_token(&LexicalElement::ClosingParen)?;
    }

    let recognizer_function_id = if parser.skip_if_equal(&LexicalElement::QuestionMark) {
        Some(parser.parse_identifier()?)
    } else {
        None
    };

    Ok(Constructor { id, properties, recognizer_function_id })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::unwrap_pattern;
    use crate::core::lexer::tokenize;

    #[test]
    fn test_parse_sort_struct_basic() {
        let tokens = tokenize("struct a | x(g: Int, h: Bag(FBag(Nat))) | y | z(Int) ? is_z").unwrap();
        let sort = Parser::new(&tokens).parse::<Sort>().unwrap();

        let constructors = unwrap_pattern!(sort.value, SortEnum::Struct { constructors } => constructors);
        assert_eq!(constructors.len(), 4);
        assert_eq!(constructors[0].id.get_value(), "a");
        assert_eq!(constructors[0].properties.len(), 0);
        assert!(constructors[0].recognizer_function_id.is_none());

        assert_eq!(constructors[1].id.get_value(), "x");
        assert_eq!(constructors[1].properties[0].0, Some(Identifier::new("g")));
    }

    #[test]
    fn test_parse_sort_struct_empty() {
        // let tokens = tokenize("struct").unwrap();
        // let result = Parser::new(&tokens).parse_sort();
        // assert!(result.is_err());

        let tokens = tokenize("struct a()").unwrap();
        let result = Parser::new(&tokens).parse::<Sort>();
        assert!(result.is_err());
    }
}
