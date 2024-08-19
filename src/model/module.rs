//! Defines AST types for mCRL2 models.
//! 
//! # See also
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/mcrl2.html).

use crate::core::parser::{Parseable, ParseError, Parser};
use crate::core::syntax::SourceRange;
use crate::model::decl::{Decl, DeclEnum};
use crate::model::display::display_pretty_default;
use crate::model::proc::Proc;

use std::fmt::{Display, Formatter};
use std::sync::Arc;

/// An mCRL2 model, which consists of a set of declarations along with an
/// initial process that may instantiate these declarations.
#[derive(Debug)]
pub struct Module {
    pub decls: Vec<Arc<Decl>>,
    pub initial: Option<Arc<Proc>>,
    pub loc: SourceRange,
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

impl Parseable for Module {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_module(parser)
    }
}

/// Parses a full mCRL2 model module.
/// 
/// These mCRL2 files usually have the `.mcrl2` extension.
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/mcrl2.html#grammar-token-mCRL2Spec
pub fn parse_module(parser: &mut Parser) -> Result<Module, ParseError> {
    let mut decls = Vec::new();
    let mut initial = None;

    let loc = if parser.has_token() {
        parser.get_loc()
    } else {
        SourceRange::new(0, 0, 0, 0)
    };

    while parser.has_token() {
        for decl in parser.parse::<Vec<Decl>>()? {
            if let DeclEnum::Initial { value } = decl.value {
                if initial.is_some() {
                    let message = "Cannot have more than one `init` declaration in the same model";
                    return Err(ParseError::new(String::from(message), value.loc));
                }
                initial = Some(value);
            } else {
                decls.push(Arc::new(decl));
            }
        }
    }

    Ok(Module {
        decls,
        initial,
        loc,
    })
}
