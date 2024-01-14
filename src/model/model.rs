//! Defines AST types for mCRL2 models.
//! 
//! # See also
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/mcrl2.html).

use crate::core::parser::{Parseable, ParseError, Parser};
use crate::model::decl::{Decl, DeclEnum};
use crate::model::display::display_pretty_default;
use crate::model::proc::Proc;

use std::fmt::{Display, Formatter};
use std::rc::Rc;

/// An mCRL2 model, which consists of a set of declarations along with an
/// initial process that may instantiate these declarations.
#[derive(Debug)]
pub struct Model {
    pub decls: Vec<Rc<Decl>>,
    pub initial: Option<Rc<Proc>>,
}

impl Display for Model {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        display_pretty_default(self, f)
    }
}

impl Parseable for Model {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_model(parser)
    }
}

/// Parses a full mCRL2 model.
/// 
/// These mCRL2 models usually have the `.mcrl2` extension.
/// 
/// # See also
/// The [mCRL2 grammar on this].
/// 
/// [mCRL2 grammar on this]: https://www.mcrl2.org/web/user_manual/language_reference/mcrl2.html#grammar-token-mCRL2Spec
pub fn parse_model(parser: &mut Parser) -> Result<Model, ParseError> {
    let mut decls = Vec::new();
    let mut initial = None;

    while parser.has_token() {
        for decl in parser.parse::<Vec<Decl>>()? {
            if let DeclEnum::InitialDecl { value } = decl.value {
                if initial.is_some() {
                    let message = "Cannot have more than one `init` declaration in the same model";
                    return Err(ParseError::new(String::from(message), value.loc));
                }
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
