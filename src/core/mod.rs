//! These modules define the core structures that are used all throughout the
//! crate.
//! 
//! They are supposed to not depend on no other modules other than utility
//! modules. They are also as generic as possible.

#[macro_use]
pub mod diagnostic;
pub mod lexer;
pub mod parser;
pub mod state_set;
pub mod syntax;
