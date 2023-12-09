//! These modules define functions and structures for parsing mCRL2 models and
//! mu-calculus formulas.
//! 
//! Note that all parsing functionality can be found in
//! [`nano_crl2::parser::parser`]. Submodules such as `decl` and `expr` only
//! add extension methods onto the [`Parser`] struct.
//! 
//! [`nano_crl2::parser::parser`]: ./parser/index.html
//! [`Parser`]: ./parser/struct.Parser.html

pub mod decl;
pub mod expr;
pub mod formula;
pub mod lexer;
pub mod parser;
pub mod proc;
pub mod sort;
