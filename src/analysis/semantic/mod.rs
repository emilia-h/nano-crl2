//! These modules implement semantic analysis.
//! 
//! Semantic analysis is figuring out what code means (semantics), rather than
//! what it is written like (syntax). It performs analysis such as:
//! 
//! - IR conversion: converting from an AST to an IR.
//! - name resolution: finding what declaration an identifier belongs to in a
//!   given context.
//! - type resolution: finding what type (or in mCRL2, "sort") a given
//!   expression has.
//! - typechecking: finding if types match correctly, emitting an error otherwise.

pub mod compilation;
pub mod name_resolution;
pub mod sort_resolution;
