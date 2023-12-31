//! These modules contain definitions for the abstract syntax tree (AST), which
//! is a tree-like representation of the syntax.
//! 
//! # See also
//! The [mCRL2 specification on models](https://mcrl2.org/web/user_manual/language_reference/mcrl2.html)
//! and the [mCRL2 specification on mu-calculus formulas](https://mcrl2.org/web/user_manual/language_reference/mucalc.html).

pub mod decl;
pub mod display;
pub mod expr;
pub mod formula;
pub mod model;
pub mod node;
pub mod sort;
pub mod proc;
