//! Defines AST types for processes, which are essentially programs that are
//! defined in terms of states (which is a tuple of data elements such as
//! numbers, lists and sets) and steps (actions) that move from one state to
//! another.
//! 
//! These steps can be either deterministic or non-deterministic.
//! 
//! # See also
//! The [mCRL2 spec on this](https://mcrl2.org/web/user_manual/language_reference/process.html).

use crate::ast::decl::VariableDecl;
use crate::ast::expr::Expr;
use crate::ast::node::AstNode;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Display, Formatter};
use std::rc::{Rc, Weak};

use super::display::display_pretty_default;

/// A process expression in a model.
pub struct Proc {
    pub value: ProcEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl Proc {
    /// Creates a new process with `parent` set to `None`.
    pub fn new(value: ProcEnum, loc: SourceLocation) -> Self {
        Proc { value, loc, parent: None }
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
        value: Action,
    },
    // NOTE: at parse time, it's not really possible to distinguish between
    // actions and named processes
    // Id {
    //     id: Identifier,
    //     args: Vec<Rc<Expr>>,
    // },
    Delta,
    Tau,
    Block {
        ids: Vec<Identifier>,
        proc: Rc<Proc>,
    },
    Allow {
        multi_ids: Vec<Vec<Identifier>>,
        proc: Rc<Proc>,
    },
    Hide {
        ids: Vec<Identifier>,
        proc: Rc<Proc>,
    },
    Rename {
        mappings: Vec<RenameMapping>,
        proc: Rc<Proc>,
    },
    Comm {
        mappings: Vec<CommMapping>,
        proc: Rc<Proc>,
    },
    Add {
        lhs: Rc<Proc>,
        rhs: Rc<Proc>,
    },
    Sum {
        variables: Vec<VariableDecl>,
        proc: Rc<Proc>,
    },
    Parallel {
        lhs: Rc<Proc>,
        rhs: Rc<Proc>,
    },
    RightParallel {
        lhs: Rc<Proc>,
        rhs: Rc<Proc>,
    },
    Multi { // a | b
        lhs: Rc<Proc>,
        rhs: Rc<Proc>,
    },
    IfThenElse {
        condition: Rc<Expr>,
        then_proc: Rc<Proc>,
        else_proc: Option<Rc<Proc>>,
    },
    // TODO what is "<<" in the spec???
    LeftShift {
        lhs: Rc<Proc>,
        rhs: Rc<Proc>,
    },
    Concat {
        lhs: Rc<Proc>,
        rhs: Rc<Proc>,
    },
    Time {
        proc: Rc<Proc>,
        time: Rc<Expr>,
    },
}

#[derive(Clone, Debug)]
pub struct Action {
    pub id: Identifier,
    pub args: Vec<Rc<Expr>>,
}

#[derive(Debug)]
pub struct CommMapping {
    pub lhs: Vec<Identifier>,
    pub rhs: Identifier,
}

#[derive(Debug)]
pub struct RenameMapping {
    pub lhs: Identifier,
    pub rhs: Identifier,
}
