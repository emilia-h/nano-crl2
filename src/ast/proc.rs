
use crate::ast::node::AstNode;
use crate::core::syntax::{Identifier, SourceLocation};

use std::fmt::{Debug, Formatter};
use std::rc::{Rc, Weak};

pub struct Proc {
    pub value: ProcEnum,
    pub loc: SourceLocation,
    pub parent: Option<Weak<dyn AstNode>>,
}

impl Proc {
    pub fn new(value: ProcEnum, loc: SourceLocation) -> Self {
        Proc { value, loc, parent: None }
    }
}

impl Debug for Proc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.value)?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum ProcEnum {
    Concat {
        lhs: Rc<Proc>,
        rhs: Rc<Proc>,
    },
    Add {
        lhs: Rc<Proc>,
        rhs: Rc<Proc>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Action {
    pub id: Identifier,
}
