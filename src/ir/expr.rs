
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::module::ModuleId;
use crate::ir::sort::IrSort;

use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub struct IrExpr {
    pub value: IrExprEnum,
    pub loc: SourceRange,
}

#[derive(Debug)]
pub enum IrExprEnum {
    Name {
        id: Identifier,
    },
    Number {
        // TODO should actually allow arbitrary-size integers
        value: u64,
    },
    // TODO many more
    Apply {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    // TODO many more
    Forall {

    },
    Exists {

    },
    // TODO many more
}

#[derive(Debug)]
pub struct IrRewriteRule {
    pub variables: Vec<(Identifier, IrSort)>,
    pub condition: ExprId,
    pub lhs: ExprId,
    pub rhs: ExprId,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ExprId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl Debug for ExprId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.expr.{}", self.module, self.value)
    }
}
