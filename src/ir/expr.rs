
use crate::core::syntax::{Identifier, SourceRange};
use crate::ir::decl::DefId;
use crate::ir::module::ModuleId;
use crate::ir::sort::SortId;

use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub struct IrExpr {
    pub value: IrExprEnum,
    pub loc: SourceRange,
}

#[derive(Debug)]
pub enum IrExprEnum {
    Name {
        identifier: Identifier,
    },
    NumberLiteral {
        // TODO should actually allow arbitrary-size integers
        value: u64,
    },
    BoolLiteral {
        value: bool,
    },
    ListLiteral {
        values: Vec<ExprId>,
    },
    SetLiteral {
        values: Vec<ExprId>,
    },
    BagLiteral {
        values: Vec<(ExprId, ExprId)>,
    },
    FunctionUpdate {
        function: ExprId,
        lhs: ExprId,
        rhs: ExprId,
    },
    Apply {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    /// An expression of the form `<op> identifier: sort . expr` where \<op\>
    /// is one of `exists`, `forall`, `lambda`, or `set` (of the form
    /// `{ ... | ... }`).
    Binder {
        op: BinderExprOp,
        def_id: DefId,
        identifier: Identifier,
        identifier_loc: SourceRange,
        sort: SortId,
        expr: ExprId,
    },
    Unary {
        op: UnaryExprOp,
        value: ExprId,
    },
    Binary {
        op: BinaryExprOp,
        lhs: ExprId,
        rhs: ExprId,
    },
    If {
        condition: ExprId,
        then_expr: ExprId,
        else_expr: ExprId,
    },
    Where {
        def_id: DefId,
        identifier: Identifier,
        identifier_loc: SourceRange,
        inner: ExprId,
        assigned: ExprId,
    },
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryExprOp {
    LogicalNot,
    Negate,
    Count,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinderExprOp {
    Forall,
    Exists,
    Lambda,
    SetComprehension,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryExprOp {
    Implies,
    LogicalOr,
    LogicalAnd,
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    In,
    Cons,
    Snoc,
    Concat,
    Add,
    Subtract,
    Divide,
    IntegerDivide,
    Mod,
    Multiply,
    Index,
}

#[derive(Debug)]
pub struct IrRewriteSet {
    pub variables: Vec<IrRewriteVar>,
    pub rules: Vec<IrRewriteRule>,
    pub loc: SourceRange,
}

#[derive(Debug)]
pub struct IrRewriteVar {
    pub def_id: DefId,
    pub identifier: Identifier,
    pub identifier_loc: SourceRange,
    pub sort: SortId,
    pub loc: SourceRange,
}

#[derive(Debug)]
pub struct IrRewriteRule {
    pub condition: Option<ExprId>,
    pub lhs: ExprId,
    pub rhs: ExprId,
    pub loc: SourceRange,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ExprId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl ExprId {
    pub fn get_module_id(&self) -> ModuleId {
        self.module
    }
}

impl Debug for ExprId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.expr.{}", self.module, self.value)
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct RewriteSetId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

impl RewriteSetId {
    pub fn get_module_id(&self) -> ModuleId {
        self.module
    }
}

impl Debug for RewriteSetId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.eqn.{}", self.module, self.value)
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct RewriteVarId {
    pub(crate) rewrite_set: RewriteSetId,
    pub(crate) index: usize,
}

impl RewriteVarId {
    pub fn get_module_id(&self) -> ModuleId {
        self.rewrite_set.module
    }
}

impl Debug for RewriteVarId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.rewritevar.{}", self.rewrite_set, self.index)
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct RewriteRuleId {
    pub(crate) rewrite_set: RewriteSetId,
    pub(crate) index: usize,
}

impl RewriteRuleId {
    pub fn get_module_id(&self) -> ModuleId {
        self.rewrite_set.module
    }
}

impl Debug for RewriteRuleId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}.rewriterule.{}", self.rewrite_set, self.index)
    }
}
