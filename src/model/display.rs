
use crate::model::decl::{Decl, DeclEnum, VariableDecl};
use crate::model::expr::{Expr, ExprEnum};
use crate::model::module::Module;
use crate::model::proc::{Proc, ProcEnum};
use crate::model::sort::{Constructor, Sort, SortEnum};

use std::fmt::Formatter;

pub struct DisplayOptions<'a> {
    pub indentation: u32,
    pub indentation_string: &'a str,
    pub precedence: Precedence,
}

impl<'a> DisplayOptions<'a> {
    pub fn with_precedence(&self, precedence: Precedence) -> Self {
        DisplayOptions {
            indentation: self.indentation,
            indentation_string: self.indentation_string,
            precedence,
        }
    }

    pub fn indent(&self, delta_indentation: u32) -> Self {
        DisplayOptions {
            indentation: self.indentation + delta_indentation,
            indentation_string: self.indentation_string,
            precedence: self.precedence,
        }
    }
}

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    Comma,
    Where,
    Implies,
    Or,
    And,
    Cmp,
    Concat,
    Add,
    Sum,
    Multiply,
    Index,
    Unary,
    Apply,
}

pub trait DisplayPretty {
    #[must_use]
    fn fmt(
        &self,
        options: &DisplayOptions,
        f: &mut Formatter,
    ) -> Result<(), std::fmt::Error>;
}

static DEFAULT_DISPLAY_OPTIONS: DisplayOptions = DisplayOptions {
    indentation: 0,
    indentation_string: "    ",
    precedence: Precedence::Comma,
};

pub fn display_pretty_default<T: DisplayPretty>(value: &T, f: &mut Formatter) -> Result<(), std::fmt::Error> {
    DisplayPretty::fmt(value, &DEFAULT_DISPLAY_OPTIONS, f)
}

impl DisplayPretty for Decl {
    fn fmt(
        &self,
        options: &DisplayOptions,
        f: &mut Formatter,
    ) -> Result<(), std::fmt::Error> {
        use DeclEnum::*;

        match &self.value {
            Action { ids, sort } => {
                write!(f, "act ")?;
                display_separated(
                    ids, ", ",
                    &|id, f| write!(f, "{}", id),
                    f,
                )?;
                if let Some(sort) = sort {
                    write!(f, ": ")?;
                    sort.fmt(options, f)?;
                }
                write!(f, ";\n")?;
            },
            Constructor { ids, sort } => {
                write!(f, "cons ")?;
                display_separated(
                    ids, ", ",
                    &|id, f| write!(f, "{}", id),
                    f,
                )?;
                write!(f, ": ")?;
                sort.fmt(options, f)?;
                write!(f, ";\n")?;
            },
            EquationSet { variables, equations } => {
                if variables.len() > 0 {
                    write!(f, "var ")?;
                    for (i, variable_decl) in variables.iter().enumerate() {
                        let new_options = options.with_precedence(Precedence::Comma)
                            .indent((i > 0) as u32);
                        if i > 0 {
                            display_indentation(&new_options, f)?;
                        }

                        display_separated(
                            &variable_decl.ids, ", ",
                            &|id, f| write!(f, "{}", id),
                            f,
                        )?;
                        write!(f, ": ")?;
                        variable_decl.sort.fmt(&new_options, f)?;
                        write!(f, ";\n")?;
                    }
                }

                write!(f, "eqn ")?;
                for (i, equation) in equations.iter().enumerate() {
                    let new_options = options.with_precedence(Precedence::Comma)
                        .indent((i > 0) as u32);
                    if i > 0 {
                        display_indentation(&new_options, f)?;
                    }

                    if let Some(condition) = &equation.condition {
                        condition.fmt(&new_options, f)?;
                        write!(f, " -> ")?;
                    }
                    equation.lhs.fmt(&new_options, f)?;
                    write!(f, " = ")?;
                    equation.rhs.fmt(&new_options, f)?;
                    write!(f, ";\n")?;
                }
            },
            GlobalVariable { variables } => {
                write!(f, "glob ")?;
                display_var_decl_list(variables, options, f)?;
                write!(f, ";\n")?;
            },
            Initial { value } => {
                write!(f, "init ")?;
                value.fmt(&options.indent(1), f)?;
                write!(f, ";\n")?;
            },
            Map { id, sort } => {
                write!(f, "map {}: ", id)?;
                sort.fmt(options, f)?;
                write!(f, ";\n")?;
            },
            Process { id, params, proc: process } => {
                let new_options = options.indent(1);
                write!(f, "proc {}", id)?;
                if params.len() > 0 {
                    write!(f, "(")?;
                    display_var_decl_list(params, &new_options, f)?;
                    write!(f, ")")?;
                }

                write!(f, " =\n")?;
                display_indentation(&new_options, f)?;
                process.fmt(&new_options, f)?;
                write!(f, ";\n")?;
            },
            Sort { ids, sort: value } => {
                write!(f, "sort ")?;
                display_separated(
                    ids, ", ",
                    &|id, f| write!(f, "{}", id),
                    f,
                )?;
                if let Some(value) = value {
                    write!(f, " = ")?;
                    value.fmt(options, f)?;
                }
                write!(f, ";\n")?;
            },
        }
        Ok(())
    }
}

impl DisplayPretty for Expr {
    fn fmt(
        &self,
        options: &DisplayOptions,
        f: &mut Formatter,
    ) -> Result<(), std::fmt::Error> {
        use ExprEnum::*;
    
        match &self.value {
            Id { id } => {
                write!(f, "{}", id)?;
            },
            Number { value } => {
                write!(f, "{}", value)?;
            },
            Bool { value } => {
                write!(f, "{}", value)?;
            },
            List { values } => {
                write!(f, "[")?;
                display_separated(
                    &values, ", ",
                    &|expr, f| expr.fmt(options, f),
                    f,
                )?;
                write!(f, "]")?;
            },
            Set { values } => {
                write!(f, "{{")?;
                display_separated(
                    values, ", ",
                    &|expr, f| expr.fmt(options, f),
                    f,
                )?;
                write!(f, "}}")?;
            },
            Bag { values } => {
                write!(f, "{{")?;
                display_separated(
                    values, ", ",
                    &|pair, f| {
                        pair.0.fmt(&options.with_precedence(Precedence::Comma), f)?;
                        write!(f, " : ")?;
                        pair.1.fmt(&options.with_precedence(Precedence::Comma), f)
                    },
                    f,
                )?;
                write!(f, "}}")?;
            },
            SetComprehension { .. } => {
                todo!();
            }
            FunctionUpdate { function, lhs, rhs } => {
                function.fmt(options, f)?;
                write!(f, "[")?;
                lhs.fmt(&options.with_precedence(Precedence::Comma), f)?;
                write!(f, " -> ")?;
                rhs.fmt(&options.with_precedence(Precedence::Comma), f)?;
                write!(f, "]")?;
            },
            Apply { callee, args } => {
                callee.fmt(options, f)?;
                write!(f, "(")?;
                display_separated(
                    args, ", ",
                    &|arg, f| arg.fmt(&options.with_precedence(Precedence::Comma), f),
                    f,
                )?;
                write!(f, ")")?;
            },
            LogicalNot { value } => {
                if options.precedence <= Precedence::Unary {
                    write!(f, "(")?;
                }
                write!(f, "!")?;
                value.fmt(&options.with_precedence(Precedence::Unary), f)?;
                if options.precedence <= Precedence::Unary {
                    write!(f, ")")?;
                }
            },
            Negate { value } => {
                if options.precedence <= Precedence::Unary {
                    write!(f, "(")?;
                }
                write!(f, "-")?;
                value.fmt(&options.with_precedence(Precedence::Unary), f)?;
                if options.precedence <= Precedence::Unary {
                    write!(f, ")")?;
                }
            },
            Count { value } => {
                if options.precedence <= Precedence::Unary {
                    write!(f, "(")?;
                }
                write!(f, "#")?;
                value.fmt(&options.with_precedence(Precedence::Unary), f)?;
                if options.precedence <= Precedence::Unary {
                    write!(f, ")")?;
                }
            },
            Forall { variables, expr } => {
                write!(f, "forall ")?;
                display_var_decl_list(variables, options, f)?;
                write!(f, " . ")?;
                expr.fmt(options, f)?;
            },
            Exists { variables, expr } => {
                write!(f, "exists ")?;
                display_var_decl_list(variables, options, f)?;
                write!(f, " . ")?;
                expr.fmt(options, f)?;
            },
            Lambda { variables, expr } => {
                write!(f, "lambda ")?;
                display_var_decl_list(variables, options, f)?;
                write!(f, " . ")?;
                expr.fmt(options, f)?;
            },
            Implies { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " => ", Precedence::Implies, options, f)?;
            },
            LogicalOr { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " || ", Precedence::Or, options, f)?;
            },
            LogicalAnd { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " && ", Precedence::And, options, f)?;
            },
            Equals { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " == ", Precedence::Cmp, options, f)?;
            },
            NotEquals { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " != ", Precedence::Cmp, options, f)?;
            },
            LessThan { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " < ", Precedence::Cmp, options, f)?;
            },
            LessThanEquals { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " <= ", Precedence::Cmp, options, f)?;
            },
            GreaterThan { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " > ", Precedence::Cmp, options, f)?;
            },
            GreaterThanEquals { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " >= ", Precedence::Cmp, options, f)?;
            },
            In { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " in ", Precedence::Cmp, options, f)?;
            },
            Cons { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " |> ", Precedence::Concat, options, f)?;
            },
            Snoc { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " <| ", Precedence::Concat, options, f)?;
            },
            Concat { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " ++ ", Precedence::Concat, options, f)?;
            },
            Add { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " + ", Precedence::Add, options, f)?;
            },
            Subtract { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " - ", Precedence::Add, options, f)?;
            },
            Divide { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " <= ", Precedence::Multiply, options, f)?;
            },
            IntegerDivide { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " div ", Precedence::Multiply, options, f)?;
            },
            Mod { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " mod ", Precedence::Multiply, options, f)?;
            },
            Multiply { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " * ", Precedence::Multiply, options, f)?;
            },
            Index { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " . ", Precedence::Index, options, f)?;
            },
            Where { expr, assignments } => {
                if options.precedence <= Precedence::Where {
                    write!(f, "(")?;
                }
                expr.fmt(&options.with_precedence(Precedence::Where), f)?;
                write!(f, " whr\n")?;
                for assignment in assignments {
                    // TODO fix: comma-separated
                    write!(f, "{} = ", assignment.0)?;
                    assignment.1.fmt(&options.with_precedence(Precedence::Where), f)?;
                }
                if options.precedence <= Precedence::Where {
                    write!(f, ")")?;
                }
            },
        }
        Ok(())
    }
}

impl DisplayPretty for Module {
    fn fmt(
        &self,
        options: &DisplayOptions,
        f: &mut Formatter,
    ) -> Result<(), std::fmt::Error> {
        for decl in &self.decls {
            decl.fmt(options, f)?;
            write!(f, "\n")?;
        }
        if let Some(initial) = &self.initial {
            write!(f, "init ")?;
            initial.fmt(options, f)?;
            write!(f, ";\n")?;
        }
        Ok(())
    }
}

impl DisplayPretty for Proc {
    fn fmt(
        &self,
        options: &DisplayOptions,
        f: &mut Formatter,
    ) -> Result<(), std::fmt::Error> {
        use ProcEnum::*;

        match &self.value {
            Action { value } => {
                if value.args.len() == 0 {
                    write!(f, "{}(", value.id)?;
                    display_separated(
                        &value.args, ", ",
                        &|arg, f| {
                            arg.fmt(&options.indent(1), f)
                        },
                        f,
                    )?;
                    write!(f, ")")?;
                } else {
                    write!(f, "{}", value.id)?;
                }
            },
            Delta => {
                write!(f, "delta")?;
            },
            Tau => {
                write!(f, "tau")?;
            },
            Block { ids, proc } => {
                display_unary_process_operator(
                    "block", &ids, proc,
                    &|item, f| write!(f, "{}", item),
                    options, f,
                )?;
            },
            Allow { multi_ids, proc } => {
                display_unary_process_operator(
                    "allow", &multi_ids, proc,
                    &|item, f| display_separated(
                        item, " | ",
                        &|id, f| write!(f, "{}", id),
                        f,
                    ),
                    options, f,
                )?;
            },
            Hide { ids, proc } => {
                display_unary_process_operator(
                    "hide", &ids, proc,
                    &|item, f| write!(f, "{}", item),
                    options, f,
                )?;
            },
            Rename { mappings, proc } => {
                display_unary_process_operator(
                    "rename", &mappings, proc,
                    &|item, f| write!(f, "{} -> {}", item.lhs, item.rhs),
                    options, f,
                )?;
            },
            Comm { mappings, proc } => {
                display_unary_process_operator(
                    "comm", &mappings, proc,
                    &|item, f| {
                        display_separated(
                            &item.lhs, " | ",
                            &|id, f| write!(f, "{}", id),
                            f,
                        )?;
                        write!(f, " -> {}", item.rhs)
                    },
                    options, f,
                )?;
            },
            Add { lhs: _, rhs: _ } => {
                write!(f, "{:?}", self)?;
            },
            Sum { variables: _, proc: _ } => {
                write!(f, "{:?}", self)?;
            },
            Parallel { lhs: _, rhs: _ } => {
                write!(f, "{:?}", self)?;
            },
            RightParallel { lhs: _, rhs: _ } => {
                write!(f, "{:?}", self)?;
            },
            Multi { lhs: _, rhs: _ } => {
                write!(f, "{:?}", self)?;
            },
            IfThenElse { condition: _, then_proc: _, else_proc: _ } => {
                write!(f, "{:?}", self)?;
            },
            LeftShift { lhs: _, rhs: _ } => {
                write!(f, "{:?}", self)?;
            },
            Concat { lhs: _, rhs: _ } => {
                write!(f, "{:?}", self)?;
            },
            Time { proc: _, time: _ } => {
                write!(f, "{:?}", self)?;
            },
        }
        Ok(())
    }
}

impl DisplayPretty for Sort {
    fn fmt(
        &self,
        options: &DisplayOptions,
        f: &mut Formatter,
    ) -> Result<(), std::fmt::Error> {
        use SortEnum::*;

        match &self.value {
            Bool => {
                write!(f, "Bool")?;
            },
            Pos => {
                write!(f, "Pos")?;
            },
            Nat => {
                write!(f, "Nat")?;
            },
            Int => {
                write!(f, "Int")?;
            },
            Real => {
                write!(f, "Real")?;
            },
            List { subsort } => {
                write!(f, "List(")?;
                subsort.fmt(&options.with_precedence(Precedence::Comma), f)?;
                write!(f, ")")?;
            },
            Set { subsort } => {
                write!(f, "Set(")?;
                subsort.fmt(&options.with_precedence(Precedence::Comma), f)?;
                write!(f, ")")?;
            },
            Bag { subsort } => {
                write!(f, "Bag(")?;
                subsort.fmt(&options.with_precedence(Precedence::Comma), f)?;
                write!(f, ")")?;
            },
            FSet { subsort } => {
                write!(f, "FSet(")?;
                subsort.fmt(&options.with_precedence(Precedence::Comma), f)?;
                write!(f, ")")?;
            },
            FBag { subsort } => {
                write!(f, "FBag(")?;
                subsort.fmt(&options.with_precedence(Precedence::Comma), f)?;
                write!(f, ")")?;
            },
            Id { id } => {
                write!(f, "{}", id)?;
            },
            Struct { constructors } => {
                write!(f, "struct ")?;
                display_separated(
                    constructors, " | ",
                    &|constructor, f| display_constructor(constructor, options, f),
                    f,
                )?;
            },
            Carthesian { lhs, rhs } => {
                display_binary_op(&**lhs, &**rhs, " # ", Precedence::Multiply, options, f)?;
            },
            Function { lhs, rhs } => {
                // choice of `Precedence` is a bit arbitrary
                display_binary_op(&**lhs, &**rhs, " -> ", Precedence::Concat, options, f)?;
            },
        }
        Ok(())
    }
}

fn display_indentation(
    options: &DisplayOptions,
    f: &mut Formatter,
) -> Result<(), std::fmt::Error> {
    for _ in 0 .. options.indentation {
        write!(f, "{}", options.indentation_string)?;
    }
    Ok(())
}

fn display_separated<T, F>(
    values: &[T],
    separator: &str,
    writer: &F,
    f: &mut Formatter,
) -> Result<(), std::fmt::Error>
where
    F: Fn(&T, &mut Formatter) -> Result<(), std::fmt::Error>,
{
    for (i, value) in values.iter().enumerate() {
        if i > 0 {
            write!(f, "{}", separator)?;
        }
        writer(&value, f)?;
    }
    Ok(())
}

fn display_binary_op<T: DisplayPretty>(
    lhs: &T,
    rhs: &T,
    separator: &str,
    precedence: Precedence,
    options: &DisplayOptions,
    f: &mut Formatter,
) -> Result<(), std::fmt::Error> {
    if options.precedence >= precedence {
        write!(f, "(")?;
    }
    lhs.fmt(&options.with_precedence(precedence), f)?;
    write!(f, "{}", separator)?;
    rhs.fmt(&options.with_precedence(precedence), f)?;
    if options.precedence >= precedence {
        write!(f, ")")?;
    }
    Ok(())
}

fn display_var_decl_list(
    variables: &[VariableDecl],
    options: &DisplayOptions,
    f: &mut Formatter,
) -> Result<(), std::fmt::Error> {
    for (i, variable_decl) in variables.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        display_separated(
            &variable_decl.ids, ", ",
            &|id, f| write!(f, "{}", id),
            f,
        )?;
        write!(f, ": ")?;
        variable_decl.sort.fmt(options, f)?;
    }
    Ok(())
}

fn display_unary_process_operator<T, F>(
    op: &str,
    list: &[T],
    proc: &Proc,
    writer: &F,
    options: &DisplayOptions,
    f: &mut Formatter,
) -> Result<(), std::fmt::Error>
where
    F: Fn(&T, &mut Formatter) -> Result<(), std::fmt::Error>,
{
    let new_options = options.indent(1);
    write!(f, "{}(\n", op)?;
    display_indentation(&new_options, f)?;
    write!(f, "{{\n")?;

    let new_options2 = options.indent(2);
    for (i, item) in list.iter().enumerate() {
        display_indentation(&new_options2, f)?;
        writer(item, f)?;

        if i < list.len() - 1 {
            write!(f, ",\n")?;
        } else {
            write!(f, "\n")?;
        }
    }

    display_indentation(&new_options, f)?;
    write!(f, "}},\n")?;
    display_indentation(&new_options, f)?;
    proc.fmt(&new_options, f)?;
    write!(f, "\n")?;
    display_indentation(options, f)?;
    write!(f, ")")?;
    Ok(())
}

fn display_constructor(
    constructor: &Constructor,
    options: &DisplayOptions,
    f: &mut Formatter,
) -> Result<(), std::fmt::Error> {
    let new_options = options.with_precedence(Precedence::Comma);

    write!(f, "{}", constructor.id)?;
    if constructor.properties.len() > 0 {
        write!(f, "(")?;
        display_separated(
            &constructor.properties, ", ",
            &|(projector, sort), f| {
                if let Some(p) = projector {
                    write!(f, "{}: ", p)?;
                }
                sort.fmt(&new_options, f)
            },
            f,
        )?;
        write!(f, ")")?;
    }
    if let Some(id) = &constructor.recognizer_function_id {
        write!(f, " ? {}", id)?;
    }
    Ok(())
}
