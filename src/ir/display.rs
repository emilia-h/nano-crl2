
use crate::ir::iterator::get_def_data;
use crate::ir::module::IrModule;
use crate::ir::sort::ResolvedSort;

use std::fmt::{Display, Formatter};

pub struct ResolvedSortDisplay<'a, 'b> {
    module: &'b IrModule,
    sort: &'a ResolvedSort,
}

impl<'a, 'b> ResolvedSortDisplay<'a, 'b> {
    pub fn new(module: &'b IrModule, sort: &'a ResolvedSort) -> Self {
        ResolvedSortDisplay { module, sort }
    }
}

impl<'a, 'b> Display for ResolvedSortDisplay<'a, 'b> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        fmt_resolved_sort(self.module, self.sort, f)
    }
}

/// Prints a pretty version of a `ResolvedSort` to the provided formatter.
pub fn fmt_resolved_sort(
    module: &IrModule,
    sort: &ResolvedSort,
    f: &mut Formatter,
) -> Result<(), std::fmt::Error> {
    match sort {
        ResolvedSort::Primitive { sort } => write!(f, "{}", (*sort).as_str())?,

        ResolvedSort::Generic { op, subsort } => {
            write!(f, "{}(", (*op).as_str())?;
            fmt_resolved_sort(module, subsort, f)?;
            write!(f, ")")?;
        },
        ResolvedSort::Function { lhs, rhs } => {
            write!(f, "(")?;
            for (i, param_sort) in lhs.iter().enumerate() {
                if i > 0 {
                    write!(f, " # ")?;
                }
                fmt_resolved_sort(module, param_sort, f)?;
            }
            write!(f, " -> ")?;
            fmt_resolved_sort(module, rhs, f)?;
            write!(f, ")")?;
        },
        ResolvedSort::Def { id } => {
            let source = module.get_def_source(*id);
            if let Some((_, identifier, _)) = get_def_data(module, source) {
                write!(f, "{}", identifier.get_value())?;
            } else {
                write!(f, "[[unknown def]]")?;
            }
        },
    }
    Ok(())
}
