
use crate::core::diagnostic::{Diagnostic, DiagnosticContext, DiagnosticSeverity};
use crate::tools::cli::CliOptions;

use std::fmt::Display;

#[derive(Debug)]
pub struct Docs {
    // TODO
}

impl Display for Docs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "todo")?;
        Ok(())
    }
}

pub fn gen_docs(
    options: &CliOptions,
    diagnostics: &mut DiagnosticContext,
) -> Result<Docs, ()> {
    let input_files = options.get_named_list("input");
    let output_file = diagnostics.union_result(
        options.get_named_string("output")
    )?;

    // TODO
    diagnostics.push(Diagnostic {
        severity: DiagnosticSeverity::Error,
        file: None,
        loc: None,
        message: "unimplemented".to_owned(),
    });
    Err(())
}
