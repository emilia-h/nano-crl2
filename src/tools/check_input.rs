
use crate::analysis::context::AnalysisContext;
use crate::analysis::ir_conversion::module::query_ir_module;
use crate::core::diagnostic::{Diagnostic, DiagnosticContext, DiagnosticSeverity};
use crate::tools::cli::CliOptions;

use std::fs::read_to_string;
use std::path::Path;

pub fn check_input(
    options: &CliOptions,
    diagnostics: &mut DiagnosticContext,
) -> Result<(), ()> {
    let input_files = options.get_named_list("input");

    let contents = input_files.into_iter().map(|input_file| {
        let content = match read_to_string(input_file) {
            Ok(value) => value,
            Err(error) => return Err(Diagnostic {
                severity: DiagnosticSeverity::Error,
                file: Some(input_file.clone()),
                loc: None,
                message: error.to_string(),
            }),
        };

        let module_name_os = match Path::new(input_file).file_stem() {
            Some(name) => name,
            None => return Err(Diagnostic {
                severity: DiagnosticSeverity::Error,
                file: Some(input_file.clone()),
                loc: None,
                message: String::from("File path does not end in a valid file name"),
            }),
        };

        let module_name = match module_name_os.to_str() {
            Some(name) => name,
            None => return Err(Diagnostic {
                severity: DiagnosticSeverity::Error,
                file: Some(input_file.clone()),
                loc: None,
                message: String::from("File path is not valid unicode"),
            }),
        };

        Ok((content, module_name))
    }).collect::<Vec<_>>();

    let input_file_contents = diagnostics.union_results(contents.into_iter())?;

    let mut context = AnalysisContext::new();
    let mut module_ids = Vec::new();
    for (content, module_name) in input_file_contents {
        // derive module name from path
        module_ids.push(context.add_model_input(module_name.to_owned(), content));
    }

    for module_id in module_ids {
        let ir = query_ir_module(&context, module_id);
        eprintln!("{:#?}", ir);
    }

    Ok(())
}
