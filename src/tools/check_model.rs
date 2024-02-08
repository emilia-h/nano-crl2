
use crate::try_into;
use crate::analysis::context::AnalysisContext;
use crate::analysis::semantic::ir_conversion::query_module_ast_mapping;
use crate::core::error::Mcrl2Error;
use crate::core::lexer::tokenize;
use crate::core::parser::Parser;
use crate::model::module::Module;
use crate::tools::cli::CliOptions;

use std::fs::read_to_string;
use std::path::Path;

pub fn check_model(options: &CliOptions) -> Result<(), Mcrl2Error> {
    let input_files = options.get_named_list("input");

    let mut context = AnalysisContext::new();
    let mut module_ids = Vec::new();
    for input_file in input_files {
        let input = read_to_string(input_file)?;

        let tokens = try_into!(tokenize(&input));
        let ast_module = try_into!(Parser::new(&tokens).parse::<Module>());

        // derive module name from path
        let module_name_os = match Path::new(input_file).file_stem() {
            Some(name) => name,
            None => return Err(Mcrl2Error::IoError {
                message: String::from("File path does not end in a valid file name"),
                path: Some(input_file.clone()),
            }),
        };
        let module_name = match module_name_os.to_str() {
            Some(name) => name,
            None => return Err(Mcrl2Error::IoError {
                message: String::from("File path is not valid unicode"),
                path: Some(input_file.clone())
            }),
        };

        module_ids.push(context.add_ast_module(module_name.to_owned(), ast_module));
    }

    for module_id in module_ids {
        let _ = query_module_ast_mapping(&context, module_id);
    }

    Ok(())
}
