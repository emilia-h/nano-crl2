
use crate::try_into;
use crate::analysis::semantic::ir_conversion::add_module_from_ast;
use crate::core::error::Mcrl2Error;
use crate::core::lexer::tokenize;
use crate::core::parser::Parser;
use crate::ir::translation_unit::TranslationUnit;
use crate::model::module::Module;
use crate::tools::cli::CliOptions;

use std::fs::read_to_string;

pub fn check_model(options: &CliOptions) -> Result<(), Mcrl2Error> {
    let input_files = options.get_named_list("input");

    let translation_unit = TranslationUnit::new_rc();
    for input_file in input_files {
        let input = read_to_string(input_file)?;
        let tokens = try_into!(tokenize(&input));
        let module_ast = try_into!(Parser::new(&tokens).parse::<Module>());
        add_module_from_ast(&translation_unit, &module_ast);
    }

    Ok(())
}
