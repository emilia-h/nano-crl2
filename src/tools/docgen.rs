
use crate::propagate_error_into;
use crate::core::error::Mcrl2Error;
// use crate::core::model::Model;
use crate::parser::lexer::{tokenize, reconstruct_from_tokens};
use crate::parser::parser::Parser;
use crate::tools::cli::CliOptions;

use std::fmt::Display;

#[derive(Debug)]
pub struct Docs {

}

impl Display for Docs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "todo")?;
        Ok(())
    }
}

pub fn docgen(options: &CliOptions) -> Result<Docs, Mcrl2Error> {
    let input_files = options.get_named_list("input");
    let output_file = propagate_error_into!(options.get_named_string("output"));

    for input_file in input_files {
        let string = std::fs::read_to_string(input_file)?;
        let tokens = propagate_error_into!(tokenize(&string));
        let mut parser = Parser::new(&tokens);
        let model = propagate_error_into!(parser.parse_model());

        std::fs::write(output_file, reconstruct_from_tokens(&tokens))?;
    }

    Ok(Docs {})
}
