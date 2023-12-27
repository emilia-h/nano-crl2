
use crate::try_into;
use crate::core::error::Mcrl2Error;
use crate::parser::lexer::{tokenize, reconstruct_from_tokens};
use crate::parser::parser::Parser;
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

pub fn gen_docs(options: &CliOptions) -> Result<Docs, Mcrl2Error> {
    let input_files = options.get_named_list("input");
    let output_file = try_into!(options.get_named_string("output"));

    for input_file in input_files {
        let string = std::fs::read_to_string(input_file)?;
        let tokens = try_into!(tokenize(&string));
        let mut parser = Parser::new(&tokens);
        let _model = try_into!(parser.parse_model());

        std::fs::write(output_file, reconstruct_from_tokens(&tokens))?;
    }

    Ok(Docs {})
}
