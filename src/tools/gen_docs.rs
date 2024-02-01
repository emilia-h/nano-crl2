
use crate::try_into;
use crate::core::error::Mcrl2Error;
use crate::core::lexer::tokenize;
use crate::core::parser::Parser;
use crate::model::module::Module;
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
        let module = try_into!(parser.parse::<Module>());

        std::fs::write(output_file, format!("{}", module))?;
    }

    Ok(Docs {})
}
