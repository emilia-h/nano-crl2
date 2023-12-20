
use crate::core::state_set::StateSetManager;
use crate::try_into;
use crate::core::error::Mcrl2Error;
use crate::lts::verification::calculate_set;
use crate::lts::lts::read_aldebaran_file;
use crate::parser::lexer::tokenize;
use crate::parser::parser::Parser;
use crate::tools::cli::CliOptions;

use std::fs::File;

/// Reads an LTS and a mu-calculus formula and calculates if the LTS satisfies
/// this formula, and optionally which states satisfy it.
pub fn check_lts(options: &CliOptions) -> Result<bool, Mcrl2Error> {
    let input_file = try_into!(options.get_named_string("input"));
    let formula_file = try_into!(options.get_named_string("formula"));
    let output_file = try_into!(options.get_named_string("output"));

    let mut file = File::open(input_file)?;
    let lts = try_into!(read_aldebaran_file(&mut file));

    let formula_string = std::fs::read_to_string(formula_file)?;
    let formula_tokens = try_into!(tokenize(&formula_string));
    let mut formula_parser = Parser::new(&formula_tokens);
    let formula = try_into!(formula_parser.parse_state_formula());

    let mut state_set_manager = StateSetManager::new(lts.nodes.len());
    let result = try_into!(calculate_set(&lts, &formula, &mut state_set_manager));

    std::fs::write(output_file, format!("{:?}", result))?;

    Ok(result.contains(lts.initial_state))
}
