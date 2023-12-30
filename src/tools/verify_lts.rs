
use crate::try_into;
use crate::analysis::lts_verification::calculate_set;
use crate::core::error::Mcrl2Error;
use crate::core::state_set::StateSetManager;
use crate::lts::aldebaran::parse_aldebaran_lts;
use crate::parser::lexer::tokenize;
use crate::parser::parser::Parser;
use crate::tools::cli::CliOptions;

use std::fs::read_to_string;
use std::time::Instant;

/// Reads an LTS and a mu-calculus formula and calculates if the LTS satisfies
/// this formula, and optionally which states satisfy it.
pub fn verify_lts(options: &CliOptions) -> Result<bool, Mcrl2Error> {
    let input_file = try_into!(options.get_named_string("input"));
    let formula_file = try_into!(options.get_named_string("property"));
    let output_file = if options.has_named("output") {
        Some(try_into!(options.get_named_string("output")))
    } else {
        None
    };

    eprintln!("Parsing LTS...");
    let now = Instant::now();
    let lts_string = read_to_string(input_file)?;
    let lts = try_into!(parse_aldebaran_lts(&lts_string));
    eprintln!("Parsing LTS took {} ms", now.elapsed().as_millis());

    eprintln!("Parsing property...");
    let now = Instant::now();
    let formula_string = read_to_string(formula_file)?;
    let formula_tokens = try_into!(tokenize(&formula_string));
    let mut formula_parser = Parser::new(&formula_tokens);
    let formula = try_into!(formula_parser.parse_state_formula());
    eprintln!("Parsing property took {} ms", now.elapsed().as_millis());

    eprintln!("Verifying property on LTS...");
    let now = Instant::now();
    let mut state_set_manager = StateSetManager::new(lts.nodes.len());
    let result = try_into!(calculate_set(&lts, &formula, &mut state_set_manager));
    eprintln!("Verifying property on LTS took {} ms", now.elapsed().as_millis());

    if let Some(output_file) = output_file {
        std::fs::write(output_file, format!("{:?}", result))?;
    }

    Ok(result.contains(lts.initial_state))
}
