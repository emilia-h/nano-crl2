
use crate::try_into;
use crate::analysis::small_progress_measures::solve_parity_game;
use crate::core::error::Mcrl2Error;
use crate::parity_game::parity_game::Player;
use crate::parity_game::pgsolver::parse_pgsolver_game;
use crate::tools::cli::CliOptions;

use std::fs::read_to_string;
use std::time::Instant;

/// Reads an LTS and a mu-calculus formula and calculates if the LTS satisfies
/// this formula, and optionally which states satisfy it.
pub fn solve_pg(options: &CliOptions) -> Result<bool, Mcrl2Error> {
    let input_file = try_into!(options.get_named_string("input"));
    let output_file = if options.has_named("output") {
        Some(try_into!(options.get_named_string("output")))
    } else {
        None
    };

    eprintln!("Parsing parity game...");
    let now = Instant::now();
    let pg_string = read_to_string(input_file)?;
    let pg = try_into!(parse_pgsolver_game(&pg_string));
    eprintln!("Parsing parity game took {} ms", now.elapsed().as_millis());

    eprintln!("Solving parity game...");
    let now = Instant::now();
    let result = solve_parity_game(&pg, Player::Even);
    eprintln!("Solving parity game took {} ms", now.elapsed().as_millis());

    if let Some(output_file) = output_file {
        std::fs::write(output_file, format!("{:?}", result))?;
    }

    Ok(result.contains(&0))
}
