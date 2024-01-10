
use crate::try_into;
use crate::analysis::small_progress_measures::{IterationPolicy, solve_parity_game};
use crate::core::error::Mcrl2Error;
use crate::parity_game::parity_game::Player;
use crate::parity_game::pgsolver::parse_pgsolver_game;
use crate::tools::cli::CliOptions;

use std::fs::{File, read_to_string};
use std::io::{BufWriter, Write};
use std::time::Instant;

/// Reads a min parity game and calculates if node 0 is won by player "even",
/// and optionally all the states that are won by player even.
pub fn solve_pg(options: &CliOptions) -> Result<Player, Mcrl2Error> {
    let input_file = try_into!(options.get_named_string("input"));
    let output_file = if options.has_named("output") {
        Some(try_into!(options.get_named_string("output")))
    } else {
        None
    };
    let policy = if options.has_named("policy") {
        let string = try_into!(options.get_named_string("policy"));
        match string.as_str() {
            "input" => IterationPolicy::InputOrder,
            "random" => {
                let seed = if options.has_named("seed") {
                    try_into!(options.get_named_int::<u64>("seed"))
                } else {
                    rand::random::<u64>()
                };
                eprintln!("Using seed {} for 'random' policy", seed);
                IterationPolicy::RandomOrder { seed }
            },
            "degree-ascending" => IterationPolicy::AscendingDegreeOrder,
            "degree-descending" => IterationPolicy::DescendingDegreeOrder,
            _ => return Err(Mcrl2Error::ToolUsageError {
                message: format!("unknown policy '{}'", string),
                option: Some(String::from("policy")),
            }),
        }
    } else {
        IterationPolicy::InputOrder
    };

    eprintln!("Parsing parity game...");
    let now = Instant::now();
    let pg_string = read_to_string(input_file)?;
    let pg = try_into!(parse_pgsolver_game(&pg_string));
    eprintln!("Parsing parity game took {} ms", now.elapsed().as_millis());

    eprintln!("Solving parity game...");
    let now = Instant::now();
    let result = solve_parity_game(&pg, Player::Even, policy);
    eprintln!("Solving parity game took {} ms", now.elapsed().as_millis());

    if let Some(output_file) = output_file {
        let mut file = BufWriter::new(File::create(output_file)?);
        for &elem in &result {
            write!(file, "{}\n", elem)?;
        }
    }

    assert!(result.len() <= pg.nodes.len());
    eprintln!("# won by even: {}, # won by odd: {}",
        result.len(),
        pg.nodes.len() - result.len(),
    );

    Ok(if result.contains(&0) {
        Player::Even
    } else {
        Player::Odd
    })
}
