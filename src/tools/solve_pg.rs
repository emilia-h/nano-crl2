
use crate::analysis::small_progress_measures::{IterationPolicy, solve_parity_game};
use crate::core::diagnostic::{Diagnostic, DiagnosticContext, DiagnosticSeverity};
use crate::parity_game::parity_game::Player;
use crate::parity_game::pgsolver::parse_pgsolver_game;
use crate::tools::cli::CliOptions;
use crate::util::io::{create_file_writer, read_file};

use std::io::Write;
use std::time::{Duration, Instant, SystemTime};

/// Reads a min parity game and calculates if node 0 is won by player "even",
/// and optionally all the states that are won by player even.
pub fn solve_pg(
    options: &CliOptions,
    diagnostics: &mut DiagnosticContext,
) -> Result<Player, ()> {
    let input_file = diagnostics.union_result(options.get_named_string("input"))?;
    let output_file = if options.has_named("output") {
        Some(diagnostics.union_result(options.get_named_string("output"))?)
    } else {
        None
    };
    let policy = if options.has_named("policy") {
        let string = diagnostics.union_result(options.get_named_string("policy"))?;
        match string.as_str() {
            "input" => IterationPolicy::InputOrder,
            "random" => {
                let seed = if options.has_named("seed") {
                    diagnostics.union_result(options.get_named_int::<u64>("seed"))?
                } else {
                    // what value we pick for this doesn't need to be random
                    SystemTime::UNIX_EPOCH.elapsed()
                        .unwrap_or(Duration::ZERO)
                        .as_nanos() as u64
                };
                eprintln!("Using seed {} for 'random' policy", seed);
                IterationPolicy::RandomOrder { seed }
            },
            "descending-degree" => IterationPolicy::DescendingDegreeOrder,
            "reverse-bfs" => IterationPolicy::ReverseBfs,
            "postorder-dfs" => IterationPolicy::PostOrderDfs,
            _ => {
                diagnostics.push(Diagnostic {
                    severity: DiagnosticSeverity::Warning,
                    file: None,
                    loc: None,
                    message: format!("unknown policy '{}'", string),
                });
                IterationPolicy::InputOrder
            },
        }
    } else {
        IterationPolicy::InputOrder
    };

    eprintln!("Parsing parity game...");
    let now = Instant::now();
    let pg_string = diagnostics.union_result(read_file(input_file))?;
    let pg = diagnostics.union_result(
        parse_pgsolver_game(&pg_string).map_err(|error| {
            error.into_diagnostic(Some(input_file.clone()))
        }),
    )?;
    eprintln!("Parsing parity game took {} ms", now.elapsed().as_millis());

    eprintln!("Solving parity game...");
    let now = Instant::now();
    let result = solve_parity_game(&pg, Player::Even, policy);
    eprintln!("Solving parity game took {} ms", now.elapsed().as_millis());

    if let Some(output_file) = output_file {
        let mut file = diagnostics.union_result(create_file_writer(output_file))?;
        for &elem in &result {
            let write_result = write!(file, "{}\n", elem).map_err(|error| {
                Diagnostic {
                    severity: DiagnosticSeverity::Error,
                    file: Some(output_file.clone()),
                    loc: None,
                    message: error.to_string(),
                }
            });
            diagnostics.union_result(write_result)?;
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
