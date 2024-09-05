
use crate::analysis::emerson_lei::calculate_set;
use crate::core::diagnostic::{Diagnostic, DiagnosticContext, DiagnosticSeverity};
use crate::core::lexer::tokenize;
use crate::core::parser::Parser;
use crate::core::state_set::StateSetManager;
use crate::lts::aldebaran::parse_aldebaran_lts;
use crate::mu_calculus::state_formula::StateFormula;
use crate::tools::cli::CliOptions;
use crate::util::io::{create_file_writer, read_file, Write};

use std::time::Instant;

/// Reads an LTS and a mu-calculus formula and calculates if the LTS satisfies
/// this formula, and optionally which states satisfy it.
pub fn verify_lts(
    options: &CliOptions,
    diagnostics: &mut DiagnosticContext,
) -> Result<bool, ()> {
    let input_file = diagnostics.union_result(options.get_named_string("input"))?;
    let formula_file = diagnostics.union_result(options.get_named_string("property"))?;
    let output_file = if options.has_named("output") {
        Some(diagnostics.union_result(options.get_named_string("output"))?)
    } else {
        None
    };

    eprintln!("Parsing LTS...");
    let now = Instant::now();
    let lts_string = diagnostics.union_result(read_file(input_file))?;
    let lts = diagnostics.union_result(
        parse_aldebaran_lts(&lts_string).map_err(|error| {
            error.into_diagnostic(Some(input_file.clone()))
        }),
    )?;
    eprintln!("Parsing LTS took {} ms", now.elapsed().as_millis());

    eprintln!("Parsing property...");
    let now = Instant::now();
    let formula_string = diagnostics.union_result(read_file(formula_file))?;
    let formula_tokens = diagnostics.union_result(
        tokenize(&formula_string).map_err(|error| {
            error.into_diagnostic(Some(formula_file.clone()))
        }),
    )?;
    let mut formula_parser = Parser::new(&formula_tokens);
    let formula = diagnostics.union_result(
        formula_parser.parse::<StateFormula>().map_err(|error| {
            error.into_diagnostic(Some(formula_file.clone()))
        }),
    )?;
    eprintln!("Parsing property took {} ms", now.elapsed().as_millis());

    eprintln!("Verifying property on LTS...");
    let now = Instant::now();
    let mut state_set_manager = StateSetManager::new(lts.nodes.len());
    let result = diagnostics.union_result(
        calculate_set(&lts, &formula, &mut state_set_manager).map_err(|error| {
            error.into_diagnostic(Some(formula_file.clone()))
        }),
    )?;
    eprintln!("Verifying property on LTS took {} ms", now.elapsed().as_millis());

    if let Some(output_file) = output_file {
        let mut writer = diagnostics.union_result(create_file_writer(&output_file))?;
        diagnostics.union_result(write!(writer, "{:?}", result).map_err(|error| {
            Diagnostic {
                severity: DiagnosticSeverity::Error,
                file: Some(output_file.clone()),
                loc: None,
                message: error.to_string(),
            }
        }))?;
    }

    Ok(result.contains(lts.initial_state))
}
