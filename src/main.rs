
use nano_crl2::core::diagnostic::DiagnosticContext;
use nano_crl2::tools::check_input::check_input;
use nano_crl2::tools::gen_docs::gen_docs;
use nano_crl2::tools::solve_pg::solve_pg;
use nano_crl2::tools::verify_lts::verify_lts;
use nano_crl2::tools::cli::{CliConfig, CliOptions};
use std::env;

// character limit per line is 100
const HELP_STRING: &'static str = "
nanoCRL2: a simple toolset for mCRL2. Usage:

$ nanocrl2 <tool> <options...>

Where <tool> is one of:
  help              Get extra help about a particular tool
  check-input       Parses files and checks that they are syntactically correct and well-formed.
  gen-docs          Parses .mcrl2 files and generates documentation for the declarations in those
                    files.
  solve-pg          Parses a parity game file (.gm) and outputs the winner for each vertex in the
                    game.
  verify-lts        Parses an LTS file (.aut) and a formula file (.mcf) and outputs for which
                    states of the LTS file it holds.
";

const HELP_STRING_CHECK_INPUT: &'static str = "
check-input: a checker for well-formedness of mCRL2 files. Usage:

$ nanocrl2 check-input <options...>

Where <options> is zero or more of the following:
  --help                    -h  Shows this message
  --input=<file>            -i  Gets input from the given file(s)
";

const HELP_STRING_GEN_DOCS: &'static str = "
gen-docs: a documentation generator for mCRL2 models. Usage:

$ nanocrl2 gen-docs <options...>

Where <options> is zero or more of the following:
  --help                    -h  Shows this message
  --input=<file>            -i  Gets input from the given .mcrl2 file(s)
  --output=<path>           -o  Outputs the documentation to a given file or directory
";

const HELP_STRING_SOLVE_PG: &'static str = "
solve-pg: a parity game solver (NOTE: assuming min parity games). Usage:

$ nanocrl2 solve-pg <options...>

Where <options> is zero or more of the following:
  --help                    -h  Shows this message
  --input=<file>            -i  Gets input from the given parity game file
  --output=<file>           -o  Outputs the set of states won by the even player to a given file
  --policy=<policy>         -p  [opt] Uses the given iteration policy, where <policy> is one of:
                                input (default) - iterates as given in the input
                                random - generates a random order, then keeps iterating in that
                                    order
                                ascending-degree - goes from low-degree vertices to high
                                reverse-bfs - does a reverse breadth-first traversal
                                postorder-dfs - does a postorder depth-first traversal
  --seed=<seed>             -s  [opt] Uses a specific seed in case a randomized algorithms is used
";

const HELP_STRING_VERIFY_LTS: &'static str = "
verify-lts: an LTS verifier of mu-calculus properties. Usage:

$ nanocrl2 verify-lts <options...>

Where <options> is zero or more of the following:
  --help                   -h  Shows this message
  --input=<file>           -i  Gets the LTS input from a given file  
  --property=<file>        -p  Gets the mu-calculus input from a given file
  --output=<file>          -o  [opt] Outputs the full set of satisfying states to a given file.
";

fn main() {
    let args = env::args().collect::<Vec<String>>();

    if args.len() < 2 {
        eprintln!("{}", HELP_STRING);
        return;
    }

    let tool = args[1].as_str();
    let options = &args[2..];

    let mut diagnostics = DiagnosticContext::new();

    if tool == "check-input" {
        let cli_config = CliConfig::new(&[
            ("help", 'h'),
            ("input", 'i'),
        ]);
        match CliOptions::parse(&cli_config, options) {
            Ok(options) if options.has_named("help") => {
                eprintln!("{}", HELP_STRING_CHECK_INPUT);
            },
            Ok(options) => match check_input(&options, &mut diagnostics) {
                Ok(()) => println!("success"),
                Err(()) => {
                    println!("failure");
                    eprintln!("{}", diagnostics);
                },
            },
            Err(error) => {
                eprintln!("{:?}", error);
            },
        }
    } else if tool == "gen-docs" {
        let cli_config = CliConfig::new(&[
            ("help", 'h'),
            ("input", 'i'),
            ("output", 'o'),
            ("format", 'f'),
        ]);
        match CliOptions::parse(&cli_config, options) {
            Ok(options) if options.has_named("help") => {
                eprintln!("{}", HELP_STRING_GEN_DOCS);
            },
            Ok(options) => match gen_docs(&options, &mut diagnostics) {
                Ok(docs) => println!("{}", docs),
                Err(()) => eprintln!("{}", diagnostics),
            },
            Err(error) => {
                eprintln!("{:?}", error);
            },
        }
    } else if tool == "solve-pg" {
        let cli_config = CliConfig::new(&[
            ("help", 'h'),
            ("input", 'i'),
            ("output", 'o'),
            ("policy", 'p'),
            ("seed", 's'),
        ]);
        match CliOptions::parse(&cli_config, options) {
            Ok(options) if options.has_named("help") => {
                eprintln!("{}", HELP_STRING_SOLVE_PG);
            },
            Ok(options) => match solve_pg(&options, &mut diagnostics) {
                Ok(winner) => println!("{}", winner),
                Err(()) => eprintln!("{}", diagnostics),
            },
            Err(error) => {
                eprintln!("{:?}", error);
            },
        }
    } else if tool == "verify-lts" {
        let cli_config = CliConfig::new(&[
            ("help", 'h'),
            ("input", 'i'),
            ("property", 'f'),
            ("output", 'o'),
        ]);
        match CliOptions::parse(&cli_config, options) {
            Ok(options) if options.has_named("help") => {
                eprintln!("{}", HELP_STRING_VERIFY_LTS);
            },
            Ok(options) => match verify_lts(&options, &mut diagnostics) {
                Ok(satisfied) => println!("{}", satisfied),
                Err(()) => eprintln!("{}", diagnostics),
            },
            Err(error) => {
                eprintln!("{:?}", error);
            }
        }
    } else if ["help", "--help", "-h", "-?"].contains(&tool) {
        match args.get(2).map(|x| x.as_str()) {
            Some("gen-docs") => {
                eprintln!("{}", HELP_STRING_GEN_DOCS);
            },
            Some("solve-pg") => {
                eprintln!("{}", HELP_STRING_SOLVE_PG);
            },
            Some("verify-lts") => {
                eprintln!("{}", HELP_STRING_VERIFY_LTS);
            },
            _ => {
                eprintln!("{}", HELP_STRING);
            }
        }
    } else {
        eprintln!("Unknown tool '{}'", tool);
        eprintln!("Use nanocrl2 --help to get a list of available tools.");
    }
}
