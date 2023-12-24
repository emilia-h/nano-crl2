
use nano_crl2::tools::verify_lts::verify_lts;
use nano_crl2::tools::cli::{CliConfig, CliOptions};
use nano_crl2::tools::docgen::docgen;
use std::env;

const HELP_STRING: &'static str = "
nanoCRL2: a simple toolset for mCRL2. Usage:

$ nanocrl2 <tool> <options...>

Where <tool> is one of:
  verify-lts            Parses an LTS file (.aut) and a formula file (.mcf) and
                        outputs for which states of the LTS file it holds.
  docgen                Parses .mcrl2 files and generates documentation for the
                        declarations in those files
";

fn main() {
    let args = env::args().collect::<Vec<String>>();

    if args.len() < 2 {
        eprintln!("{}", HELP_STRING);
        return;
    }

    let tool = args[1].as_str();
    let options = &args[2..];

    if tool == "verify-lts" {
        let cli_config = CliConfig::new(&[
            ("help", "help", 'h'),
            ("input", "input", 'i'),
            ("formula", "formula", 'f'),
            ("output", "output", 'o'),
        ]);
        match CliOptions::parse(&cli_config, options) {
            Ok(options) => {
                match verify_lts(&options) {
                    Ok(set) => println!("{}", set),
                    Err(error) => eprintln!("{:?}", error),
                }
            },
            Err(error) => {
                eprintln!("{:?}", error);
            }
        }
    } else if tool == "docgen" {
        let cli_config = CliConfig::new(&[
            ("help", "help", 'h'),
            ("input", "input", 'i'),
            ("output", "output", 'o'),
            ("format", "format", 'f'),
        ]);
        match CliOptions::parse(&cli_config, options) {
            Ok(options) => {
                match docgen(&options) {
                    Ok(docs) => println!("{}", docs),
                    Err(error) => eprintln!("{:?}", error),
                }
            },
            Err(error) => {
                eprintln!("{:?}", error);
            },
        }
    } else if tool == "help" || tool == "--help" || tool == "-h" {
        eprintln!("{}", HELP_STRING);
    } else {
        eprintln!("Unknown tool '{}'", tool);
        eprintln!("Use nanocrl2 --help to get a list of available tools.");
    }
}
