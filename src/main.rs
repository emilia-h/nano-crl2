
use nano_crl2::tools::cli::{CliConfig, CliOptions};
use nano_crl2::tools::docgen::docgen;
use std::env;

const HELP_STRING: &'static str = "
nanoCRL2: a minimalistic implementation of mCRL2. Usage:

$ nanocrl2 <tool> <args...>

Where <tool> is one of:
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
    if tool == "docgen" {
        let cli_config: CliConfig = CliConfig::new(&[
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
        eprintln!("Use mcrl22lps --help to get a list of available tools.");
    }
}
