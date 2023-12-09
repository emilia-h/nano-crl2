use std::env;

const HELP_STRING: &'static str = "
nanoCRL2: a minimalistic implementation of mCRL2. Usage:

$ nanocrl2 <tool> <args...>

Where <tool> is one of:
  mcrl22lps             Parses a .mcrl2 file and converts it to a .lps (linear process) file
";

fn main() {
    let args = env::args().collect::<Vec<String>>();

    if args.len() < 2 {
        eprintln!("{}", HELP_STRING);
        return;
    }

    let tool = args[1].as_str();
    if tool == "mcrl22lps" {

    } else if tool == "help" || tool == "--help" || tool == "-h" {
        eprintln!("{}", HELP_STRING);
    } else {
        eprintln!("Unknown tool '{}'", tool);
        eprintln!("Use mcrl22lps --help to get a list of available tools.");
    }
}
