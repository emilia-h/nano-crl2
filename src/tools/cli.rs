
use crate::core::diagnostic::{Diagnostic, DiagnosticSeverity};

use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::str::FromStr;

/// An error in the CLI input format.
#[derive(Debug, Eq, PartialEq)]
pub enum CliError {
    IncorrectFormat(&'static str),
    UnrecognizedOption(String),
    NotSupplied(String),
    TooManySupplied(String),
    NotBool(String),
    NotInteger(String),
}

impl CliError {
    pub fn get_option(&self) -> Option<&str> {
        use CliError::*;

        match self {
            IncorrectFormat(_) => None,
            UnrecognizedOption(option) => Some(option),
            NotSupplied(option) => Some(option),
            TooManySupplied(option) => Some(option),
            NotBool(option) => Some(option),
            NotInteger(option) => Some(option),
        }
    }
}

impl Into<Diagnostic> for CliError {
    fn into(self) -> Diagnostic {
        use CliError::*;

        Diagnostic {
            severity: DiagnosticSeverity::Error,
            file: None,
            module: None,
            loc: None,
            message: match &self {
                IncorrectFormat(string) =>
                    format!("{}", string),
                UnrecognizedOption(option) =>
                    format!("unrecognized argument '{}'", option),
                NotSupplied(option) =>
                    format!("missing argument '{}'", option),
                TooManySupplied(option) =>
                    format!("too many values given for argument '{}'", option),
                NotBool(option) =>
                    format!("expected a boolean for argument '{}'", option),
                NotInteger(option) =>
                    format!("expected an integer for argument '{}'", option),
            },
        }
    }
}

/// The configuration of a CLI tool, specifiying what options (`--option`) and
/// shorthands (`-o`) are available.
pub struct CliConfig<'a> {
    pub names: HashSet<&'a str>,
    pub shorthands: HashMap<char, &'a str>,
}

impl<'a> CliConfig<'a> {
    /// Creates a new `CliConfig` using a list of `(id, name, shorthand)`
    /// triples, such that both `--name` and `-shorthand` map to `id` when
    /// parsing the list of CLI arguments in `CliOptions::parse()`.
    /// 
    /// Pass a null byte `'\0'` as the shorthand string to have no shorthand
    /// for that named argument.
    /// 
    /// # Preconditions
    /// The first and second element in each tuple cannot be an empty string.
    pub fn new(config: &[(&'a str, char)]) -> CliConfig<'a> {
        let mut names = HashSet::new();
        let mut shorthands = HashMap::new();

        for &(name, shorthand) in config {
            assert_ne!(name, "");
            names.insert(name);
            if shorthand != '\0' {
                shorthands.insert(shorthand, name);
            }
        }

        CliConfig { names, shorthands }
    }
}

#[derive(Debug)]
pub struct CliOptions {
    values: Vec<String>,
    named_values: HashMap<String, Vec<String>>,
}

static EMPTY_VECTOR: Vec<String> = Vec::new();

impl CliOptions {
    /// Parses a list of arguments and maps named arguments to the options
    /// given in `config`.
    /// 
    /// # Example
    /// ```rust
    /// # use nano_crl2::tools::cli::{CliConfig, CliOptions};
    /// 
    /// let config = CliConfig::new(&[
    ///     ("message", 'm'),
    ///     ("help", 'h'),
    /// ]);
    /// let args = [
    ///     String::from("git"), String::from("commit"),
    ///     String::from("-m"), String::from("example"),
    /// ];
    /// let options = CliOptions::parse(&config, &args).unwrap();
    /// assert_eq!(options.get_unnamed_string(1), Some(&String::from("commit")));
    /// assert_eq!(options.get_named_string("message"), Ok(&String::from("example")));
    /// assert!(!options.has_named("help"));
    /// ```
    pub fn parse(config: &CliConfig, args: &[String]) -> Result<CliOptions, CliError> {
        let mut values = Vec::new();
        let mut named_values = HashMap::new();

        let mut current_option: Option<&str> = None;
        for arg in args {
            if arg.starts_with("-") || arg.starts_with("--") {
                if let Some(prev_option) = current_option {
                    // "... --option1 -option2 ..." should set --option1 to ""
                    named_values.entry(String::from(prev_option))
                        .or_insert(vec![])
                        .push(String::new());
                    current_option = None;
                }
            }

            if arg.starts_with("--") {
                if let Some(split_index) = arg.find("=") {
                    // "--option=value"
                    let (key, value) = arg.split_at(split_index);
                    let name = key.split_at(2).1;
                    if config.names.contains(name) {
                        named_values.entry(String::from(name))
                            .or_insert(vec![])
                            .push(String::from(value.split_at(1).1));
                    } else {
                        return Err(CliError::UnrecognizedOption(String::from(key)));
                    }
                } else {
                    // "--option value" or just "--option"
                    let key = arg.split_at(2).1;
                    if config.names.contains(key) {
                        current_option = Some(key);
                    } else {
                        return Err(CliError::UnrecognizedOption(String::from(key)));
                    }
                }
            } else if arg.starts_with("-") {
                if arg.len() == 1 {
                    // just a loose "-" which is never correct
                    return Err(CliError::IncorrectFormat("Arguments contain a loose '-'"));
                } else if arg.len() == 2 {
                    // "-x value" or just "-x"
                    let key = arg.chars().nth(1).unwrap();
                    if let Some(&id) = config.shorthands.get(&key) {
                        current_option = Some(id);
                    } else {
                        return Err(CliError::UnrecognizedOption(String::from(key)));
                    }
                } else {
                    // "-xpath"
                    let key = arg.chars().nth(1).unwrap();
                    if let Some(&id) = config.shorthands.get(&key) {
                        let value = String::from(arg.split_at(2).1);
                        named_values.entry(String::from(id))
                            .or_insert(vec![])
                            .push(value);
                    } else {
                        return Err(CliError::UnrecognizedOption(String::from(key)));
                    }
                }
            } else if let Some(name) = current_option {
                // if we had "--option" previously and now arg = "value", then --option=value
                named_values.entry(String::from(name))
                    .or_insert(vec![])
                    .push(arg.clone());

                current_option = None;
            } else {
                values.push(String::from(arg));
            }
        }

        if let Some(name) = current_option {
            named_values.entry(String::from(name))
                .or_insert(vec![])
                .push(String::new());
        }

        Ok(CliOptions { values, named_values })
    }

    /// Returns the number of unnamed values.
    /// 
    /// # Example
    /// ```rust
    /// # use nano_crl2::tools::cli::{CliConfig, CliOptions};
    /// let config = CliConfig::new(&[("message", 'm')]);
    /// let args = [
    ///     String::from("git"), String::from("commit"),
    ///     String::from("-m"), String::from("example"),
    /// ];
    /// let options = CliOptions::parse(&config, &args).unwrap();
    /// assert_eq!(options.get_unnamed_len(), 2);
    /// ```
    pub fn get_unnamed_len(&self) -> usize {
        self.values.len()
    }

    /// Returns the unnamed argument at a given `index`, or `None` if there are
    /// not that many unnamed arguments.
    /// 
    /// # Example
    /// ```rust
    /// # use nano_crl2::tools::cli::{CliConfig, CliOptions};
    /// let config = CliConfig::new(&[("input", 'i')]);
    /// let args = [
    ///     String::from("cli-tool"), String::from("run"),
    ///     String::from("-i"), String::from("file.txt"),
    ///     String::from("arg2"),
    /// ];
    /// let options = CliOptions::parse(&config, &args).unwrap();
    /// assert_eq!(options.get_unnamed_string(0).unwrap(), "cli-tool");
    /// assert_eq!(options.get_unnamed_string(1).unwrap(), "run");
    /// assert_eq!(options.get_unnamed_string(2).unwrap(), "arg2");
    /// assert!(options.get_unnamed_string(3).is_none());
    /// ```
    pub fn get_unnamed_string(&self, index: usize) -> Option<&String> {
        self.values.get(index)
    }

    /// Returns whether a specific named option was given in the input.
    pub fn has_named(&self, id: &str) -> bool {
        self.named_values.contains_key(id)
    }

    pub fn get_named_list(&self, id: &str) -> &Vec<String> {
        self.named_values.get(id).unwrap_or(&EMPTY_VECTOR)
    }

    pub fn get_named_string(&self, id: &str) -> Result<&String, CliError> {
        if let Some(values) = self.named_values.get(id) {
            if values.len() == 1 {
                Ok(&values[0])
            } else {
                Err(CliError::TooManySupplied(String::from(id)))
            }
        } else {
            Err(CliError::NotSupplied(String::from(id)))
        }
    }

    pub fn get_named_int<T: FromStr>(&self, id: &str) -> Result<T, CliError> {
        if let Some(values) = self.named_values.get(id) {
            assert!(values.len() >= 1);

            if values.len() == 1 {
                match values[0].parse::<T>() {
                    Ok(value) => Ok(value),
                    Err(_) => Err(CliError::NotInteger(String::from(id))),
                }
            } else {
                Err(CliError::TooManySupplied(String::from(id)))
            }
        } else {
            Err(CliError::NotSupplied(String::from(id)))
        }
    }

    /// Returns the value of a named option and converts it to a bool.
    /// 
    /// This maps an empty string (""), "1" and "true" to `true` and maps "0"
    /// or "false" to `false`.
    pub fn get_named_bool(&self, id: &str, default_value: bool) -> Result<bool, CliError> {
        if let Some(values) = self.named_values.get(id) {
            assert!(values.len() >= 1);

            if values.len() == 1 {
                let value = &values[0];
                if value == "" || value == "true" || value == "1" {
                    Ok(true)
                } else if value == "false" || value == "0" {
                    Ok(false)
                } else {
                    Err(CliError::NotBool(String::from(id)))
                }
            } else {
                Err(CliError::TooManySupplied(String::from(id)))
            }
        } else {
            Ok(default_value)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_cli_basic() {
        let config = CliConfig::new(&[
            ("input", 'i'),
            ("output", 'o'),
        ]);
        let args = [
            "--input=x",
            "--input", "x",
            "-i", "hello",
            "-opath",
            "test",
        ].map(|s| String::from(s)).into_iter().collect::<Vec<_>>();
        let options = CliOptions::parse(&config, &args).unwrap();

        assert_eq!(options.get_named_list("input"), &vec![String::from("x"), String::from("x"), String::from("hello")]);
        assert_eq!(options.get_named_list("output"), &vec![String::from("path")]);
        assert_eq!(options.get_named_list("fake"), &Vec::<String>::new());
        assert_eq!(options.get_named_string("output"), Ok(&String::from("path")));
        assert!(options.get_named_string("fake").is_err());

        assert_eq!(options.get_unnamed_len(), 1);
        assert_eq!(options.get_unnamed_string(0), Some(&String::from("test")));
        assert!(options.get_unnamed_string(1).is_none());
    }

    #[test]
    fn test_parse_cli_empty() {
        let config = CliConfig::new(&[
            ("input", 'i'),
            ("output", 'o'),
        ]);

        let args = [
            "-i",
        ].map(|s| String::from(s)).into_iter().collect::<Vec<_>>();
        let options = CliOptions::parse(&config, &args).unwrap();
        assert!(options.has_named("input"));
        assert_eq!(options.get_named_string("input"), Ok(&String::from("")));
        assert_eq!(options.get_named_bool("input", false), Ok(true));
        assert_eq!(options.get_named_bool("input", true), Ok(true));
        assert_eq!(options.get_named_bool("output", false), Ok(false));
        assert_eq!(options.get_named_bool("output", true), Ok(true));

        let args = [
            "--input",
        ].map(|s| String::from(s)).into_iter().collect::<Vec<_>>();
        let options = CliOptions::parse(&config, &args).unwrap();
        assert!(options.has_named("input"));
        assert_eq!(options.get_named_string("input"), Ok(&String::from("")));
        assert_eq!(options.get_named_bool("input", false), Ok(true));
        assert_eq!(options.get_named_bool("input", true), Ok(true));
        assert_eq!(options.get_named_bool("output", false), Ok(false));
        assert_eq!(options.get_named_bool("output", true), Ok(true));
    }

    #[test]
    fn test_parse_cli_bool() {
        let config = CliConfig::new(&[
            ("aa", 'a'), // note b and a are swapped
            ("bb", 'b'),
            ("cc", 'c'),
            ("dd", 'd'),
            ("ee", 'e'),
        ]);
        let args = [
            "--bb=true",
            "--aa",
            "-c",
            "-e", "true",
            "-a0",
            "remainder",
        ].map(|s| String::from(s)).into_iter().collect::<Vec<_>>();
        let options = CliOptions::parse(&config, &args).unwrap();

        assert!(options.get_named_bool("aa", false).is_err()); // multiple values
        assert_eq!(options.get_named_bool("bb", false), Ok(true));
        assert_eq!(options.get_named_bool("dd", true), Ok(true));
        assert_eq!(options.get_named_bool("dd", false), Ok(false));
        assert_eq!(options.get_named_bool("cc", false), Ok(true));

        assert_eq!(options.get_unnamed_len(), 1);
        assert_eq!(options.get_unnamed_string(0), Some(&String::from("remainder")));
    }
}
