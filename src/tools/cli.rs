
use crate::core::error::Mcrl2Error;

use std::collections::hash_map::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq)]
pub enum CliError {
    IncorrectFormat(&'static str),
    UnrecognizedOption(String),
    NotSupplied(String),
    TooManySupplied(String),
    NotBool(String),
    NotInteger(String),
}

impl Display for CliError {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            CliError::IncorrectFormat(string) =>
                write!(fmt, "{}", string)?,
            CliError::UnrecognizedOption(key) =>
                write!(fmt, "Unrecognized argument '{}'", key)?,
            CliError::NotSupplied(key) =>
                write!(fmt, "Missing argument '{}'", key)?,
            CliError::TooManySupplied(key) =>
                write!(fmt, "Too many values given for argument '{}'", key)?,
            CliError::NotBool(key) =>
                write!(fmt, "Expected a boolean for argument '{}'", key)?,
            CliError::NotInteger(key) =>
                write!(fmt, "Expected an integer for argument '{}'", key)?,
        }
        Ok(())
    }
}

impl Into<Mcrl2Error> for CliError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::ToolUsageError {
            message: format!("{}", self),
        }
    }
}

pub struct CliConfig<'a, 'b> {
    pub names: HashMap<&'b str, &'a str>,
    pub shorthands: HashMap<char, &'a str>,
}

impl<'a, 'b> CliConfig<'a, 'b> {
    /// Creates a new `CliConfig` using a list of `(id, name, shorthand)`
    /// triples, such that both `--name` and `-shorthand` map to `id` when
    /// parsing the list of CLI arguments in `CliOptions::parse()`.
    /// 
    /// Pass a null byte `'\0'` as the shorthand string to have no shorthand
    /// for that named argument.
    /// 
    /// # Preconditions
    /// The first and second element in each tuple cannot be an empty string.
    pub fn new(config: &[(&'a str, &'b str, char)]) -> CliConfig<'a, 'b> {
        let mut names = HashMap::new();
        let mut shorthands = HashMap::new();

        for &(id, name, shorthand) in config {
            assert_ne!(id, "");
            assert_ne!(name, "");
            names.insert(name, id);
            if shorthand != '\0' {
                shorthands.insert(shorthand, id);
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
                    if let Some(&id) = config.names.get(key.split_at(2).1) {
                        named_values.entry(String::from(id))
                            .or_insert(vec![])
                            .push(String::from(value.split_at(1).1));
                    } else {
                        return Err(CliError::UnrecognizedOption(String::from(key)));
                    }
                } else {
                    // "--option value" or just "--option"
                    let key = arg.split_at(2).1;
                    if let Some(&id) = config.names.get(key) {
                        current_option = Some(id);
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
                        eprintln!("adding {:?} to {:?}", value, id);
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

        Ok(CliOptions { values, named_values })
    }

    pub fn get_unnamed_len(&self) -> usize {
        self.values.len()
    }

    pub fn get_unnamed_value(&self, index: usize) -> Option<&String> {
        return self.values.get(index);
    }

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
            ("inp", "input", 'i'),
            ("out", "output", 'o'),
        ]);
        let args = [
            "--input=x",
            "--input", "x",
            "-i", "hello",
            "-opath",
            "test",
        ].map(|s| String::from(s)).into_iter().collect::<Vec<_>>();
        let options = CliOptions::parse(&config, &args).unwrap();

        assert_eq!(options.get_named_list("inp"), &vec![String::from("x"), String::from("x"), String::from("hello")]);
        assert_eq!(options.get_named_list("out"), &vec![String::from("path")]);
        assert_eq!(options.get_named_list("fake"), &Vec::<String>::new());
        assert_eq!(options.get_named_list("output"), &Vec::<String>::new());
        assert_eq!(options.get_named_list("input"), &Vec::<String>::new());
        assert_eq!(options.get_named_string("out"), Ok(&String::from("path")));
        assert!(options.get_named_string("inp").is_err());
        assert!(options.get_named_string("fake").is_err());

        assert_eq!(options.get_unnamed_len(), 1);
        assert_eq!(options.get_unnamed_value(0), Some(&String::from("test")));
        assert!(options.get_unnamed_value(1).is_none());
    }

    #[test]
    fn test_parse_cli_bool() {
        let config = CliConfig::new(&[
            ("b", "bb", 'a'), // note b and a are swapped
            ("a", "aa", 'b'),
            ("c", "cc", 'c'),
            ("d", "dd", 'd'),
            ("e", "ee", 'e'),
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

        eprintln!("{:?}", options);

        assert_eq!(options.get_named_bool("a", false), Ok(true));
        assert!(options.get_named_bool("b", false).is_err()); // multiple values
        assert_eq!(options.get_named_bool("d", true), Ok(true));
        assert_eq!(options.get_named_bool("d", false), Ok(false));
        assert_eq!(options.get_named_bool("c", false), Ok(true));

        assert_eq!(options.get_unnamed_len(), 1);
        assert_eq!(options.get_unnamed_value(0), Some(&String::from("remainder")));
    }
}
