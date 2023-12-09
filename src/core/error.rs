
#[derive(Debug)]
pub enum Mcrl2Error {
    /// A tool usage error, for instance when the user incorrectly passes CLI
    /// arguments or passes a file that does not exist.
    ToolUsageError { message: String },

    /// A syntax or other compilation error in a mu-calculus formula.
    FormulaError { message: String },

    /// A syntax or other compilation error in an mCRL2 model.
    ModelError { message: String, line: usize, character: usize },

    /// A syntax error in an LTS file.
    LtsSyntaxError { message: String, line: usize },
}

impl From<std::io::Error> for Mcrl2Error {
    fn from(error: std::io::Error) -> Mcrl2Error {
        Mcrl2Error::ToolUsageError {
            message: format!("File error: {}", error)
        }
    }
}

#[macro_export]
macro_rules! try_into {
    ($expr:expr) => {
        match $expr {
            Ok(value) => {
                value
            },
            Err(error) => {
                return Err(error.into());
            },
        }
    }
}
