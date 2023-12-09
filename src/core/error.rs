
#[derive(Debug)]
pub enum Mcrl2Error {
    /// A tool usage error, for instance when the user incorrectly passes CLI
    /// arguments or passes a file that does not exist.
    ToolUsageError { message: String },

    /// A syntax error in an mCRL2 model.
    ModelSyntaxError { message: String, line: usize, character: usize },
}

impl From<std::io::Error> for Mcrl2Error {
    fn from(error: std::io::Error) -> Mcrl2Error {
        Mcrl2Error::ToolUsageError {
            message: format!("File error: {}", error)
        }
    }
}

#[macro_export]
macro_rules! propagate_error_into {
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
