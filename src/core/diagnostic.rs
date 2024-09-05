
use crate::core::syntax::SourceRange;

use std::fmt::{Display, Formatter};

pub struct Diagnostic {
    pub severity: DiagnosticSeverity,
    pub file: Option<String>,
    pub loc: Option<SourceRange>,
    pub message: String,
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        // path/to/file (line1, char1) - (line2, char2): details
        if let Some(file) = &self.file {
            write!(f, "{} ", file)?;
        }
        if let Some(loc) = self.loc {
            write!(f, "{:?}", loc)?;
        }
        if self.file.is_some() || self.loc.is_some() {
            write!(f, ": ")?;
        }
        write!(f, "{}", self.message)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiagnosticCategory {
    /// A syntax or other compilation error in a mu-calculus formula.
    Formula,

    /// An error in input or output, for instance when a given file does not
    /// exist or the process does not have the correct permissions.
    Io,

    /// A syntax error in an LTS file.
    Lts,

    /// An error not described by any of the other categories.
    Misc,

    /// A syntax or other compilation error in an mCRL2 model.
    Model,

    /// A syntax error in a parity game file.
    Pg,

    /// A tool usage error, for instance when the user incorrectly passes CLI
    /// arguments or passes a file that does not exist.
    Tool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Message,
}

pub struct DiagnosticContext {
    values: Vec<Diagnostic>,
}

impl DiagnosticContext {
    pub fn new() -> Self {
        DiagnosticContext {
            values: Vec::new(),
        }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.values.push(diagnostic);
    }

    pub fn is_ok(&self) -> bool {
        self.values.is_empty()
    }

    pub fn union_result<T, E>(&mut self, result: Result<T, E>) -> Result<T, ()>
    where
        E: Into<Diagnostic>,
    {
        match result {
            Ok(value) => return Ok(value),
            Err(error) => {
                self.push(error.into());
                Err(())
            },
        }
    }

    pub fn union_results<T, E, I>(&mut self, iterator: I) -> Result<Vec<T>, ()>
    where
        I: Iterator<Item = Result<T, E>>,
        E: Into<Diagnostic>,
    {
        let mut values = Vec::new();
        for result in iterator {
            match result {
                Ok(value) => values.push(value),
                Err(error) => self.push(error.into()),
            }
        }
        if self.is_ok() {
            Ok(values)
        } else {
            Err(())
        }
    }
}

impl Display for DiagnosticContext {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        for value in &self.values {
            write!(f, "{}", value)?;
        }
        Ok(())
    }
}

impl From<Diagnostic> for DiagnosticContext {
    fn from(value: Diagnostic) -> Self {
        let mut diagnostics = DiagnosticContext::new();
        diagnostics.push(value);
        diagnostics
    }
}

impl IntoIterator for DiagnosticContext {
    type Item = Diagnostic;
    type IntoIter = std::vec::IntoIter<Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

impl<'a> IntoIterator for &'a DiagnosticContext {
    type Item = &'a Diagnostic;
    type IntoIter = std::slice::Iter<'a, Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.iter()
    }
}
