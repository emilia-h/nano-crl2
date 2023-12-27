
use std::fmt::{Display, Formatter};

/// An identifier, which is usually a short string that uniquely refers to a
/// declaration somewhere else.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Identifier {
    value: String,
}

impl Identifier {
    pub fn new(value: &str) -> Self {
        Identifier { value: String::from(value) }
    }

    pub fn get_value(&self) -> &str {
        &self.value
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.value)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourceLocation {
    line: usize,
    character: usize,
}

impl SourceLocation {
    pub const fn new(line: usize, character: usize) -> Self {
        SourceLocation { line, character }
    }

    pub fn get_line(&self) -> usize {
        self.line
    }

    pub fn get_char(&self) -> usize {
        self.character
    }
}
