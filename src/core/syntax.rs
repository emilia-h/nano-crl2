
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};

/// An identifier, which is usually a short string that uniquely refers to a
/// declaration somewhere else.
#[derive(Clone, Eq, Hash, PartialEq)]
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

impl Debug for Identifier {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Display::fmt(self, f)
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

impl Ord for SourceLocation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_line().cmp(&other.get_line())
        .then(self.get_char().cmp(&other.get_char()))
    }
}

impl PartialOrd for SourceLocation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
