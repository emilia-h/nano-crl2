
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};

/// An identifier, which is a usually short string that uniquely refers to a
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

/// A location in a source file, i.e. an inclusive interval over two `(line,
/// char)` coordinates.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourceLocation {
    start_line: usize,
    start_char: usize,
    end_line: usize,
    end_char: usize,
}

impl SourceLocation {
    /// Creates a new `SourceLocation` spanning `(start_line, start_char)` to
    /// `(end_line, end_char)`.
    pub const fn new(
        start_line: usize,
        start_char: usize,
        end_line: usize,
        end_char: usize,
    ) -> Self {
        SourceLocation { start_line, start_char, end_line, end_char }
    }

    pub fn get_start_line(&self) -> usize {
        self.start_line
    }

    pub fn get_start_char(&self) -> usize {
        self.start_char
    }

    pub fn get_end_line(&self) -> usize {
        self.end_line
    }

    pub fn get_end_char(&self) -> usize {
        self.end_char
    }

    /// Creates a new `SourceLocation` that spans two given locations.
    /// 
    /// In other words, returns the smallest location that is a superinterval
    /// of both.
    pub fn span(&self, other: &SourceLocation) -> Self {
        SourceLocation {
            start_line: usize::min(self.start_line, other.start_line),
            start_char: usize::min(self.start_char, other.start_char),
            end_line: usize::max(self.end_line, other.end_line),
            end_char: usize::max(self.end_char, other.end_char),
        }
    }
}

impl Ord for SourceLocation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_start_line().cmp(&other.get_start_line())
        .then(self.get_start_char().cmp(&other.get_start_char()))
    }
}

impl PartialOrd for SourceLocation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
