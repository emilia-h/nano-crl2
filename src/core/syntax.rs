
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

/// A location in a text file.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct SourcePos {
    line: u32,
    character: u32,
}

impl SourcePos {
    /// Creates a new `SourceLoc` from its components
    pub const fn new(line: u32, character: u32) -> Self {
        SourcePos { line, character }
    }

    pub const fn get_line(&self) -> u32 {
        self.line
    }

    pub const fn get_char(&self) -> u32 {
        self.character
    }
}

impl Ord for SourcePos {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Less => Ordering::Less,
            Ordering::Equal => self.character.cmp(&other.character),
            Ordering::Greater => Ordering::Greater,
        }
    }
}

impl PartialOrd for SourcePos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// A selection in a source file, i.e. an inclusive interval over two `(line,
/// char)` coordinates.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct SourceRange {
    start_line: u32,
    start_char: u32,
    end_line: u32,
    end_char: u32,
}

impl SourceRange {
    /// Creates a new `SourceRange` spanning `(start_line, start_char)` to
    /// `(end_line, end_char)`.
    pub const fn new(
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
    ) -> Self {
        SourceRange { start_line, start_char, end_line, end_char }
    }

    pub const fn get_start(&self) -> SourcePos {
        SourcePos::new(self.start_line, self.start_char)
    }

    pub const fn get_start_line(&self) -> u32 {
        self.start_line
    }

    pub const fn get_start_char(&self) -> u32 {
        self.start_char
    }

    pub const fn get_end(&self) -> SourcePos {
        SourcePos::new(self.end_line, self.end_char)
    }

    pub const fn get_end_line(&self) -> u32 {
        self.end_line
    }

    pub const fn get_end_char(&self) -> u32 {
        self.end_char
    }

    /// Creates a new `SourceRange` that spans two given locations.
    /// 
    /// In other words, returns the smallest range that is a superinterval
    /// of both.
    pub fn span(&self, other: &SourceRange) -> Self {
        SourceRange {
            start_line: u32::min(self.start_line, other.start_line),
            start_char: u32::min(self.start_char, other.start_char),
            end_line: u32::max(self.end_line, other.end_line),
            end_char: u32::max(self.end_char, other.end_char),
        }
    }
}

impl Debug for SourceRange {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Display::fmt(&self, f)
    }
}

impl Display for SourceRange {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(
            f, "({}, {}) - ({}, {})",
            self.start_line, self.start_char,
            self.end_line, self.end_char,
        )
    }
}

impl Ord for SourceRange {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_start_line().cmp(&other.get_start_line())
        .then(self.get_start_char().cmp(&other.get_start_char()))
    }
}

impl PartialOrd for SourceRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
