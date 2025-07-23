
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

    pub fn new_from_owned(value: String) -> Self {
        Identifier { value }
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

/// A (nameless) identifier that, within a given context, refers to a specific
/// module or other kind of input (like a file).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ModuleId {
    pub(crate) index: usize,
}

impl Debug for ModuleId {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.index)
    }
}

/// A character position `(line, character)` in a text file.
/// 
/// These character positions are lexicographically ordered, i.e. `(l1, c1) <
/// (l2, c2)` is true iff `l1 < l2 || (l1 == l2 && c1 < c2)`.
/// 
/// Note that the position of a character is different from the position of the
/// cursor in an editor! A cursor is positioned in between two characters, so
/// be careful not to mix up these two concepts.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct SourcePos(u32, u32);

impl SourcePos {
    pub const fn new(line: u32, character: u32) -> Self {
        SourcePos(line, character)
    }

    pub const fn get_line(&self) -> u32 {
        self.0
    }

    pub const fn get_char(&self) -> u32 {
        self.1
    }
}

impl Debug for SourcePos {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl Ord for SourcePos {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
            .then(self.1.cmp(&other.1))
    }
}

impl PartialOrd for SourcePos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourceCursorPos(u32, u32);

impl SourceCursorPos {
    pub const fn new(line: u32, character: u32) -> Self {
        SourceCursorPos(line, character)
    }

    pub const fn get_line(self) -> u32 {
        self.0
    }

    pub const fn get_char(self) -> u32 {
        self.1
    }

    /// The distance between two cursor positions is a tuple of:
    /// - The number of lines distance from the smaller position and the larger
    /// position
    /// - The signed difference between the character of the smaller position
    /// and the character of the larger position
    /// 
    /// We must use a signed integer for the second value, since it can be
    /// negative (the character of the smaller position could be behind the
    /// character of the larger position, if the two positions are not on the
    /// same line).
    /// 
    /// This is sort of a distance metric, because it satisfies the following
    /// laws:
    /// - `dist(a, b) == (0, 0)` iff `a == b`
    /// - `dist(a, b) == dist(b, a)` (symmetry)
    /// - `dist(a, b) + dist(b, c) >= dist(a, c)` (triangle inequality)
    /// where `>=` is lexicographic comparison
    pub fn dist(self, other: SourceCursorPos) -> (u32, i64) {
        let (a, b) = if self < other {
            (self, other)
        } else {
            (other, self)
        };
        (b.0 - a.0, b.1 as i64 - a.1 as i64)
    }
}

impl Ord for SourceCursorPos {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
            .then(self.1.cmp(&other.1))
    }
}

impl PartialOrd for SourceCursorPos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// A selection in a source file, i.e. a start-inclusive end-exclusive interval
/// over two `(line, char)` coordinates.
/// 
/// Note that this represents a selection, so the start and end positions are
/// cursor positions and not character positions (a character position is
/// represented using `SourcePos`). This is why it is an inclusive-exclusive
/// interval.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct SourceRange {
    start_line: u32,
    start_char: u32,
    end_line: u32,
    end_char: u32,
}

impl SourceRange {
    /// Creates a new `SourceRange` spanning `(start_line, start_char)` to
    /// `(end_line, end_char)` exclusive.
    /// 
    /// # Panics
    /// The starting position must come before the ending position, or this
    /// function will panic.
    pub const fn new(
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
    ) -> Self {
        assert!(start_line <= end_line);
        assert!(start_line != end_line || start_char <= end_char);
        SourceRange { start_line, start_char, end_line, end_char }
    }

    pub const EMPTY: Self = SourceRange::new(0, 0, 0, 0);

    pub const fn get_start(&self) -> SourceCursorPos {
        SourceCursorPos(self.start_line, self.start_char)
    }

    pub const fn get_start_line(&self) -> u32 {
        self.start_line
    }

    pub const fn get_start_char(&self) -> u32 {
        self.start_char
    }

    pub const fn get_end(&self) -> SourceCursorPos {
        SourceCursorPos(self.end_line, self.end_char)
    }

    pub const fn get_end_line(&self) -> u32 {
        self.end_line
    }

    pub const fn get_end_char(&self) -> u32 {
        self.end_char
    }

    /// Returns the distance between the start position and the end position.
    /// 
    /// This is equivalent to `self.get_start().dist(self.get_end())`.
    pub const fn get_distance(&self) -> (u32, i64) {
        (
            self.end_line - self.start_line,
            self.end_char as i64 - self.start_char as i64
        )
    }

    /// Compares two source ranges by the distance between their .
    /// 
    /// Note that this is not really a "canonical" way of ordering two ranges,
    /// so we do not implement `Ord` and `PartialOrd` for this.
    pub fn cmp_distance(&self, other: &SourceRange) -> Ordering {
        self.get_distance().cmp(&other.get_distance())
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

    /// Returns whether or not the given character position `pos` is within
    /// this selection.
    pub const fn contains(&self, pos: SourcePos) -> bool {
        self.start_line <= pos.0 && pos.0 <= self.end_line &&
        (self.start_line != pos.0 || self.start_char <= pos.1) &&
        (pos.0 != self.end_line || pos.1 < self.end_char)
    }

    pub const fn contains_cursor(&self, cursor_pos: SourceCursorPos) -> bool {
        self.start_line <= cursor_pos.0 && cursor_pos.0 <= self.end_line &&
        (self.start_line != cursor_pos.0 || self.start_char <= cursor_pos.1) &&
        (cursor_pos.0 != self.end_line || cursor_pos.1 <= self.end_char)
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
