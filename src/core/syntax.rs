
#[derive(Debug)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourceLocation {
    line: usize,
    character: usize,
}

impl SourceLocation {
    pub fn new(line: usize, character: usize) -> Self {
        SourceLocation { line, character }
    }

    pub fn get_line(&self) -> usize {
        self.line
    }

    pub fn get_char(&self) -> usize {
        self.character
    }
}
