
use std::str::Chars;

/// A generic parser that iterates over individual characters.
/// 
/// It uses the [`Chars`] iterator to iterate over the given input.
/// 
/// [`Chars`]: https://doc.rust-lang.org/std/str/struct.Chars.html
pub struct CharParser<'a, E, F> where F: Fn(String, usize, usize) -> E {
    chars: Chars<'a>,
    curr_line: usize,
    curr_char: usize,
    to_err: F,
}

impl<'a, E, F> CharParser<'a, E, F> where F: Fn(String, usize, usize) -> E {
    /// Creates a new parser from a string input, with a custom erroring
    /// function.
    /// 
    /// The `to_err` function should take a message, a line number and
    /// character number and construct an error from this.
    pub fn new(input: &'a str, to_err: F) -> Self {
        CharParser {
            chars: input.chars(),
            curr_line: 0,
            curr_char: 0,
            to_err,
        }
    }

    pub fn parse_number(&mut self) -> Result<u64, E> {
        self.skip_whitespace();

        let mut result = 0u64;
        let mut found = false;
        while let Some(c) = self.get_char() {
            if c as u8 >= '0' as u8 && c as u8 <= '9' as u8 {
                found = true;
                self.next_char();
                result *= 10;
                result += c as u64 - '0' as u64;
            } else {
                break;
            }
        }

        if found {
            Ok(result)
        } else {
            let message = "expected nonnegative number";
            Err(self.error(String::from(message)))
        }
    }

    pub fn parse_string(&mut self) -> Result<String, E> {    
        self.expect_chars("\"")?;
        let mut string = String::new();
        while let Some(c) = self.next_char() {
            if c == '"' {
                break;
            }
            string.push(c);
        }
        Ok(string)
    }

    pub fn expect_chars(&mut self, string: &str) -> Result<(), E> {
        self.skip_whitespace();

        let mut chars2 = string.chars();
        loop {
            if let Some(c2) = chars2.next() {
                if let Some(c1) = self.next_char() {
                    if c1 != c2 {
                        break Err(self.error(format!("expected \"{}\" but got \"{}\"", c2, c1)));
                    } // else we are happy, so just continue
                } else {
                    break Err(self.error(format!("line not long enough, expected \"{}\"", string)));
                }
            } else {
                break Ok(())
            }
        }
    }

    /// Advances a `Chars` iterator for as long as there are whitespace
    /// characters (spaces, line breaks or tabs).
    /// 
    /// This could also be seen as a "strip-left" or "trim-left" operation.
    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.clone().next() {
            if c == ' ' || c == '\r' || c == '\n' || c == '\t' {
                self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn get_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn next_char(&mut self) -> Option<char> {
        if let Some(ch) = self.chars.next() {
            if ch == '\n' {
                self.curr_line += 1;
                self.curr_char = 0;
            } else {
                self.curr_char += 1;
            }
            Some(ch)
        } else {
            None
        }
    }

    pub fn error(&self, message: String) -> E {
        (self.to_err)(message, self.curr_line, self.curr_char)
    }
}
