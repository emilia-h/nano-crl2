
use crate::core::error::Mcrl2Error;

use std::fmt::{Display, Formatter};
use std::str::Chars;

#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub character: usize,
}

impl Into<Mcrl2Error> for LexError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::ModelSyntaxError {
            message: self.message,
            line: self.line,
            character: self.character,
        }
    }
}

/// A single lexical element
/// # See also
/// https://www.mcrl2.org/web/user_manual/language_reference/lex.html
#[derive(Clone, Debug)]
pub enum LexicalElement {
    OpeningParen, // (
    ClosingParen, // )
    OpeningBracket, // [
    ClosingBracket, // ]
    OpeningBrace, // {
    ClosingBrace, // }

    Tilde, // ~
    ExclamationMark, // !
    AtSign, // @
    HashSign, // #
    DollarSign, // $
    Circonflex, // ^
    Ampersand, // &
    Asterisk, // *
    Dash, // -
    Equals, // =
    Plus, // +
    Pipe, // |
    Semicolon, // ;
    Colon, // :
    Comma, // ,
    LessThan, // <
    Period, // .
    GreaterThan, // >
    Slash, // /
    QuestionMark, // ?

    Act,
    Allow,
    Block,
    Comm,
    Cons,
    Delay,
    Div,
    End,
    Eqn,
    Exists,
    Forall,
    Glob,
    Hide,
    If,
    In,
    Init,
    Lambda,
    Map,
    Mod,
    Mu,
    Nu,
    Pbes,
    Proc,
    Rename,
    Sort,
    Struct,
    Sum,
    Val,
    Var,
    Whr,
    Yaled,
    Bag,
    Bool,
    Int,
    List,
    Nat,
    Pos,
    Real,
    Set,
    Delta,
    False,
    Nil,
    Tau,
    True,

    Identifier(String),

    Comment(String),

    // TODO: should be arbitrarily large
    Integer(u64),
}

impl Display for LexicalElement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        if let Some(value) = match self {
            LexicalElement::OpeningParen => Some("("),
            LexicalElement::ClosingParen => Some(")"),
            LexicalElement::OpeningBracket => Some("["),
            LexicalElement::ClosingBracket => Some("]"),
            LexicalElement::OpeningBrace => Some("{"),
            LexicalElement::ClosingBrace => Some("}"),
            LexicalElement::Tilde => Some("~"),
            LexicalElement::ExclamationMark => Some("!"),
            LexicalElement::AtSign => Some("@"),
            LexicalElement::HashSign => Some("#"),
            LexicalElement::DollarSign => Some("$"),
            LexicalElement::Circonflex => Some("^"),
            LexicalElement::Ampersand => Some("&"),
            LexicalElement::Asterisk => Some("*"),
            LexicalElement::Dash => Some("-"),
            LexicalElement::Equals => Some("="),
            LexicalElement::Plus => Some("+"),
            LexicalElement::Pipe => Some("|"),
            LexicalElement::Semicolon => Some(";"),
            LexicalElement::Colon => Some(":"),
            LexicalElement::Comma => Some(","),
            LexicalElement::LessThan => Some("<"),
            LexicalElement::Period => Some("."),
            LexicalElement::GreaterThan => Some(">"),
            LexicalElement::Slash => Some("/"),
            LexicalElement::QuestionMark => Some("?"),
            LexicalElement::Act => Some("act"),
            LexicalElement::Allow => Some("allow"),
            LexicalElement::Block => Some("block"),
            LexicalElement::Comm => Some("comm"),
            LexicalElement::Cons => Some("cons"),
            LexicalElement::Delay => Some("delay"),
            LexicalElement::Div => Some("div"),
            LexicalElement::End => Some("end"),
            LexicalElement::Eqn => Some("eqn"),
            LexicalElement::Exists => Some("exists"),
            LexicalElement::Forall => Some("forall"),
            LexicalElement::Glob => Some("glob"),
            LexicalElement::Hide => Some("hide"),
            LexicalElement::If => Some("if"),
            LexicalElement::In => Some("in"),
            LexicalElement::Init => Some("init"),
            LexicalElement::Lambda => Some("lambda"),
            LexicalElement::Map => Some("map"),
            LexicalElement::Mod => Some("mod"),
            LexicalElement::Mu => Some("mu"),
            LexicalElement::Nu => Some("nu"),
            LexicalElement::Pbes => Some("pbes"),
            LexicalElement::Proc => Some("proc"),
            LexicalElement::Rename => Some("rename"),
            LexicalElement::Sort => Some("sort"),
            LexicalElement::Struct => Some("struct"),
            LexicalElement::Sum => Some("sum"),
            LexicalElement::Val => Some("val"),
            LexicalElement::Var => Some("var"),
            LexicalElement::Whr => Some("whr"),
            LexicalElement::Yaled => Some("yaled"),
            LexicalElement::Bag => Some("Bag"),
            LexicalElement::Bool => Some("Bool"),
            LexicalElement::Int => Some("Int"),
            LexicalElement::List => Some("List"),
            LexicalElement::Nat => Some("Nat"),
            LexicalElement::Pos => Some("Pos"),
            LexicalElement::Real => Some("Real"),
            LexicalElement::Set => Some("Set"),
            LexicalElement::Delta => Some("delta"),
            LexicalElement::False => Some("false"),
            LexicalElement::Nil => Some("nil"),
            LexicalElement::Tau => Some("tau"),
            LexicalElement::True => Some("true"),
            LexicalElement::Identifier(string) => Some(string.as_str()),
            LexicalElement::Comment(string) => {
                write!(f, "% {}", string)?;
                None
            },
            LexicalElement::Integer(value) => {
                write!(f, "{}", value)?;
                None
            },
        } {
            write!(f, "\"{}\"", value)?;
        }
        Ok(())
    }
}

pub struct Token {
    pub value: LexicalElement,
    pub line: usize,
    pub character: usize,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Display::fmt(&self, f)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{} ({}, {})", self.value, self.line, self.character)?;
        Ok(())
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, LexError> {
    Lexer::new(input).parse_tokens()
}

struct Lexer<'a> {
    curr_line: usize,
    curr_char: usize,
    iterator: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            curr_line: 0,
            curr_char: 0,
            iterator: input.chars(),
        }
    }

    pub fn parse_tokens(&mut self) -> Result<Vec<Token>, LexError> {
        self.skip_whitespace();

        let mut tokens = Vec::new();

        let mut in_identifier = false;
        let mut identifier = String::new();

        let mut in_integer = false;
        let mut integer: u64 = 0;

        let mut in_comment = false;
        let mut comment = String::new();

        let mut in_symbol = false; // TODO

        let new_line = self.curr_line;
        let new_char = self.curr_char;

        // parse next lexical element
        while let Some(next) = self.iterator.clone().next() {
            if is_number_character(next) { // 0 - 9
                if in_identifier {
                    identifier.push(next);
                } else {
                    in_integer = true;
                    integer *= 10;
                    integer += next as u64 - '0' as u64;
                }
                self.advance_char(next);
                continue;
            }

            if is_word_character(next) { // a - z, A - Z, _, '
                if in_integer {
                    return Err(LexError {
                        message: String::from("found word character right after integer"),
                        line: 0,
                        character: 0,
                    });
                }
                in_identifier = true;
                identifier.push(next);
                self.advance_char(next);
                continue;
            }

            if next == '%' {
                in_comment = true;
                self.advance_char(next);
                continue;
            }

            if let Some(e) = match next {
                '(' => Some(LexicalElement::OpeningParen),
                ')' => Some(LexicalElement::ClosingParen),
                '[' => Some(LexicalElement::OpeningBracket),
                ']' => Some(LexicalElement::ClosingBracket),
                '{' => Some(LexicalElement::OpeningBrace),
                '}' => Some(LexicalElement::ClosingBrace),
                '~' => Some(LexicalElement::Tilde),
                '!' => Some(LexicalElement::ExclamationMark),
                '@' => Some(LexicalElement::AtSign),
                '#' => Some(LexicalElement::HashSign),
                '$' => Some(LexicalElement::DollarSign),
                '^' => Some(LexicalElement::Circonflex),
                '&' => Some(LexicalElement::Ampersand),
                '*' => Some(LexicalElement::Asterisk),
                '-' => Some(LexicalElement::Dash),
                '=' => Some(LexicalElement::Equals),
                '+' => Some(LexicalElement::Plus),
                '|' => Some(LexicalElement::Pipe),
                ';' => Some(LexicalElement::Semicolon),
                ':' => Some(LexicalElement::Colon),
                ',' => Some(LexicalElement::Comma),
                '<' => Some(LexicalElement::LessThan),
                '.' => Some(LexicalElement::Period),
                '>' => Some(LexicalElement::GreaterThan),
                '/' => Some(LexicalElement::Slash),
                '?' => Some(LexicalElement::QuestionMark),
                ' ' => None,
                '\n' => None,
                '\r' => None,
                _ => {
                    return Err(LexError {
                        message: format!("Found unknown character {:?}", next),
                        character: 0,
                        line: 0,
                    });
                }
            } {
                if !in_identifier && !in_integer && !in_comment {
                    // in_symbol = true;
                    tokens.push(Token {
                        value: e,
                        line: new_line,
                        character: new_char,
                    });
                    self.advance_char(next);
                    self.skip_whitespace();
                    continue;
                }
            }

            // if while building integer/identifier/comment, another character
            // is found, then finish the token
            if in_comment {
                self.advance_char(next);
                if next == '\n' {
                    in_comment = false;
                    continue;
                } else {
                    comment.push(next);
                }
                self.skip_whitespace();
            }

            if in_identifier {
                let e = match identifier.as_str() {
                    "act" => LexicalElement::Act,
                    "allow" => LexicalElement::Allow,
                    "block" => LexicalElement::Block,
                    "comm" => LexicalElement::Comm,
                    "cons" => LexicalElement::Cons,
                    "delay" => LexicalElement::Delay,
                    "div" => LexicalElement::Div,
                    "end" => LexicalElement::End,
                    "eqn" => LexicalElement::Eqn,
                    "exists" => LexicalElement::Exists,
                    "forall" => LexicalElement::Forall,
                    "glob" => LexicalElement::Glob,
                    "hide" => LexicalElement::Hide,
                    "if" => LexicalElement::If,
                    "in" => LexicalElement::In,
                    "init" => LexicalElement::Init,
                    "lambda" => LexicalElement::Lambda,
                    "map" => LexicalElement::Map,
                    "mod" => LexicalElement::Mod,
                    "mu" => LexicalElement::Mu,
                    "nu" => LexicalElement::Nu,
                    "pbes" => LexicalElement::Pbes,
                    "proc" => LexicalElement::Proc,
                    "rename" => LexicalElement::Rename,
                    "sort" => LexicalElement::Sort,
                    "struct" => LexicalElement::Struct,
                    "sum" => LexicalElement::Sum,
                    "val" => LexicalElement::Val,
                    "var" => LexicalElement::Var,
                    "whr" => LexicalElement::Whr,
                    "yaled" => LexicalElement::Yaled,
                    "bag" => LexicalElement::Bag,
                    "Bool" => LexicalElement::Bool,
                    "Int" => LexicalElement::Int,
                    "List" => LexicalElement::List,
                    "Nat" => LexicalElement::Nat,
                    "Pos" => LexicalElement::Pos,
                    "Real" => LexicalElement::Real,
                    "Set" => LexicalElement::Set,
                    "delta" => LexicalElement::Delta,
                    "false" => LexicalElement::False,
                    "nil" => LexicalElement::Nil,
                    "tau" => LexicalElement::Tau,
                    "true" => LexicalElement::True,
                    _ => {
                        let e = LexicalElement::Identifier(identifier);
                        e
                    },
                };

                tokens.push(Token {
                    value: e,
                    line: new_line,
                    character: new_char,
                });
                identifier = String::new();
                in_identifier = false;
                self.skip_whitespace();
                continue;
            }

            if in_integer {
                tokens.push(Token {
                    value: LexicalElement::Integer(integer),
                    line: new_line,
                    character: new_char,
                });
                integer = 0;
                in_integer = false;
                self.skip_whitespace();
                continue;
            }

            if in_symbol {
                // TODO
            }
        }

        Ok(tokens)
    }

    fn skip_whitespace(&mut self) {
        while let Some(next) = self.iterator.clone().next() {
            if next == '\n' {
                self.curr_line += 1;
                self.curr_char = 0;
            } else if next == ' ' || next == '\r' {
                self.curr_char += 1;
            } else {
                return;
            }
            self.advance_char(next);
        }
    }

    fn advance_char(&mut self, ch: char) {
        self.iterator.next();
        if ch == '\n' {
            self.curr_line += 1;
            self.curr_char = 0;
        } else {
            self.curr_char += 1;
        }
    }
}

fn is_number_character(c: char) -> bool {
    let ascii = c as u8;
    ascii >= '0' as u8 && ascii <= '9' as u8
}

fn is_word_character(c: char) -> bool {
    let ascii = c as u8;

    (ascii >= 'a' as u8 && ascii <= 'z' as u8) ||
    (ascii >= 'A' as u8 && ascii <= 'Z' as u8) ||
    c == '_' ||
    c == '\'' ||
    (ascii >= '0' as u8 && ascii <= '9' as u8)
}
