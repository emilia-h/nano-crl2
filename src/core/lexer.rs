//! The lexer is a facility for transforming a string into tokens (AKA
//! tokenization).
//! 
//! For instance, the string `"eqn test(a) = a ++ a;"` would be turned into
//! the tokens `["eqn", "test", "(", "a", ")", "=", "a", "++", "a", ";"]`.
//! 
//! Tokenization makes parsing a lot easier. This lexer is used for both the
//! [mCRL2 model parser] and the [mu-calculus parser].
//! 
//! [mCRL2 model parser]: ../parser/index.html
//! [mu-calculus parser]: ../formula/index.html

use crate::core::error::Mcrl2Error;
use crate::core::syntax::SourceRange;

use std::fmt::{Display, Formatter};
use std::str::Chars;

/// An error during the tokenization process, which happens when the input is
/// invalid.
/// 
/// # Examples
/// ```
/// # use nano_crl2::core::lexer::tokenize;
/// // a word character is not allowed after a number
/// let error = tokenize("123t").unwrap_err();
/// eprintln!("{:?}", error);
/// ```
#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub line: u32,
    pub character: u32,
}

impl LexError {
    pub fn get_loc(&self) -> SourceRange {
        SourceRange::new(
            self.line,
            self.character,
            self.line,
            self.character,
        )
    }
}

impl Into<Mcrl2Error> for LexError {
    fn into(self) -> Mcrl2Error {
        let loc = self.get_loc();
        Mcrl2Error::ModelError { message: self.message, loc }
    }
}

/// Represents a single lexical element, for instance `"++"`, `my_map`, `123`
/// or `struct`.
/// 
/// # See also
/// [The mCRL2 spec on this](https://www.mcrl2.org/web/user_manual/language_reference/lex.html)
#[derive(Clone, Debug, Eq, PartialEq)]
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

    DoublePipe, // ||
    DoubleAmpersand, // &&
    DoubleEquals, // ==
    NotEquals, // !=
    LessThanEquals, // <=
    GreaterThanEquals, // >=
    Diamond, // <>
    Arrow, // ->
    ThickArrow, // =>
    ConsOperator, // |>
    SnocOperator, // <|
    Concat, // ++
    DoublePipeUnderscore,

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
    FBag,
    FSet,
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
    DocComment(String),

    // TODO: should be arbitrarily large
    Integer(u64),
}

impl LexicalElement {
    pub fn get_length(&self) -> usize {
        use LexicalElement::*;

        match self {
            OpeningParen | ClosingParen | OpeningBracket | ClosingBracket |
            OpeningBrace | ClosingBrace | Tilde | ExclamationMark |
            AtSign | HashSign | DollarSign | Circonflex | Ampersand |
            Asterisk | Dash | Equals | Plus | Pipe | Semicolon | Colon |
            Comma | LessThan | Period | GreaterThan | Slash | QuestionMark => 1,

            DoublePipe | DoubleAmpersand| DoubleEquals | NotEquals |
            LessThanEquals | GreaterThanEquals | Diamond | Arrow | ThickArrow |
            ConsOperator | SnocOperator | Concat => 2,

            DoublePipeUnderscore => 3,

            Act => 3,
            Allow => 5,
            Block => 5,
            Comm => 4,
            Cons => 4,
            Delay => 5,
            Div => 3,
            End => 3,
            Eqn => 3,
            Exists => 6,
            Forall => 6,
            Glob => 4,
            Hide => 4,
            If => 2,
            In => 2,
            Init => 4,
            Lambda => 6,
            Map => 3,
            Mod => 3,
            Mu => 2,
            Nu => 2,
            Pbes => 4,
            Proc => 4,
            Rename => 6,
            Sort => 4,
            Struct => 6,
            Sum => 3,
            Val => 3,
            Var => 3,
            Whr => 3,
            Yaled => 5,
            Bag => 3,
            Bool => 4,
            FBag => 4,
            FSet => 4,
            Int => 3,
            List => 4,
            Nat => 3,
            Pos => 3,
            Real => 4,
            Set => 3,
            Delta => 5,
            False => 5,
            Nil => 3,
            Tau => 3,
            True => 4,

            Identifier(string) => string.len(),
            Comment(string) => string.len() + 1,
            DocComment(string) => string.len() + 2,
            Integer(value) => value.checked_ilog10().unwrap_or(0) as usize + 1,
        }
    }
}

impl Display for LexicalElement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        use LexicalElement::*;

        let value = match self {
            OpeningParen => "(",
            ClosingParen => ")",
            OpeningBracket => "[",
            ClosingBracket => "]",
            OpeningBrace => "{",
            ClosingBrace => "}",
            Tilde => "~",
            ExclamationMark => "!",
            AtSign => "@",
            HashSign => "#",
            DollarSign => "$",
            Circonflex => "^",
            Ampersand => "&",
            Asterisk => "*",
            Dash => "-",
            Equals => "=",
            Plus => "+",
            Pipe => "|",
            Semicolon => ";",
            Colon => ":",
            Comma => ",",
            LessThan => "<",
            Period => ".",
            GreaterThan => ">",
            Slash => "/",
            QuestionMark => "?",
            DoublePipe => "||",
            DoubleAmpersand => "&&",
            DoubleEquals => "==",
            NotEquals => "!=",
            LessThanEquals => "<=",
            GreaterThanEquals => ">=",
            Diamond => "<>",
            Arrow => "->",
            ThickArrow => "=>",
            ConsOperator => "|>",
            SnocOperator => "<|",
            Concat => "++",
            DoublePipeUnderscore => "||_",
            Act => "act",
            Allow => "allow",
            Block => "block",
            Comm => "comm",
            Cons => "cons",
            Delay => "delay",
            Div => "div",
            End => "end",
            Eqn => "eqn",
            Exists => "exists",
            Forall => "forall",
            Glob => "glob",
            Hide => "hide",
            If => "if",
            In => "in",
            Init => "init",
            Lambda => "lambda",
            Map => "map",
            Mod => "mod",
            Mu => "mu",
            Nu => "nu",
            Pbes => "pbes",
            Proc => "proc",
            Rename => "rename",
            Sort => "sort",
            Struct => "struct",
            Sum => "sum",
            Val => "val",
            Var => "var",
            Whr => "whr",
            Yaled => "yaled",
            Bag => "Bag",
            Bool => "Bool",
            FBag => "FBag",
            FSet => "FSet",
            Int => "Int",
            List => "List",
            Nat => "Nat",
            Pos => "Pos",
            Real => "Real",
            Set => "Set",
            Delta => "delta",
            False => "false",
            Nil => "nil",
            Tau => "tau",
            True => "true",
            Identifier(string) => string.as_str(),
            Comment(string) => {
                write!(f, "%{}", string)?;
                return Ok(());
            },
            DocComment(string) => {
                write!(f, "%%{}", string)?;
                return Ok(());
            },
            Integer(value) => {
                write!(f, "{}", value)?;
                return Ok(());
            },
        };
        write!(f, "{}", value)?;
        Ok(())
    }
}

/// Represents a single token with information about the location of the token
/// in the original string (line and character on that line).
#[derive(Clone, Eq, PartialEq)]
pub struct Token {
    pub value: LexicalElement,
    pub loc: SourceRange,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Display::fmt(&self, f)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{} ({}, {})", self.value, self.loc.get_start_line(), self.loc.get_start_char())?;
        Ok(())
    }
}

/// Tokenizes a string of characters.
/// 
/// Returns: a vector of `Token`s, or an error if the input was in an invalid
/// format.
pub fn tokenize(input: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(input);
    lexer.parse_tokens()?;
    Ok(lexer.into_tokens())
}

/// Takes a list of characters and turns it back into a string.
/// 
/// # Panics
/// This assumes that the tokens are sorted on location, which is guaranteed
/// when the tokens came from a [`tokenize`] call.
/// 
/// [`tokenize`]: ./fn.tokenize.html
pub fn reconstruct_from_tokens(input: &[Token]) -> String {
    let mut result = String::new();

    let mut curr_line = 0;
    let mut curr_char = 0;
    for token in input {
        assert!(token.loc.get_start_line() >= curr_line);
        if token.loc.get_start_line() == curr_line {
            assert!(token.loc.get_start_char() >= curr_char);
        }

        while curr_line < token.loc.get_start_line() {
            result.push('\n');
            curr_line += 1;
            curr_char = 0;
        }
        while curr_char < token.loc.get_start_char() {
            result.push(' ');
            curr_char += 1;
        }

        let string = format!("{}", token.value);
        result.push_str(&string);
        curr_char += string.len() as u32;
    }

    result
}

struct Lexer<'a> {
    tokens: Vec<Token>,
    curr_line: u32,
    curr_char: u32,
    token_line: u32,
    token_char: u32,
    iterator: Chars<'a>,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer` from an input string.
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            tokens: Vec::new(),
            curr_line: 0,
            curr_char: 0,
            token_line: 0,
            token_char: 0,
            iterator: input.chars(),
        }
    }

    /// Tokenizes the string given in [`Lexer::new`](./fn.new.html).
    pub fn parse_tokens(&mut self) -> Result<(), LexError> {
        self.skip_whitespace();
        self.token_char = self.curr_char;
        self.token_line = self.curr_line;

        let mut in_identifier = false;
        let mut identifier = String::new();

        let mut in_integer = false;
        let mut integer: u64 = 0;

        let mut symbol = None;

        // when we are out of characters, we shouldn't forget to add the last
        // token, which is why this is a `loop`
        loop {
            let option_next = self.iterator.clone().next();
            if let Some(next) = option_next {
                // if next == '\r' {
                //     // skip this cursed character
                //     self.advance_char(next);
                //     continue;
                // }

                if symbol.is_none() && is_number_character(next) {
                    // 0 - 9
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

                if symbol.is_none() && is_word_character(next) {
                    // a - z, A - Z, _, ', 0-9
                    if in_integer {
                        return Err(LexError {
                            message: String::from("found word character right after integer"),
                            line: self.curr_line as u32,
                            character: self.curr_char as u32,
                        });
                    }
                    in_identifier = true;
                    identifier.push(next);
                    self.advance_char(next);
                    continue;
                }

                if !in_integer && !in_identifier && symbol.is_none() && next == '%' {
                    self.advance_char(next);
                    let mut is_doc_comment = false;
                    let mut comment = String::new();
                    if let Some(ch) = self.iterator.clone().next() {
                        if ch == '%' {
                            self.advance_char(ch);
                            is_doc_comment = true;
                        }
                    }

                    while let Some(ch) = self.iterator.clone().next() {
                        if ch == '\r' || ch == '\n' {
                            break;
                        }
                        self.advance_char(ch);

                        comment.push(ch);
                    }

                    if is_doc_comment {
                        self.push_token(LexicalElement::DocComment(comment));
                    } else {
                        self.push_token(LexicalElement::Comment(comment));
                    }

                    continue;
                }

                if !in_identifier && !in_integer {
                    if let Some(prev_symbol) = &symbol {
                        if let Some(combined) = combine_symbols(prev_symbol, next) {
                            symbol = Some(combined);
                            self.advance_char(next);
                            continue;
                        }
                    } else if let Some(e) = match next {
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
                                message: format!("Found unexpected character {:?}", next),
                                line: self.curr_line,
                                character: self.curr_char,
                            });
                        }
                    } {
                        self.advance_char(next);
                        symbol = Some(e);
                        continue;
                    }
                }
            }

            // if while building integer/identifier/comment/symbol, another
            // character is found, then finish the token

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
                    "Bag" => LexicalElement::Bag,
                    "Bool" => LexicalElement::Bool,
                    "FBag" => LexicalElement::FBag,
                    "FSet" => LexicalElement::FSet,
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

                self.push_token(e);
                identifier = String::new();
                in_identifier = false;
            }

            if in_integer {
                self.push_token(LexicalElement::Integer(integer));
                integer = 0;
                in_integer = false;
            }

            if let Some(e) = symbol {
                self.push_token(e);
                symbol = None;
            }

            if option_next.is_none() {
                break; // out of characters
            }
        }

        Ok(())
    }

    /// Returns the tokens parsed by `parse_tokens`, consuming the `Lexer`.
    pub fn into_tokens(self) -> Vec<Token> {
        self.tokens
    }

    fn skip_whitespace(&mut self) {
        while let Some(next) = self.iterator.clone().next() {
            if next != '\n' && next != ' ' && next != '\r' && next != '\t' {
                break;
            }
            self.advance_char(next);
        }
        self.token_line = self.curr_line;
        self.token_char = self.curr_char;
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

    fn push_token(&mut self, element: LexicalElement) {
        assert_eq!(self.curr_line, self.token_line);
        assert!(self.curr_char >= self.token_char);
        self.tokens.push(Token {
            value: element,
            loc: SourceRange::new(
                self.token_line as u32,
                self.token_char as u32,
                self.curr_line as u32,
                self.curr_char as u32,
            ),
        });
        self.skip_whitespace();
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

fn combine_symbols(symbol: &LexicalElement, next: char) -> Option<LexicalElement> {
    use LexicalElement::*;

    match (symbol, next) {
        (Pipe, '|') => Some(DoublePipe),
        (Ampersand, '&') => Some(DoubleAmpersand),
        (Equals, '=') => Some(DoubleEquals),
        (ExclamationMark, '=') => Some(NotEquals),
        (LessThan, '=') => Some(LessThanEquals),
        (GreaterThan, '=') => Some(GreaterThanEquals),
        (LessThan, '>') => Some(Diamond),
        (Dash, '>') => Some(Arrow),
        (Equals, '>') => Some(ThickArrow),
        (Pipe, '>') => Some(ConsOperator),
        (LessThan, '|') => Some(SnocOperator),
        (Plus, '+') => Some(Concat),
        (DoublePipe, '_') => Some(DoublePipeUnderscore),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use LexicalElement::*;

    #[test]
    fn test_tokenize_basic() {
        let string = "
            a(      whitespace (
            ); test_fun(
            ; b||_
            1==%abc
            proc== %bc \n%%docs\tmore
            \r\n\r\n\r\r\r
            % % not docs\t\t\t
            cons a:b
            %\t\r\t
            % not docs\r
            0= =0
            a1= =_b_2'=
            %\t% not docs
        ";
        let tokens = &[
            Identifier(String::from("a")), OpeningParen,
            Identifier(String::from("whitespace")), OpeningParen, ClosingParen,
            Semicolon, Identifier(String::from("test_fun")), OpeningParen,
            Semicolon, Identifier(String::from("b")), DoublePipeUnderscore,
            Integer(1), DoubleEquals, Comment(String::from("abc")), Proc,
            DoubleEquals, Comment(String::from("bc ")),
            DocComment(String::from("docs\tmore")),
            Comment(String::from(" % not docs\t\t\t")), Cons,
            Identifier(String::from("a")), Colon,
            Identifier(String::from("b")), Comment(String::from("\t")),
            Comment(String::from(" not docs")), Integer(0), Equals, Equals,
            Integer(0), Identifier(String::from("a1")), Equals, Equals,
            Identifier(String::from("_b_2'")), Equals,
            Comment(String::from("\t% not docs")),
        ];
        let result = tokenize(string).unwrap();
        assert_eq!(result.len(), tokens.len());
        for i in 0 .. tokens.len() {
            assert_eq!(result[i].value, tokens[i]);
        }
    }

    #[test]
    fn test_tokenize_symbols() {
        let string = "()[]{}~!@#$^&*-=+|;:,<.>/?||&&==!==<=>=<>->=>|><|++||_";
        let tokens = &[
            OpeningParen, ClosingParen, OpeningBracket, ClosingBracket,
            OpeningBrace, ClosingBrace, Tilde, ExclamationMark, AtSign, HashSign,
            DollarSign, Circonflex, Ampersand, Asterisk, Dash, Equals, Plus, Pipe,
            Semicolon, Colon, Comma, LessThan, Period, GreaterThan, Slash,
            QuestionMark, DoublePipe, DoubleAmpersand, DoubleEquals, NotEquals, Equals,
            LessThanEquals, GreaterThanEquals, Diamond, Arrow, ThickArrow,
            ConsOperator, SnocOperator, Concat, DoublePipeUnderscore,
        ];
        let result = tokenize(string).unwrap();
        assert_eq!(result.len(), tokens.len());
        for i in 0 .. tokens.len() {
            assert_eq!(result[i].value, tokens[i]);
        }
    }

    #[test]
    fn test_tokenize_comments() {
        let string = "
            test % abc
            a%bc
            123 %% docu

                 % whitespace      \n% fiasdjfais % fasojdo\n
            % % % % %
            =
            %";
        let tokens = &[
            Identifier(String::from("test")), Comment(String::from(" abc")),
            Identifier(String::from("a")), Comment(String::from("bc")),
            Integer(123), DocComment(String::from(" docu")),
            Comment(String::from(" whitespace      ")),
            Comment(String::from(" fiasdjfais % fasojdo")),
            Comment(String::from(" % % % %")), Equals, Comment(String::new()),
        ];
        let result = tokenize(string).unwrap();
        assert_eq!(result.len(), tokens.len());
        for i in 0 .. tokens.len() {
            assert_eq!(result[i].value, tokens[i]);
        }
    }
}
