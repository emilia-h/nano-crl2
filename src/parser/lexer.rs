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
use crate::core::syntax::SourceLocation;

use std::fmt::{Display, Formatter};
use std::str::Chars;

/// Represents an error during the tokenization process, which happens when the
/// input is invalid.
/// 
/// # Examples
/// ```
/// # use nano_crl2::parser::lexer::tokenize;
/// // a word character is not allowed after a number
/// let error = tokenize("123t").unwrap_err();
/// eprintln!("{:?}", error);
/// ```
#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub character: usize,
}

impl Into<Mcrl2Error> for LexError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::ModelError {
            message: self.message,
            line: self.line,
            character: self.character,
        }
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

    DocComment(String),

    // TODO: should be arbitrarily large
    Integer(u64),
}

impl Display for LexicalElement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        use LexicalElement::*;

        if let Some(value) = match self {
            OpeningParen => Some("("),
            ClosingParen => Some(")"),
            OpeningBracket => Some("["),
            ClosingBracket => Some("]"),
            OpeningBrace => Some("{"),
            ClosingBrace => Some("}"),
            Tilde => Some("~"),
            ExclamationMark => Some("!"),
            AtSign => Some("@"),
            HashSign => Some("#"),
            DollarSign => Some("$"),
            Circonflex => Some("^"),
            Ampersand => Some("&"),
            Asterisk => Some("*"),
            Dash => Some("-"),
            Equals => Some("="),
            Plus => Some("+"),
            Pipe => Some("|"),
            Semicolon => Some(";"),
            Colon => Some(":"),
            Comma => Some(","),
            LessThan => Some("<"),
            Period => Some("."),
            GreaterThan => Some(">"),
            Slash => Some("/"),
            QuestionMark => Some("?"),
            DoublePipe => Some("||"),
            DoubleAmpersand => Some("&&"),
            DoubleEquals => Some("=="),
            NotEquals => Some("!="),
            LessThanEquals => Some("<="),
            GreaterThanEquals => Some(">="),
            Diamond => Some("<>"),
            Arrow => Some("->"),
            ThickArrow => Some("=>"),
            ConsOperator => Some("|>"),
            SnocOperator => Some("<|"),
            Concat => Some("++"),
            DoublePipeUnderscore => Some("||_"),
            Act => Some("act"),
            Allow => Some("allow"),
            Block => Some("block"),
            Comm => Some("comm"),
            Cons => Some("cons"),
            Delay => Some("delay"),
            Div => Some("div"),
            End => Some("end"),
            Eqn => Some("eqn"),
            Exists => Some("exists"),
            Forall => Some("forall"),
            Glob => Some("glob"),
            Hide => Some("hide"),
            If => Some("if"),
            In => Some("in"),
            Init => Some("init"),
            Lambda => Some("lambda"),
            Map => Some("map"),
            Mod => Some("mod"),
            Mu => Some("mu"),
            Nu => Some("nu"),
            Pbes => Some("pbes"),
            Proc => Some("proc"),
            Rename => Some("rename"),
            Sort => Some("sort"),
            Struct => Some("struct"),
            Sum => Some("sum"),
            Val => Some("val"),
            Var => Some("var"),
            Whr => Some("whr"),
            Yaled => Some("yaled"),
            Bag => Some("Bag"),
            Bool => Some("Bool"),
            FBag => Some("FBag"),
            FSet => Some("FSet"),
            Int => Some("Int"),
            List => Some("List"),
            Nat => Some("Nat"),
            Pos => Some("Pos"),
            Real => Some("Real"),
            Set => Some("Set"),
            Delta => Some("delta"),
            False => Some("false"),
            Nil => Some("nil"),
            Tau => Some("tau"),
            True => Some("true"),
            Identifier(string) => Some(string.as_str()),
            DocComment(string) => {
                write!(f, "%{}", string)?;
                None
            },
            LexicalElement::Integer(value) => {
                write!(f, "{}", value)?;
                None
            },
        } {
            write!(f, "{}", value)?;
        }
        Ok(())
    }
}

/// Represents a single token with information about the location of the token
/// in the original string (line and character on that line).
#[derive(Clone, Eq, PartialEq)]
pub struct Token {
    pub value: LexicalElement,
    pub loc: SourceLocation,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Display::fmt(&self, f)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{} ({}, {})", self.value, self.loc.get_line(), self.loc.get_char())?;
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
        assert!(token.loc.get_line() >= curr_line);
        if token.loc.get_line() == curr_line {
            assert!(token.loc.get_char() >= curr_char);
        }

        while curr_line < token.loc.get_line() {
            result.push('\n');
            curr_line += 1;
            curr_char = 0;
        }
        while curr_char < token.loc.get_char() {
            result.push(' ');
            curr_char += 1;
        }

        let string = format!("{}", token.value);
        result.push_str(&string);
        curr_char += string.len();
    }

    result
}

struct Lexer<'a> {
    tokens: Vec<Token>,
    curr_line: usize,
    curr_char: usize,
    token_line: usize,
    token_char: usize,
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
                if next == '\r' {
                    // skip this cursed character
                    self.advance_char(next);
                    continue;
                }

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
                            line: 0,
                            character: 0,
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
                        self.advance_char(ch);
                        if ch == '\r' || ch == '\n' {
                            break;
                        }

                        if is_doc_comment {
                            comment.push(ch);
                        }
                    }

                    if is_doc_comment {
                        self.push_token(LexicalElement::DocComment(comment));
                    } else {
                        self.skip_whitespace();
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
                                character: 0,
                                line: 0,
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
        self.tokens.push(Token {
            value: element,
            loc: SourceLocation::new(self.token_line, self.token_char),
        });
        self.skip_whitespace();
        self.token_line = self.curr_line;
        self.token_char = self.curr_char;
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
            );
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
            Semicolon, Semicolon, Identifier(String::from("b")),
            DoublePipeUnderscore, Integer(1), DoubleEquals, Proc, DoubleEquals,
            DocComment(String::from("docs\tmore")), Cons,
            Identifier(String::from("a")), Colon,
            Identifier(String::from("b")),
            Integer(0), Equals, Equals, Integer(0),
            Identifier(String::from("a1")), Equals, Equals,
            Identifier(String::from("_b_2'")), Equals,
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

                 % whitespace      
            % fiasdjfais\n% fasojdo
            % % % % %
            =
            %";
        let tokens = &[
            Identifier(String::from("test")), Identifier(String::from("a")),
            Integer(123), DocComment(String::from(" docu")), Equals,
        ];
        let result = tokenize(string).unwrap();
        assert_eq!(result.len(), tokens.len());
        for i in 0 .. tokens.len() {
            assert_eq!(result[i].value, tokens[i]);
        }
    }
}
