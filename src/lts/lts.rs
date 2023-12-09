
use crate::ast::proc::Action;
use crate::core::error::Mcrl2Error;
use crate::core::syntax::Identifier;

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::Chars;

#[derive(Debug)]
pub struct LtsParseError {
    pub message: String,
    pub line: usize,
}

impl Into<Mcrl2Error> for LtsParseError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::LtsSyntaxError {
            message: self.message,
            line: self.line,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LtsEdge {
    pub target: usize,
    pub label: Action,
}

#[derive(Clone, Debug)]
pub struct LtsNode {
    pub adj: Vec<LtsEdge>,
    pub trans_adj: Vec<LtsEdge>,
}

#[derive(Clone, Debug)]
pub struct Lts {
    pub initial_state: usize,
    pub nodes: Vec<LtsNode>,
}

pub fn read_aldebaran_file(file: &mut File) -> Result<Lts, LtsParseError> {
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    // read header
    let header_line = match lines.next() {
        Some(Ok(l)) if l.starts_with("des") => l,
        _ => return Err(LtsParseError {
            message: String::from("AUT file does not contain a header line"),
            line: 0,
        }),
    };

    let mut chars = header_line.chars();
    skip_chars(&mut chars, "des", 0)?;
    skip_chars(&mut chars, "(", 0)?;
    let initial_state = read_number(&mut chars, 0)? as usize;
    skip_chars(&mut chars, ",", 0)?;
    let edge_count = read_number(&mut chars, 0)? as usize;
    skip_chars(&mut chars, ",", 0)?;
    let node_count = read_number(&mut chars, 0)? as usize;
    skip_chars(&mut chars, ")", 0)?;

    // read edges
    let mut nodes = vec![LtsNode { adj: vec![], trans_adj: vec![] }; node_count];

    let mut i = 0;
    for line in lines {
        let line = match line {
            Ok(l) => l,
            Err(err) => return Err(LtsParseError {
                message: err.to_string(),
                line: i,
            }),
        };

        let (start_state, edge) = read_aldebaran_line(&line, i)?;
        if start_state >= node_count || edge.target >= node_count {
            return Err(LtsParseError {
                message: String::from("label start/end out of bounds"),
                line: i,
            });
        }
        nodes[start_state].adj.push(edge.clone());
        nodes[edge.target].adj.push(LtsEdge { target: start_state, label: edge.label });

        i += 1;
    }

    if i != edge_count {
        return Err(LtsParseError {
            message: String::from(format!("expected {} lines but got {}", edge_count, i)),
            line: i as usize,
        });
    }

    Ok(Lts { initial_state, nodes })
}

fn read_aldebaran_line(line: &str, i: usize) -> Result<(usize, LtsEdge), LtsParseError> {
    let mut chars = line.chars();

    skip_chars(&mut chars, "(", i + 1)?;

    let start_state = read_number(&mut chars, i + 1)?;
    skip_chars(&mut chars, ",", i + 1)?;

    skip_chars(&mut chars, "\"", i + 1)?;
    let mut label = String::new();
    while let Some(c) = chars.next() {
        if c == '"' {
            break;
        }
        label.push(c);
    }
    skip_chars(&mut chars, ",", i + 1)?;

    let end_state = read_number(&mut chars, i + 1)?;

    skip_chars(&mut chars, ")", i + 1)?;

    Ok((
        start_state as usize,
        LtsEdge {
            target: end_state as usize,
            label: Action { id: Identifier::new(&label) },
        }
    ))
}

fn read_number(chars: &mut Chars, i: usize) -> Result<u64, LtsParseError> {
    skip_spaces(chars);

    let mut result = 0u64;
    let mut found = false;
    while let Some(c) = chars.clone().next() {
        if c as u8 >= '0' as u8 && c as u8 <= '9' as u8 {
            found = true;
            chars.next();
            result *= 10;
            result += c as u64 - '0' as u64;
        } else {
            break;
        }
    }
    if found {
        Ok(result)
    } else {
        Err(LtsParseError {
            message: String::from("expected a number"),
            line: i,
        })
    }
}

fn skip_chars(chars: &mut Chars, string: &str, line: usize) -> Result<(), LtsParseError> {
    skip_spaces(chars);

    let mut chars2 = string.chars();
    loop {
        if let Some(c2) = chars2.next() {
            if let Some(c1) = chars.next() {
                if c1 != c2 {
                    break Err(LtsParseError {
                        message: String::from(format!("expected {}", c1)),
                        line,
                    });
                } // else we are happy
            } else {
                break Err(LtsParseError {
                    message: String::from(format!("line not long enough, expected {}", string)),
                    line,
                });
            }
        } else {
            break Ok(())
        }
    }
}

fn skip_spaces(chars: &mut Chars) {
    while let Some(' ') = chars.clone().next() {
        chars.next();
    }
}

#[test]
fn test_parse_aldeberan() {
    let mut file = File::open("./test.aut").unwrap();
    let _lts = read_aldebaran_file(&mut file).unwrap();

    // TODO test contents
    // eprintln!("{:#?}", lts);
    // panic!();
}
