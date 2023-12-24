
use crate::lts::lts::{Lts, LtsEdge, LtsNode, LtsParseError};
use crate::parser::lexer::tokenize;
use crate::parser::parser::Parser;

use std::str::Chars;

/// Reads an LTS from a string in [Aldebaran format].
/// 
/// These files usually have a `.aut` extension.
/// 
/// [Aldebaran format]: https://mcrl2.org/web/user_manual/tools/lts.html#the-aut-format
pub fn parse_aldebaran_lts(input: &str) -> Result<Lts, LtsParseError> {
    let mut lines = input.lines();

    // read header
    let header_line = match lines.next() {
        Some(l) if l.starts_with("des") => l,
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
    let mut nodes = vec![LtsNode { adj: vec![] }; node_count];

    let mut i = 0;
    for line in lines {
        let (start_state, string, end_state) = parse_aldebaran_line(&line, i)?;
        if start_state >= node_count || end_state >= node_count {
            return Err(LtsParseError {
                message: String::from("label start/end out of bounds"),
                line: i,
            });
        }

        let tokens = match tokenize(&string) {
            Ok(tokens) => tokens,
            Err(error) => return Err(LtsParseError {
                message: error.message,
                line: i,
            }),
        };
        let multi_action = match Parser::new(&tokens).parse_multi_action() {
            Ok(multi_action) => multi_action,
            Err(error) => return Err(LtsParseError {
                message: error.message,
                line: i,
            }),
        };
        nodes[start_state].adj.push(LtsEdge { target: end_state, label: multi_action });
        // nodes[edge.target].trans_adj.push(LtsEdge { target: start_state, label });

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

fn parse_aldebaran_line(
    line: &str,
    i: usize,
) -> Result<(usize, String, usize), LtsParseError> {
    let mut chars = line.chars();

    skip_chars(&mut chars, "(", i + 1)?;

    let start_state = read_number(&mut chars, i + 1)?;
    skip_chars(&mut chars, ",", i + 1)?;

    skip_chars(&mut chars, "\"", i + 1)?;
    let mut string = String::new();
    while let Some(c) = chars.next() {
        if c == '"' {
            break;
        }
        string.push(c);
    }
    skip_chars(&mut chars, ",", i + 1)?;

    let end_state = read_number(&mut chars, i + 1)?;

    skip_chars(&mut chars, ")", i + 1)?;

    Ok((start_state as usize, string, end_state as usize))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_aldeberan_basic() {
        let lts = parse_aldebaran_lts(
            "des (0, 4, 4)
            (0, \"a\", 1)
            (1, \"a\", 2)
            (2, \"a\", 3)
            (3, \"b\", 0)"
        ).unwrap();

        eprintln!("{:#?}", lts); // TODO
    }

    #[test]
    fn test_parse_aldeberan_empty() {
        let lts = parse_aldebaran_lts("des (0, 0, 1)").unwrap();
        assert_eq!(lts.nodes.len(), 1);
        assert_eq!(lts.nodes[0].adj.len(), 0);
    }
}
