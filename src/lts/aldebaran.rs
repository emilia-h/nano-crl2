//! Defines functions for the Aldebaran format.
//! 
//! # See also
//! The [mCRL2 user manual on this].
//! 
//! [mCRL2 user manual on this]: https://mcrl2.org/web/user_manual/tools/lts.html

use crate::core::lexer::tokenize;
use crate::core::parser::Parser;
use crate::lts::lts::{Lts, LtsEdge, LtsNode, LtsParseError};
use crate::model::proc::parse_multi_action;
use crate::util::parsing::CharParser;

/// Reads an LTS from a string in [Aldebaran format].
/// 
/// These files usually have a `.aut` extension.
/// 
/// [Aldebaran format]: https://mcrl2.org/web/user_manual/tools/lts.html#the-aut-format
pub fn parse_aldebaran_lts(input: &str) -> Result<Lts, LtsParseError> {
    let mut parser = CharParser::new(input, &|message, line, character| {
        LtsParseError { message, line, character }
    });

    // read header
    parser.expect_chars("des")?;
    parser.expect_chars("(")?;

    let initial_state = parser.parse_number()? as usize;
    parser.expect_chars(",")?;
    let edge_count = parser.parse_number()? as usize;
    parser.expect_chars(",")?;
    let node_count = parser.parse_number()? as usize;
    parser.expect_chars(")")?;

    if initial_state >= node_count {
        return Err(parser.error(
            format!("The initial state {} is not a valid index", initial_state),
        ));
    }

    // read edges
    let mut nodes = vec![LtsNode { adj: Vec::new() }; node_count];

    for _ in 0 .. edge_count {
        let (start_state, string, end_state) = parse_aldebaran_line(&mut parser)?;

        if start_state >= node_count || end_state >= node_count {
            let message = "label start/end out of bounds";
            return Err(parser.error(String::from(message)));
        }

        let tokens = match tokenize(&string) {
            Ok(tokens) => tokens,
            Err(error) => return Err(parser.error(error.message)),
        };
        let multi_action = match parse_multi_action(&mut Parser::new(&tokens)) {
            Ok(multi_action) => multi_action,
            Err(error) => return Err(parser.error(error.message)),
        };
        nodes[start_state].adj.push(LtsEdge { target: end_state, label: multi_action });
        // nodes[edge.target].trans_adj.push(LtsEdge { target: start_state, label });
    }

    parser.skip_whitespace();
    if parser.get_char().is_some() {
        return Err(parser.error(
            format!("expected {} lines but got more", edge_count),
        ));
    }

    Ok(Lts { initial_state, nodes })
}

fn parse_aldebaran_line<F>(
    parser: &mut CharParser<'_, LtsParseError, F>,
) -> Result<(usize, String, usize), LtsParseError>
where
    F: Fn(String, usize, usize) -> LtsParseError {
    parser.expect_chars("(")?;

    let start_state = parser.parse_number()?;
    parser.expect_chars(",")?;
    let string = parser.parse_string()?;
    parser.expect_chars(",")?;

    let end_state = parser.parse_number()?;

    parser.expect_chars(")")?;

    Ok((start_state as usize, string, end_state as usize))
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
        let lts = parse_aldebaran_lts("des   (0,    0,1)").unwrap();
        assert_eq!(lts.nodes.len(), 1);
        assert_eq!(lts.nodes[0].adj.len(), 0);
    }
    
    #[test]
    fn test_parse_aldeberan_too_little() {
        assert!(parse_aldebaran_lts("des (0, 5, 1)\n(0, \"a\", 0)\n").is_err());
        assert!(parse_aldebaran_lts("des(0, 1, 1)\n").is_err());
        assert!(parse_aldebaran_lts("des (0, 0, 0)\n").is_err());
    }

    #[test]
    fn test_parse_aldeberan_too_much() {
        assert!(parse_aldebaran_lts("des (0, 1, 2)\n(0, \"a\", 1)\n(1, \"a\", 0)").is_err());
        assert!(parse_aldebaran_lts("des (0, 2, 2)\n(0, \"a\", 2)\n(1, \"a\", 0)").is_err());
        assert!(parse_aldebaran_lts("des (2, 2, 2)\n(0, \"a\", 1)\n(1, \"a\", 0)").is_err());
    }
}
