//! Defines functions for the PGSolver parity game format.
//! 
//! # See also:
//! The [PGSolver github] and the [PGSolver parity game format] (chapter 3.5).
//! 
//! [PGSolver github]: https://github.com/tcsprojects/pgsolver
//! [PGSolver parity game format]: https://github.com/tcsprojects/pgsolver/blob/master/doc/pgsolver.pdf

use crate::core::syntax::SourcePos;
use crate::parity_game::parity_game::{Pg, PgEdge, PgParseError, PgNode, Player};
use crate::util::parsing::CharParser;

/// Parses a parity game in PGSolver format.
pub fn parse_pgsolver_game(input: &str) -> Result<Pg, PgParseError> {
    let mut parser = CharParser::new(input, &|message, line, character| {
        PgParseError {
            message,
            loc: SourcePos::new(line as u32, character as u32),
        }
    });

    parser.skip_whitespace();

    let max_identifier = if parser.get_char() == Some('p') {
        parser.expect_chars("parity")?;
        let id = parser.parse_number()? as usize;
        parser.expect_chars(";")?;

        Some(id)
    } else {
        None
    };

    parser.skip_whitespace();

    let placeholder_node = PgNode { adj: Vec::new(), priority: 0, owner: Player::Even };
    let mut nodes = if let Some(value) = max_identifier {
        vec![placeholder_node.clone(); value as usize + 1]
    } else {
        Vec::new()
    };

    let mut min_priority = u32::MAX;
    let mut max_priority = 0;
    while {
        // for now, ignore the name
        let (id, priority, owner, adj, _name) = parse_pgsolver_line(&mut parser)?;

        if let Some(value) = max_identifier {
            if id > value {
                return Err(parser.error(format!("Identifier must be below {}", value)));
            }
        } else if id < nodes.len() {
            nodes.resize(id + 1, placeholder_node.clone());
        }

        nodes[id] = PgNode { adj, priority, owner };
        min_priority = u32::min(min_priority, priority);
        max_priority = u32::max(max_priority, priority);

        parser.skip_whitespace();
        parser.get_char().is_some()
    } {}

    Ok(Pg { nodes, min_priority, max_priority })
}

fn parse_pgsolver_line<F>(
    parser: &mut CharParser<'_, PgParseError, F>,
) -> Result<(usize, u32, Player, Vec<PgEdge>, Option<String>), PgParseError>
where
    F: Fn(String, usize, usize) -> PgParseError,
{
    let identifier = parser.parse_number()? as usize;

    let priority = parser.parse_number()? as u32;

    let owner = parser.parse_number()?;
    let player = if owner == 0 {
        Player::Even
    } else if owner == 1 {
        Player::Odd
    } else {
        let message = "the owner of a node must be 0 or 1";
        return Err(parser.error(String::from(message)));
    };

    // read successors
    let mut adj = Vec::new();
    while {
        adj.push(PgEdge {
            target: parser.parse_number()? as usize,
        });

        parser.skip_whitespace();
        if parser.get_char() == Some(',') {
            parser.expect_chars(",").unwrap();
            true
        } else {
            false
        }
    } {}

    parser.skip_whitespace();
    let name = if parser.get_char() == Some('"') {
        Some(parser.parse_string()?)
    } else {
        None
    };

    parser.expect_chars(";")?;

    Ok((identifier, priority, player, adj, name))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_pgsolver_basic() {
        let game = parse_pgsolver_game("
            parity 4;
            1 3 0 1,3,4 \"Europe\";
            0 6 1 4,2 \"Africa\";
            4 5 1 0 \"Antarctica\";
            1 8 1 2,4,3 \"America\";
            3 6 0 4,2 \"Australia\";
            2 7 0 3,1,0,4 \"Asia\";"
        ).unwrap();

        assert_eq!(game.nodes.len(), 5);

        assert_eq!(game.nodes[0].adj.iter().map(|e| e.target).collect::<Vec<_>>(), vec![4, 2]);
        assert_eq!(game.nodes[0].priority, 6);
        assert_eq!(game.nodes[0].owner, Player::Odd);

        assert_eq!(game.nodes[1].adj.iter().map(|e| e.target).collect::<Vec<_>>(), vec![2, 4, 3]);
        assert_eq!(game.nodes[1].priority, 8);
        assert_eq!(game.nodes[1].owner, Player::Odd);

        assert_eq!(game.nodes[2].adj.iter().map(|e| e.target).collect::<Vec<_>>(), vec![3, 1, 0, 4]);
        assert_eq!(game.nodes[2].priority, 7);
        assert_eq!(game.nodes[2].owner, Player::Even);

        assert_eq!(game.nodes[3].adj.iter().map(|e| e.target).collect::<Vec<_>>(), vec![4, 2]);
        assert_eq!(game.nodes[3].priority, 6);
        assert_eq!(game.nodes[3].owner, Player::Even);

        assert_eq!(game.nodes[4].adj.iter().map(|e| e.target).collect::<Vec<_>>(), vec![0]);
        assert_eq!(game.nodes[4].priority, 5);
        assert_eq!(game.nodes[4].owner, Player::Odd);
    }

    #[test]
    fn test_parse_pgsolver_too_little() {
        assert!(parse_pgsolver_game("
            parity 4;
            1 3 0 1,3,4 \"Europe\";
            0 6 1 4,2 \"Africa\";
            4 5 1 0 \"Antarctica\";
            1 8 1 2,4,3 \"America\";
            3 6 0 4,2 \"Australia\";
            2 7 0 3,1,0,4"
        ).is_err());
        assert!(parse_pgsolver_game("
            parity 4;
            1 3 0 1,3,4 \"Europe\";
            0 6 1 4,2 \"Africa\";
            4 5 1 0 \"Antarctica\";
            1 8 1 2,4,3 \"America\";
            3 6 0 4,2 \"Australia\";
            2 7 0 \"Asia\";"
        ).is_err());
        assert!(parse_pgsolver_game("
            parity 4;
            1 3 0 1,3,4 \"Europe\";
            1 8 1 2,4,3 \"America\";
            3 6 0 4,2 \"Australia\"
            2 7 0 3,1,0,4 \"Asia\";"
        ).is_err());
    }
}
