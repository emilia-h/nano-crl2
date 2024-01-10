//! Defines structures for parity games (PGs).

use crate::core::error::Mcrl2Error;

use std::fmt::{Display, Formatter};

/// An error while parsing a parity game in some format.
#[derive(Debug)]
pub struct PgParseError {
    pub message: String,
    pub line: usize,
    pub character: usize,
}

impl Into<Mcrl2Error> for PgParseError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::PgSyntaxError {
            message: self.message,
            line: self.line,
            character: self.character,
        }
    }
}

/// A parity game with two players, "even" and "odd".
/// 
/// It is assumed in most functions that this represents a min-priority game.
/// 
/// A node in the parity game is won by a player "even" iff no matter what
/// strategy is taken by their opponent, the minimum of the priorities of
/// infinitely often occuring nodes on the play is even. Otherwise, player
/// "odd" wins.
/// 
/// # Invariant
/// For each node, its priority must lie in the interval `[min_priority,
/// max_priority]`.
#[derive(Debug)]
pub struct Pg {
    pub nodes: Vec<PgNode>,
    pub min_priority: u32,
    pub max_priority: u32,
}

impl Pg {
    /// Normalizes the game to have a min priority of either 0 or 1 and
    /// reducing all priorities to be in this new interval, while still
    /// maintaining the original solution and winning strategies.
    pub fn normalize(&mut self) {
        let redundant = 2 * (self.min_priority / 2);
        for node in &mut self.nodes {
            assert!(node.priority >= self.min_priority);
            node.priority -= redundant;
        }
        self.min_priority -= redundant;
        self.max_priority -= redundant;
    }
}

/// A node (vertex) in the parity game.
#[derive(Clone, Debug)]
pub struct PgNode {
    /// The set of moves that can be made by the owning player.
    pub adj: Vec<PgEdge>,

    /// How important this node is considered for the result of a play, if it
    /// occurs infinitely often in the play. In a min-parity game, the lower the
    /// priority, only the minimum priority counts, meaning that lower priority
    /// values are considered more important.
    pub priority: u32,

    /// Which player gets to choose the next move in a play.
    pub owner: Player,
}

/// An edge in the parity game, which is essentially a move/transition that can
/// be made by one of the players.
#[derive(Clone, Debug)]
pub struct PgEdge {
    pub target: usize,
}

/// A player in a parity game, which is always either the "odd" or the "even"
/// player.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Player {
    /// The "even" player, who wins a play iff the minimum infinitely often
    /// occuring priority is even.
    Even,
    
    /// The "odd" player, who wins a play iff the minimum infinitely often
    /// occuring priority is odd.
    Odd,
}

impl Display for Player {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Player::Even => write!(f, "even"),
            Player::Odd => write!(f, "odd"),
        }
    }
}
