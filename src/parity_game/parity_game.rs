
#[derive(Debug)]
pub struct PgParseError {
    pub message: String,
    pub line: usize,
    pub character: usize,
}

#[derive(Debug)]
pub struct Pg {
    pub nodes: Vec<PgNode>,
    pub min_priority: u32,
    pub max_priority: u32,
}

#[derive(Clone, Debug)]
pub struct PgNode {
    pub adj: Vec<PgEdge>,
    pub priority: u32,
    pub owner: Player,
}

#[derive(Clone, Debug)]
pub struct PgEdge {
    pub target: usize,
}

/// Represents a player in a parity game, which is always either the "odd" or
/// the "even" player.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Player {
    /// The "even" player, who wins a play iff the minimum infinitely often
    /// occuring priority is even.
    Even,
    
    /// The "odd" player, who wins a play iff the minimum infinitely often
    /// occuring priority is odd.
    Odd,
}
