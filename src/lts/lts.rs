//! Defines structures for labelled transition systems (LTSs).

use crate::model::proc::Action;
use crate::core::error::Mcrl2Error;

/// An error while parsing the LTS, which happens when the given string is not
/// in the correct format.
#[derive(Debug)]
pub struct LtsParseError {
    pub message: String,
    pub line: usize,
    pub character: usize,
}

impl Into<Mcrl2Error> for LtsParseError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::LtsSyntaxError {
            message: self.message,
            line: self.line,
        }
    }
}

/// An LTS (labelled transition system), which is in essence a directed graph
/// with unlabelled nodes and with edges that are labelled with an action.
/// 
/// An LTS usually represents a state space, where the labels represent steps
/// in the program from one state to another.
/// 
/// Note that this is a multigraph, so there for a pair of states there can be
/// arbitrarily many edges as long as the labels are distinct.
#[derive(Clone, Debug)]
pub struct Lts {
    pub initial_state: usize,
    pub nodes: Vec<LtsNode>,
}

/// A node (vertex) in the LTS, which represents a single state.
#[derive(Clone, Debug)]
pub struct LtsNode {
    pub adj: Vec<LtsEdge>,
    // pub trans_adj: Vec<LtsEdge>,
}

/// A directed edge from one LTS node to another.
/// 
/// An edge in an LTS has a "label", which is an [`Action`] that can have data
/// attached to it.
/// 
/// [`Action`]: ../ast/proc/struct.Proc.html
#[derive(Clone, Debug)]
pub struct LtsEdge {
    pub target: usize,
    pub label: Vec<Action>,
}

impl Lts {
    /// # Preconditions
    /// The input must be well-formed, for instance there cannot be edges that
    /// start or end at non-existent nodes.
    pub fn from_edge_list(
        initial_state: usize,
        node_count: usize,
        edge_list: Vec<(usize, Vec<Action>, usize)>,
    ) -> Self {
        let mut nodes = vec![
            LtsNode { adj: Vec::new() };
            node_count
        ];
        for (start, label, target) in edge_list {
            assert!(start < node_count);
            assert!(target < node_count);
            nodes[start].adj.push(LtsEdge { target, label });
            //nodes[edge.target].trans_adj.push(LtsEdge { target: start, label: edge.label });
        }
        Lts { initial_state, nodes }
    }
}
