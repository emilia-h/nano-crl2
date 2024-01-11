//! Implements the small progress measures algorithm for solving parity games.

use crate::parity_game::parity_game::{Pg, Player};

use rand::SeedableRng;
use rand::rngs::StdRng;
use rand::seq::SliceRandom;

use std::cmp::Ordering;
use std::collections::vec_deque::VecDeque;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IterationPolicy {
    InputOrder,
    RandomOrder {
        seed: u64,
    },
    DescendingDegreeOrder,
    ReverseBfs,
    PostOrderDfs,
}

/// Solves a parity game, treating it as a min-priority game.
/// 
/// Returns the set of nodes that are won by the given `player`.
/// 
/// This implements the small progress measures algorithm.
/// 
/// # Preconditions
/// The given parity game has to be well-formed and normalized, meaning that
/// its `min_priority` field has to be 0 or 1 and that its nodes have
/// priorities that are at most as high as `max_priority`. Additionally, each
/// node must have at least one outgoing edge.
pub fn solve_parity_game(
    game: &Pg,
    player: Player,
    policy: IterationPolicy,
) -> Vec<usize> {
    if player == Player::Odd {
        todo!();
    }

    assert!(game.min_priority <= 1);
    for node in &game.nodes {
        assert!(node.priority <= game.max_priority);
        assert!(!node.adj.is_empty());
    }

    let play_value_limit = get_play_value_limit(game);
    let mut progress_measure = vec![PlayValue::zeroes(game.max_priority); game.nodes.len()];

    match policy {
        IterationPolicy::InputOrder => {
            let order = 0 .. game.nodes.len();
            lift_ordered(game, &mut progress_measure, &play_value_limit, &order);
        },
        IterationPolicy::RandomOrder { seed } => {
            let mut rng = StdRng::seed_from_u64(seed);
            let mut order = (0 .. game.nodes.len()).collect::<Vec<_>>();
            order.shuffle(&mut rng);
            lift_ordered(game, &mut progress_measure, &play_value_limit, &order.into_iter());
        },
        IterationPolicy::DescendingDegreeOrder => {
            let mut indegree = vec![0; game.nodes.len()];
            for node in &game.nodes {
                for edge in &node.adj {
                    indegree[edge.target] += 1;
                }
            }
            let mut order = (0 .. game.nodes.len()).collect::<Vec<_>>();
            order.sort_by(|&i, &j| indegree[j].cmp(&indegree[i]));
            lift_ordered(game, &mut progress_measure, &play_value_limit, &order.into_iter());
        },
        IterationPolicy::ReverseBfs => {
            let mut order = get_distance_order(game);
            order.reverse();
            lift_ordered(game, &mut progress_measure, &play_value_limit, &order.into_iter());
        },
        IterationPolicy::PostOrderDfs => {
            // technically not really a "topological" order, as the graph is
            // not a DAG
            let order = get_reverse_topological_order(game);
            lift_ordered(game, &mut progress_measure, &play_value_limit, &order.into_iter());
        },
    }

    let mut result = Vec::new();
    for v in 0 .. game.nodes.len() {
        if !progress_measure[v].is_top() {
            result.push(v);
        }
    }
    result
}

fn lift_ordered<T>(
    game: &Pg,
    progress_measure: &mut Vec<PlayValue>,
    play_value_limit: &PlayValue,
    iterator: &T,
)
where
    T: Clone + Iterator<Item = usize>
{
    let mut found = true;
    while found {
        found = false;
        for v in iterator.clone() {
            while lift(game, progress_measure, &play_value_limit, v) {
                found = true;
            }
        }
    }
}

fn get_play_value_limit(game: &Pg) -> PlayValue {
    let mut result = PlayValue::zeroes(game.max_priority);
    for node in &game.nodes {
        if node.priority % 2 != 0 {
            result.set(node.priority, result.get(node.priority).unwrap() + 1);
        }
    }
    result
}

// returns whether the original progress measure was changed.
fn lift(
    game: &Pg,
    progress_measure: &mut Vec<PlayValue>,
    play_value_limit: &PlayValue,
    v: usize,
) -> bool {
    if progress_measure[v].is_top() {
        return false;
    }

    let node = &game.nodes[v];
    let v_priority = node.priority;
    match node.owner {
        Player::Even => {
            let mut min_prog = PlayValue::top();
            for edge in &node.adj {
                let new_play_value = prog(
                    game.max_priority, play_value_limit,
                    v_priority, &progress_measure[edge.target],
                );
                if new_play_value.partial_cmp(&min_prog).unwrap() == Ordering::Less {
                    min_prog = new_play_value;
                }
            }

            if min_prog > progress_measure[v] {
                progress_measure[v] = min_prog;
                true
            } else {
                false
            }
        },
        Player::Odd => {
            let mut max_prog = progress_measure[v].clone();
            for edge in &node.adj {
                let new_play_value = prog(
                    game.max_priority, play_value_limit,
                    v_priority, &progress_measure[edge.target],
                );
                if new_play_value.partial_cmp(&max_prog).unwrap() == Ordering::Greater {
                    max_prog = new_play_value;
                }
            }

            if max_prog > progress_measure[v] {
                progress_measure[v] = max_prog;
                true
            } else {
                false
            }
        },
    }
}

fn prog(
    max_priority: u32,
    play_value_limit: &PlayValue,
    v_priority: u32,
    w_play_value: &PlayValue,
) -> PlayValue {
    if w_play_value.is_top() {
        PlayValue::top()
    } else if v_priority % 2 == 0 {
        // find least play value `m` such that `m >=_{v.priority} progress_measure[w]`
        let mut m = w_play_value.clone();
        let mut i = v_priority + 1;
        while i <= max_priority {
            m.set(i, 0);
            i += 2;
        }
        assert_ne!(m.compare_up_to(w_play_value, v_priority), Ordering::Less);
        m
    } else {
        // find least play value `m` such that
        // `m >_{v.priority} progress_measure[w]`;
        // do this by finding the last index in [0, v.priority] such that the
        // element at that index can still be increased and set everything
        // after that to 0; if there is no such index, return "top"
        let mut m = w_play_value.clone();
        let mut i = v_priority as i64;
        while i >= 1 {
            let m_element = m.get(i as u32).unwrap();
            let limit_element = play_value_limit.get(i as u32).unwrap();
            assert!(m_element <= limit_element);

            if m_element < limit_element {
                m.set(i as u32, m_element + 1);

                let mut j = i as u32 + 2;
                while j <= max_priority {
                    m.set(j, 0);
                    j += 2;
                }

                assert_eq!(m.compare_up_to(w_play_value, v_priority), Ordering::Greater);
                return m;
            }
            i -= 2;
        }
        PlayValue::top()
    }
}

#[derive(Clone, Eq, PartialEq)]
struct PlayValue {
    elements: Option<Vec<usize>>,
}

impl PlayValue {
    pub fn top() -> Self {
        PlayValue { elements: None }
    }

    /// Creates a play value with a value for each priority in the interval
    /// `[0, max_priority]`, i.e. of length `max_priority + 1`.
    /// 
    /// Since each element corresponding to an even index (where an index is a
    /// priority) is always going to be 0, internally this does not allocate
    /// unnecessary storage for those 0 values.
    pub fn zeroes(max_priority: u32) -> Self {
        PlayValue { elements: Some(vec![0; (max_priority + 1) as usize / 2]) }
    }

    /// Creates a play value from a slice.
    /// 
    /// # Preconditions
    /// Each element at an even index must equal 0.
    #[allow(unused)]
    pub fn from_slice(elements: &[usize]) -> Self {
        let mut result = vec![0; elements.len() / 2];
        for i in 0 .. elements.len() {
            if i % 2 == 0 {
                assert_eq!(elements[i], 0);
            } else {
                result[i / 2] = elements[i];
            }
        }
        PlayValue { elements: Some(result) }
    }

    /// Compares two play values lexicographically for only the first `limit`
    /// elements.
    /// 
    /// # Preconditions
    /// If either play value is not "top", then `limit` must be at most the
    /// `max_priority` that was given in the constructor.
    /// 
    /// If both play values aree not "top", then they should both have the
    /// same number of elements.
    pub fn compare_up_to(&self, other: &Self, limit: u32) -> Ordering {
        if let (Some(a), Some(b)) = (&self.elements, &other.elements) {
            assert!(limit as usize <= 2 * a.len() && limit as usize <= 2 * b.len());

            // compares odd indices only, because the even indices are always 0
            for i in 0 .. (limit as usize + 1) / 2 {
                if a[i] < b[i] {
                    return Ordering::Less;
                } else if a[i] > b[i] {
                    return Ordering::Greater;
                }
            }
            Ordering::Equal
        } else {
            self.elements.is_some().cmp(&other.elements.is_some())
        }
    }

    /// Returns whether or not this 
    pub fn is_top(&self) -> bool {
        self.elements.is_none()
    }

    /// Returns the `i`th element of this play value, or `None` if this is a
    /// "top" play value.
    /// 
    /// # Preconditions
    /// if the play value is not "top", then `priority` must be at most the
    /// `max_priority` that was given in the constructor.
    pub fn get(&self, priority: u32) -> Option<usize> {
        self.elements.as_ref().map(|v| {
            if priority % 2 == 0 {
                0
            } else {
                v[priority as usize / 2]
            }
        })
    }

    /// Sets the `i`th element of this play value.
    /// 
    /// # Preconditions
    /// This must not be a "top" play value.
    /// 
    /// If `priority` is even, then `value` must be 0.
    pub fn set(&mut self, priority: u32, value: usize) {
        assert!(self.elements.is_some());
        if priority % 2 == 0 {
            assert_eq!(value, 0);
        } else {
            self.elements.as_mut().unwrap()[priority as usize / 2] = value;
        }
    }
}

impl Debug for PlayValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        Display::fmt(self, f)
    }
}

impl Display for PlayValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        if let Some(value) = &self.elements {
            write!(f, "(")?;
            for (i, v) in value.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "0, {}", v)?;
            }
            write!(f, ")")?;
        } else {
            write!(f, "top")?;
        }
        Ok(())
    }
}

impl PartialOrd for PlayValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self.elements, &other.elements) {
            (Some(a), Some(b)) => {
                if a.len() != b.len() {
                    return None;
                }
                for i in 0 .. a.len() {
                    if a[i] < b[i] {
                        return Some(Ordering::Less);
                    } else if a[i] > b[i] {
                        return Some(Ordering::Greater);
                    }
                }
                Some(Ordering::Equal)
            },
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),
            (None, None) => Some(Ordering::Equal),
        }
    }
}

// bfs
fn get_distance_order(game: &Pg) -> Vec<usize> {
    let mut result = Vec::new();
    let mut visited = vec![false; game.nodes.len()];
    let mut queue = VecDeque::new();
    for i in 0 .. game.nodes.len() {
        if visited[i] {
            continue;
        }

        visited[i] = true;

        assert!(queue.is_empty());
        queue.push_back(i);
        while let Some(next) = queue.pop_front() {
            for edge in &game.nodes[next].adj {
                if !visited[edge.target] {
                    visited[edge.target] = true;
                    queue.push_back(edge.target);
                }
            }

            result.push(next);
        }
    }

    // result should be a permutation of `0 .. game.nodes.len()`
    assert_eq!(result.len(), game.nodes.len());
    result
}

// dfs
fn get_reverse_topological_order(game: &Pg) -> Vec<usize> {
    // https://stackoverflow.com/questions/20153488/topological-sort-using-dfs-without-recursion
    let mut result = Vec::new();
    let mut visited = vec![false; game.nodes.len()];
    let mut stack = Vec::new();

    for i in 0 .. game.nodes.len() {
        if visited[i] {
            continue;
        }

        stack.push((i, false));
        while let Some((next, should_add)) = stack.pop() {
            if should_add {
                result.push(next);
                continue;
            }
            if visited[next] {
                continue;
            }

            visited[next] = true;
            stack.push((next, true));
            for edge in &game.nodes[next].adj {
                if !visited[edge.target] {
                    stack.push((edge.target, false));
                }
            }
        }
    }

    // result should be a permutation of `0 .. game.nodes.len()`
    assert_eq!(result.len(), game.nodes.len());
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parity_game::pgsolver::parse_pgsolver_game;

    #[test]
    fn test_prog_basic() {
        // domain M = {0} x {0, 1, 2} x {0} x {0, 1, 2}
        let limit = PlayValue::from_slice(&[0, 2, 0, 2]);

        assert_eq!(
            prog(3, &limit, 0, &PlayValue::from_slice(&[0, 2, 0, 0])),
            PlayValue::from_slice(&[0, 0, 0, 0]),
        );
        assert_eq!(
            prog(3, &limit, 0, &PlayValue::from_slice(&[0, 2, 0, 2])),
            PlayValue::from_slice(&[0, 0, 0, 0]),
        );
        assert_eq!(
            prog(3, &limit, 1, &PlayValue::from_slice(&[0, 2, 0, 0])),
            PlayValue::top(),
        );
        assert_eq!(
            prog(3, &limit, 3, &PlayValue::from_slice(&[0, 2, 0, 0])),
            PlayValue::from_slice(&[0, 2, 0, 1]),
        );

        let limit = PlayValue::from_slice(&[0, 2, 0, 3, 0]);
        assert_eq!(
            prog(4, &limit, 0, &PlayValue::from_slice(&[0, 2, 0, 3, 0])),
            PlayValue::from_slice(&[0, 0, 0, 0, 0]),
        );
        assert_eq!(
            prog(4, &limit, 2, &PlayValue::from_slice(&[0, 2, 0, 3, 0])),
            PlayValue::from_slice(&[0, 2, 0, 0, 0]),
        );
        assert_eq!(
            prog(4, &limit, 4, &PlayValue::from_slice(&[0, 2, 0, 3, 0])),
            PlayValue::from_slice(&[0, 2, 0, 3, 0]),
        );

        let limit = PlayValue::from_slice(&[0, 1, 0, 1, 0, 1, 0, 2]);
        assert_eq!(
            prog(7, &limit, 7, &PlayValue::from_slice(&[0, 0, 0, 1, 0, 1, 0, 2])),
            PlayValue::from_slice(&[0, 1, 0, 0, 0, 0, 0, 0]),
        );
        assert_eq!(
            prog(7, &limit, 5, &PlayValue::from_slice(&[0, 0, 0, 1, 0, 1, 0, 2])),
            PlayValue::from_slice(&[0, 1, 0, 0, 0, 0, 0, 0]),
        );
        assert_eq!(
            prog(7, &limit, 7, &PlayValue::from_slice(&[0, 0, 0, 1, 0, 1, 0, 1])),
            PlayValue::from_slice(&[0, 0, 0, 1, 0, 1, 0, 2]),
        );
        assert_eq!(
            prog(7, &limit, 5, &PlayValue::from_slice(&[0, 0, 0, 1, 0, 1, 0, 1])),
            PlayValue::from_slice(&[0, 1, 0, 0, 0, 0, 0, 0]),
        );
        assert_eq!(
            prog(7, &limit, 7, &PlayValue::from_slice(&[0, 1, 0, 1, 0, 1, 0, 1])),
            PlayValue::from_slice(&[0, 1, 0, 1, 0, 1, 0, 2]),
        );
        assert_eq!(
            prog(7, &limit, 5, &PlayValue::from_slice(&[0, 1, 0, 1, 0, 1, 0, 1])),
            PlayValue::top(),
        );
        assert_eq!(
            prog(7, &limit, 3, &PlayValue::from_slice(&[0, 1, 0, 1, 0, 1, 0, 1])),
            PlayValue::top(),
        );
        assert_eq!(
            prog(7, &limit, 1, &PlayValue::from_slice(&[0, 1, 0, 1, 0, 1, 0, 1])),
            PlayValue::top(),
        );
    }

    fn check_each_policy(game: &Pg, player: Player) -> Vec<usize> {
        use IterationPolicy::*;

        let result = solve_parity_game(game, player, IterationPolicy::InputOrder);
        let policies = [
            RandomOrder { seed: rand::random::<u64>() },
            DescendingDegreeOrder,
            ReverseBfs,
            PostOrderDfs,
        ];
        for policy in policies {
            assert_eq!(solve_parity_game(game, player, policy), result);
        }
        result
    }

    #[test]
    fn test_solve_parity_game_all_top() {
        let game = parse_pgsolver_game(
            "parity 6;
            0 1 1 0,2;
            1 2 0 0,3;
            2 1 0 3,5;
            3 2 1 1,6;
            4 3 0 4;
            5 3 0 4;
            6 3 0 5,6;",
        ).unwrap();

        let limit = get_play_value_limit(&game);
        assert_eq!(limit, PlayValue::from_slice(&[0, 2, 0, 3]));

        let won_by_even = check_each_policy(&game, Player::Even);
        assert!(won_by_even.is_empty());
    }
}
