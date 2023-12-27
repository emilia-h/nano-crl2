
use std::collections::hash_set::HashSet;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

/// A structure that can efficiently create and store sets of states
/// (`StateSet`s).
/// 
/// It allows for efficient set operations like union, intersection and
/// complement. It also allows for constant-time equivalence checks, assuming
/// that the two [`StateSet`]s that are compared come from the same
/// `StateSetManager`.
/// 
/// It is currently implemented using hash sets, but this will be changed to
/// [ROBDD]s (reduced ordered binary decision diagrams).
/// 
/// [`StateSet`]: ../struct.StateSet.html
/// [ROBDD]: https://en.wikipedia.org/wiki/Binary_decision_diagram
#[derive(Debug)]
pub struct StateSetManager(Rc<StateSetManagerData>);

impl StateSetManager {
    /// Creates a new shared state set with a specific maximum `state_count`.
    /// 
    /// This creates a "universal" set of which all sets (that are managed
    /// under this `StateSetManager`) must be a subset. This universal set can
    /// can be queried by `get_full_set()`.
    pub fn new(state_count: usize) -> Self {
        let mut full_set = HashSet::new();
        for i in 0 .. state_count {
            full_set.insert(i);
        }
        StateSetManager(Rc::new(StateSetManagerData {
            state_count,
            full_set: Rc::new(full_set),
            empty_set: Rc::new(HashSet::new()),
        }))
    }

    /// Returns a set object that contains all states, managed by this manager.
    pub fn get_full(&self) -> StateSet {
        StateSet {
            manager_data: Rc::clone(&self.0),
            set: Rc::clone(&self.0.full_set),
        }
    }

    /// Returns an empty set object, managed by this manager.
    pub fn get_empty(&self) -> StateSet {
        StateSet {
            manager_data: Rc::clone(&self.0),
            set: Rc::clone(&self.0.empty_set),
        }
    }

    /// Creates a set from a list of states.
    pub fn create_from_slice(&mut self, slice: &[usize]) -> StateSet {
        let set = Rc::new(HashSet::from_iter(slice.into_iter().map(|&x| x)));
        StateSet { manager_data: Rc::clone(&self.0), set }
    }
}

#[derive(Debug)]
struct StateSetManagerData {
    state_count: usize,
    full_set: Rc<HashSet<usize>>,
    empty_set: Rc<HashSet<usize>>,
}

/// One set of states, managed by a `SharedStateManager`.
pub struct StateSet {
    manager_data: Rc<StateSetManagerData>,
    set: Rc<HashSet<usize>>,
}

impl StateSet {
    /// Returns whether a state is in this set.
    /// 
    /// # Preconditions
    /// `state` must be within the range of the `StateSetManager` that this set
    /// was created by.
    pub fn contains(&self, state: usize) -> bool {
        assert!(state < self.manager_data.state_count);
        self.set.contains(&state)
    }

    /// Returns whether this set equals the empty set.
    /// 
    /// This is equivalent to saying that `self.contains(state)` is false for
    /// any state in the range of the `StateSetManager` for this set.
    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    /// Returns whether this set equals the full/"universal" set.
    /// 
    /// This is equivalent to saying that `self.contains(state)` is true for
    /// any state in the range of the `StateSetManager` for this set.
    pub fn is_full(&self) -> bool {
        self.set.len() == self.manager_data.state_count
    }

    /// Computes a set that contains this set plus the given `state`.
    /// 
    /// # Preconditions
    /// `state` must be within the range of the `StateSetManager` that this set
    /// was created by.
    pub fn insert(&self, state: usize) -> StateSet {
        assert!(state < self.manager_data.state_count);
        let mut copy = (*self.set).clone();
        copy.insert(state);
        StateSet {
            manager_data: Rc::clone(&self.manager_data),
            set: Rc::new(copy),
        }
    }

    /// Computes the logical OR i.e. the set union of two sets.
    /// 
    /// # Preconditions
    /// The managers of `self` and `rhs` must be the same or this method will
    /// panic.
    pub fn or(&self, rhs: StateSet) -> StateSet {
        assert!(Rc::ptr_eq(&self.manager_data, &rhs.manager_data));
        StateSet {
            manager_data: Rc::clone(&self.manager_data),
            set: Rc::new(self.set.union(&rhs.set).map(|&x| x).collect()),
        }
    }

    /// Computes the logical AND i.e. the set intersection of two sets.
    /// 
    /// # Preconditions
    /// The managers of `self` and `rhs` must be the same or this method will
    /// panic.
    pub fn and(&self, rhs: StateSet) -> StateSet {
        assert!(Rc::ptr_eq(&self.manager_data, &rhs.manager_data));
        StateSet {
            manager_data: Rc::clone(&self.manager_data),
            set: Rc::new(self.set.intersection(&rhs.set).map(|&x| x).collect()),
        }
    }

    /// Computes the logical NOT i.e. the set complement of a set.
    pub fn not(&self) -> StateSet {
        StateSet {
            manager_data: Rc::clone(&self.manager_data),
            set: Rc::new(self.manager_data.full_set.difference(&self.set).map(|&x| x).collect()),
        }
    }
}

impl PartialEq for StateSet {
    fn eq(&self, other: &Self) -> bool {
        return Rc::ptr_eq(&self.set, &other.set) || *self.set == *other.set;
    }
}
impl Eq for StateSet {}

impl Clone for StateSet {
    fn clone(&self) -> Self {
        StateSet {
            manager_data: Rc::clone(&self.manager_data),
            set: Rc::clone(&self.set),
        }
    }
}

impl Debug for StateSet {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.set)?;
        Ok(())
    }
}

impl<'a> IntoIterator for &'a StateSet {
    type Item = &'a usize;
    type IntoIter = std::collections::hash_set::Iter<'a, usize>;

    fn into_iter(self) -> Self::IntoIter {
        (&*self.set).into_iter()
    }
}
