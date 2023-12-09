
use crate::ast::formula::{StateFormula, StateFormulaEnum};
use crate::core::error::Mcrl2Error;
use crate::core::syntax::Identifier;
use crate::lts::lts::Lts;

use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::rc::Rc;

/// An error in the semantics of the formula.
/// 
/// Examples are a bound variable being reused, a variable not being bound
/// anywhere, or a non-monotonic formula being written inside of a fixed point
/// operator.
pub struct FormulaCheckError {
    pub message: String,
}

impl Into<Mcrl2Error> for FormulaCheckError {
    fn into(self) -> Mcrl2Error {
        Mcrl2Error::FormulaError {
            message: self.message,
        }
    }
}

/// Checks if a labelled transtion system (LTS) satisfies the property
/// specified in a given mu-calculus state formula.
/// 
/// Equivalently, this means that the initial state is in the set of states
/// that satisfy `formula`.
/// 
/// It does this by recursively calculating the set of states for each subformula
/// of `formula`.
pub fn check_formula(lts: &Lts, formula: &StateFormula) -> Result<bool, FormulaCheckError> {
    use StateFormulaEnum::*;

    Ok(match &formula.value {
        True => true,
        False => false,
        Id { id } => {
            return Err(FormulaCheckError {
                message: format!("Unbound variable {}", id.get_value()),
            });
        },
        Implies { lhs, rhs } => {
            !(check_formula(lts, &*lhs)?) || check_formula(lts, &*rhs)?
        },
        Or { lhs, rhs } => {
            check_formula(lts, &*lhs)? || check_formula(lts, &*rhs)?
        },
        And { lhs, rhs } => {
            check_formula(lts, &*lhs)? && check_formula(lts, &*rhs)?
        },
        Not { value } => {
            !check_formula(lts, &*value)?
        },
        // TODO Box and Diamond
        _ => {
            let set = calculate_set(lts, formula)?;
            set.contains(&lts.initial_state)
        },
    })
}

/// Calculates the set of states in an labelled transition system (LTS).
/// 
/// # Returns
/// If successful, a ref-counted hash set of states, where a state is encoded
/// using an index in `lts.nodes`. Otherwise, an error describing why the given
/// formula was specified incorrectly.
pub fn calculate_set(
    lts: &Lts,
    formula: &StateFormula,
) -> Result<Rc<HashSet<usize>>, FormulaCheckError> {
    let mut state_set = HashSet::new();
    for i in 0 .. lts.nodes.len() {
        state_set.insert(i);
    }
    let mut environment = HashMap::new();
    calculate_set_impl(lts, formula, Rc::new(state_set), &mut environment)
}

fn calculate_set_impl(
    lts: &Lts,
    formula: &StateFormula,
    full_set: Rc<HashSet<usize>>,
    environment: &mut HashMap<Identifier, Rc<HashSet<usize>>>,
) -> Result<Rc<HashSet<usize>>, FormulaCheckError> {
    use StateFormulaEnum::*;

    Ok(match &formula.value {
        True => full_set,
        False => Rc::new(HashSet::new()),
        Id { id } => {
            if let Some(value) = environment.get(id) {
                value.clone()
            } else {
                return Err(FormulaCheckError { message: String::from(
                    format!("Unbound variable {}", id.get_value()),
                ) })
            }
        },
        Delay { expr: _ } => {
            unimplemented!()
        },
        Yaled { expr: _ } => {
            unimplemented!()
        },
        Mu { id, formula } => {
            calculate_fixpoint_set(
                lts, full_set, environment,
                id, Rc::new(HashSet::new()), &formula,
            )?
        },
        Nu { id, formula } => {
            calculate_fixpoint_set(
                lts, full_set.clone(), environment,
                id, full_set, &formula,
            )?
        },
        Forall { ids: _, formula: _ } => {
            unimplemented!()
        },
        Exists { ids: _, formula: _ } => {
            unimplemented!()
        },
        Implies { lhs: _, rhs: _ } => {
            unimplemented!()
        },
        Or { lhs, rhs } => {
            let l = calculate_set_impl(lts, lhs, full_set.clone(), environment)?;
            let r = calculate_set_impl(lts, rhs, full_set, environment)?;
            Rc::new(l.union(&r).map(|&x| x).collect())
        },
        And { lhs, rhs } => {
            let l = calculate_set_impl(lts, lhs, full_set.clone(), environment)?;
            let r = calculate_set_impl(lts, rhs, full_set, environment)?;
            Rc::new(l.intersection(&r).map(|&x| x).collect())
        },
        Not { value } => {
            let v = calculate_set_impl(lts, value, full_set.clone(), environment)?;

            Rc::new(full_set.difference(&v).map(|&x| x).collect())
        },
        Box { action, state_formula } => {
            let r = calculate_set_impl(lts, state_formula, full_set.clone(), environment)?;

            let mut result = HashSet::new();
            for &state in &*full_set {
                // insert `state` if for ALL outgonig transitions with label =
                // action, the resulting state satisfies `state_formula`
                let mut found = false;
                for edge in &lts.nodes[state].adj {
                    if &edge.label == action && !r.contains(&edge.target) {
                        found = true;
                        break;
                    }
                }
                if !found {
                    result.insert(state);
                }
            }

            Rc::new(result)
        },
        Diamond { action, state_formula } => {
            let r = calculate_set_impl(lts, state_formula, full_set.clone(), environment)?;

            let mut result = HashSet::new();
            for &state in &*full_set {
                // insert `state` if for ANY outgonig transitions with label =
                // action, the resulting state satisfies `state_formula`
                for edge in &lts.nodes[state].adj {
                    if &edge.label == action && r.contains(&edge.target) {
                        result.insert(state);
                        break;
                    }
                }
            }

            Rc::new(result)
        },
    })
}

fn calculate_fixpoint_set(
    lts: &Lts,
    full_set: Rc<HashSet<usize>>,
    environment: &mut HashMap<Identifier, Rc<HashSet<usize>>>,
    id: &Identifier,
    initial: Rc<HashSet<usize>>,
    state_formula: &StateFormula,
) -> Result<Rc<HashSet<usize>>, FormulaCheckError> {
    if environment.contains_key(id) {
        return Err(FormulaCheckError { message: String::from(
            format!("Bound variable {} reused", id.get_value()),
        ) });
    }

    environment.insert(id.clone(), initial);

    while {
        let original = environment.get(id).unwrap().clone();
        let new = calculate_set_impl(lts, state_formula, full_set.clone(), environment)?;
        if &*original == &*new {
            false
        } else {
            *environment.get_mut(id).unwrap() = new;
            true
        }
    } {}

    Ok(environment.remove(id).unwrap())
}
