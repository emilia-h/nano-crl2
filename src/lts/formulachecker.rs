
use crate::ast::formula::{StateFormula, StateFormulaEnum};
use crate::core::error::Mcrl2Error;
use crate::core::state_set::{StateSetManager, StateSet};
use crate::core::syntax::Identifier;
use crate::lts::lts::Lts;

use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;

/// An error in the semantics of the formula.
/// 
/// Examples are a bound variable being reused, a variable not being bound
/// anywhere, or a non-monotonic formula being written inside of a fixed point
/// operator.
#[derive(Debug)]
pub struct FormulaError {
    pub message: String,
}

impl Into<Mcrl2Error> for FormulaError {
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
pub fn check_formula(lts: &Lts, formula: &StateFormula) -> Result<bool, FormulaError> {
    let mut state_set_manager = StateSetManager::new(lts.nodes.len());
    check_formula_impl(lts, formula, &mut state_set_manager)
}

fn check_formula_impl(
    lts: &Lts,
    formula: &StateFormula,
    state_set_manager: &mut StateSetManager,
) -> Result<bool, FormulaError> {
    use StateFormulaEnum::*;

    Ok(match &formula.value {
        True => true,
        False => false,
        Id { id } => {
            return Err(FormulaError {
                message: format!("Unbound variable {}", id.get_value()),
            });
        },
        Implies { lhs, rhs } => {
            !(check_formula_impl(lts, &*lhs, state_set_manager)?) ||
            check_formula_impl(lts, &*rhs, state_set_manager)?
        },
        Or { lhs, rhs } => {
            check_formula_impl(lts, &*lhs, state_set_manager)? ||
            check_formula_impl(lts, &*rhs, state_set_manager)?
        },
        And { lhs, rhs } => {
            check_formula_impl(lts, &*lhs, state_set_manager)? &&
            check_formula_impl(lts, &*rhs, state_set_manager)?
        },
        Not { value } => {
            !check_formula_impl(lts, &*value, state_set_manager)?
        },
        // TODO Box and Diamond
        _ => {
            calculate_set(lts, formula, state_set_manager)?.contains(lts.initial_state)
        },
    })
}

/// Calculates the set of states in an labelled transition system (LTS).
/// 
/// This implements the Emerson-Lei algorithm.
/// 
/// # Returns
/// If successful, a ref-counted hash set of states, where a state is encoded
/// using an index in `lts.nodes`. Otherwise, an error describing why the given
/// formula was specified incorrectly.
pub fn calculate_set(
    lts: &Lts,
    formula: &StateFormula,
    state_set_manager: &mut StateSetManager,
) -> Result<StateSet, FormulaError> {
    let mut variable_map = create_variable_map(formula, state_set_manager)?;
    Ok(calculate_set_impl(
        lts, formula, FixedPointParity::Closed,
        state_set_manager, &mut variable_map,
    ))
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum FixedPointParity {
    Mu,
    Nu,
    Closed,
}

impl FixedPointParity {
    pub fn negate(&self) -> FixedPointParity {
        match self {
            Self::Mu => Self::Nu,
            Self::Nu => Self::Mu,
            Self::Closed => Self::Closed,
        }
    }
}

fn calculate_set_impl(
    lts: &Lts,
    f: &StateFormula,
    enclosing_parity: FixedPointParity,
    state_set_manager: &mut StateSetManager,
    variable_map: &mut HashMap<Identifier, StateSet>,
) -> StateSet {
    use StateFormulaEnum::*;

    match &f.value {
        True => state_set_manager.get_full(),
        False => state_set_manager.get_empty(),
        Id { id } => {
            let value = variable_map.get(id)
                .expect("Error should have been found already");
            value.clone()
        },
        Delay { expr: _ } => {
            unimplemented!()
        },
        Yaled { expr: _ } => {
            unimplemented!()
        },
        Mu { id, formula } => {
            calculate_fixpoint_set(
                lts, enclosing_parity,
                state_set_manager, variable_map,
                FixedPointParity::Mu, id, &formula,
            )
        },
        Nu { id, formula } => {
            calculate_fixpoint_set(
                lts, enclosing_parity,
                state_set_manager, variable_map,
                FixedPointParity::Nu, id, &formula,
            )
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
            let l = calculate_set_impl(lts, lhs, enclosing_parity, state_set_manager, variable_map);
            let r = calculate_set_impl(lts, rhs, enclosing_parity, state_set_manager, variable_map);
            l.or(r)
        },
        And { lhs, rhs } => {
            let l = calculate_set_impl(lts, lhs, enclosing_parity, state_set_manager, variable_map);
            let r = calculate_set_impl(lts, rhs, enclosing_parity, state_set_manager, variable_map);
            l.and(r)
        },
        Not { value } => {
            calculate_set_impl(lts, value, enclosing_parity, state_set_manager, variable_map).not()
        },
        Box { action, formula } => {
            let r = calculate_set_impl(lts, formula, enclosing_parity, state_set_manager, variable_map);

            let mut result = state_set_manager.get_empty();
            for &state in &state_set_manager.get_full() {
                // insert `state` if for ALL outgonig transitions with label =
                // action, the resulting state satisfies `state_formula`
                let mut found = false;
                for edge in &lts.nodes[state].adj {
                    if &edge.label == action && !r.contains(edge.target) {
                        found = true;
                        break;
                    }
                }
                if !found {
                    result = result.insert(state);
                }
            }

            result
        },
        Diamond { action, formula } => {
            let r = calculate_set_impl(lts, formula, enclosing_parity, state_set_manager, variable_map);

            let mut result = state_set_manager.get_empty();
            for &state in &state_set_manager.get_full() {
                // insert `state` if for ANY outgonig transitions with label =
                // action, the resulting state satisfies `state_formula`
                for edge in &lts.nodes[state].adj {
                    if &edge.label == action && r.contains(edge.target) {
                        result = result.insert(state);
                        break;
                    }
                }
            }

            result
        },
    }
}

fn calculate_fixpoint_set(
    lts: &Lts,
    enclosing_parity: FixedPointParity,
    state_set_manager: &mut StateSetManager,
    variable_map: &mut HashMap<Identifier, StateSet>,
    fixed_point_parity: FixedPointParity,
    id: &Identifier,
    formula: &StateFormula,
) -> StateSet {
    assert_ne!(fixed_point_parity, FixedPointParity::Closed);

    if enclosing_parity.negate() == fixed_point_parity {
        // for all open (i.e. not closed) subformulas of the form "mu|nu X_k . g":
        //     variable_map[X_k] := initial set
        let unbound_variables = reset_variable_sets(formula, fixed_point_parity, state_set_manager, variable_map);

        if unbound_variables.len() > 1 {
            // also reset the variable of this formula, if it is not closed
            eprintln!("Resetting {:?}", id);
            let initial = match &fixed_point_parity {
                FixedPointParity::Mu => state_set_manager.get_empty(),
                FixedPointParity::Nu => state_set_manager.get_full(),
                FixedPointParity::Closed => unreachable!(),
            };
            *variable_map.get_mut(&id).unwrap() = initial;
        } else if let Some(unbound) = unbound_variables.iter().next() {
            // if formula is closed, then the only "unbound" variable must be
            // the variable bound by this fixed point operator
            assert_eq!(unbound, id);
        }
    }

    // fixed-point iteration scheme
    while {
        let original = variable_map.get(id).unwrap().clone();
        let new = calculate_set_impl(lts, formula, fixed_point_parity, state_set_manager, variable_map);
        if original == new {
            false
        } else {
            *variable_map.get_mut(id).unwrap() = new;
            true
        }
    } {}

    variable_map.get(id).unwrap().clone()
}

fn create_variable_map(
    formula: &StateFormula,
    state_set_manager: &mut StateSetManager,
) -> Result<HashMap<Identifier, StateSet>, FormulaError> {
    let mut result = HashMap::new();
    create_variable_map_impl(formula, state_set_manager, &mut result)?;
    Ok(result)
}

// TODO: this assumes that all variable names are unique!
// e.g. (mu X . X) && (nu X . X) would give incorrect results.
fn create_variable_map_impl(
    f: &StateFormula,
    state_set_manager: &mut StateSetManager,
    variable_map: &mut HashMap<Identifier, StateSet>,
) -> Result<(), FormulaError> {
    use StateFormulaEnum::*;

    match &f.value {
        True | False => {},
        Id { id } => {
            if !variable_map.contains_key(&id) {
                return Err(FormulaError {
                    message: format!("Unbound variable {}", id.get_value()),
                });
            }
        },
        Delay { expr: _ } | Yaled { expr: _ } => {
            unimplemented!()
        },
        Mu { id, formula } => {
            if variable_map.contains_key(&id) {
                return Err(FormulaError {
                    message: format!("Variable {} bound twice", id.get_value()),
                });
            }
            variable_map.insert(id.clone(), state_set_manager.get_empty());
            create_variable_map_impl(formula, state_set_manager, variable_map)?;
        },
        Nu { id, formula } => {
            if variable_map.contains_key(&id) {
                return Err(FormulaError {
                    message: format!("Variable {} bound twice", id.get_value()),
                });
            }
            variable_map.insert(id.clone(), state_set_manager.get_full());
            create_variable_map_impl(formula, state_set_manager, variable_map)?;
        },
        Forall { ids: _, formula } | Exists { ids: _, formula } => {
            // TODO: track ids that are introduced by this function, to check
            // that they are not reused anywhere
            create_variable_map_impl(formula, state_set_manager, variable_map)?;
        },
        Implies { lhs, rhs } | And { lhs, rhs } | Or { lhs, rhs } => {
            create_variable_map_impl(lhs, state_set_manager, variable_map)?;
            create_variable_map_impl(rhs, state_set_manager, variable_map)?;
        },
        Not { value } => {
            create_variable_map_impl(value, state_set_manager, variable_map)?;
        },
        Box { action: _, formula } | Diamond { action: _, formula } => {
            create_variable_map_impl(formula, state_set_manager, variable_map)?;
        },
    }
    Ok(())
}

// Resets the variables of all open subformulae of the form `mu|nu X_k . g`.
// 
// Returns the set of variables (by identifier) in `f` that are unbound, i.e.
// variables X_k within `f` that are not enclosed by `mu|nu X_k`.
fn reset_variable_sets(
    f: &StateFormula,
    parity: FixedPointParity,
    state_set_manager: &mut StateSetManager,
    variable_map: &mut HashMap<Identifier, StateSet>,
) -> HashSet<Identifier> {
    use StateFormulaEnum::*;

    match &f.value {
        True | False => HashSet::new(),
        Id { id } => {
            let mut result = HashSet::new();
            result.insert(id.clone());
            result
        },
        Delay { expr: _ } | Yaled { expr: _ } => {
            unimplemented!()
        },
        Mu { id, formula } => {
            let mut variables = reset_variable_sets(formula, parity, state_set_manager, variable_map);
            variables.remove(id);
            if !variables.is_empty() && parity == FixedPointParity::Nu {
                eprintln!("Resetting {} to empty", id.get_value());
                // subformula is not closed and of a different sign, so we have to reset
                *variable_map.get_mut(&id).unwrap() = state_set_manager.get_empty();
            } else {
                eprintln!("Not resetting {} to empty", id.get_value());
            }

            variables
        },
        Nu { id, formula } => {
            let mut variables = reset_variable_sets(formula, parity, state_set_manager, variable_map);
            variables.remove(id);
            if !variables.is_empty() && parity == FixedPointParity::Mu {
                eprintln!("Resetting {} to full", id.get_value());
                // subformula is not closed and of a different sign, so we have to reset
                *variable_map.get_mut(&id).unwrap() = state_set_manager.get_full();
            } else {
                eprintln!("Not resetting {} to full", id.get_value());
            }

            variables
        },
        Forall { ids: _, formula: _ } | Exists { ids: _, formula: _ } => {
            unimplemented!()
        },
        Implies { lhs, rhs } | And { lhs, rhs } | Or { lhs, rhs } => {
            let l = reset_variable_sets(lhs, parity, state_set_manager, variable_map);
            let r = reset_variable_sets(rhs, parity, state_set_manager, variable_map);
            l.union(&r).map(|x| x.clone()).collect()
        },
        Not { value } => {
            reset_variable_sets(value, parity, state_set_manager, variable_map)
        },
        Box { action: _, formula } | Diamond { action: _, formula } => {
            reset_variable_sets(formula, parity, state_set_manager, variable_map)
        },
    }
}

#[cfg(test)]
use crate::parser::lexer::tokenize;
#[cfg(test)]
use crate::parser::parser::Parser;
#[cfg(test)]
use crate::util::unwrap_result;

#[test]
fn test_create_variable_map() {
    let mut state_set_manager = StateSetManager::new(124);

    let tokens = tokenize("mu X123_456 . ([a] X123_456 && nu Y . (<b> Y || <a> true))").unwrap();
    let formula = Parser::new(&tokens).parse_state_formula().unwrap();
    let variables = unwrap_result(create_variable_map(&formula, &mut state_set_manager));
    assert_eq!(variables.len(), 2);
    assert_eq!(variables.get(&Identifier::new("X123_456")).unwrap(), &state_set_manager.get_empty());
    assert_eq!(variables.get(&Identifier::new("Y")).unwrap(), &state_set_manager.get_full());

    let tokens = tokenize("mu X . ([a] X2 && <a> true)").unwrap();
    let formula = Parser::new(&tokens).parse_state_formula().unwrap();
    assert!(create_variable_map(&formula, &mut state_set_manager).is_err());

    let tokens = tokenize("X").unwrap();
    let formula = Parser::new(&tokens).parse_state_formula().unwrap();
    assert!(create_variable_map(&formula, &mut state_set_manager).is_err());
}

#[test]
fn test_calculate_set() {
    use crate::ast::proc::Action;

    let tokens = tokenize("nu X . <a> X").unwrap();
    let has_infinite_a_loop = unwrap_result(Parser::new(&tokens).parse_state_formula());

    let tokens = tokenize("nu X . ([a] X && <a> true)").unwrap();
    let has_no_deadlock = unwrap_result(Parser::new(&tokens).parse_state_formula());

    let tokens = tokenize("<a> true").unwrap();
    let can_do_action = unwrap_result(Parser::new(&tokens).parse_state_formula());

    let lts = Lts::from_edge_list(0, 5, vec![
        (0, Action { id: Identifier::new("a") }, 1),
        (1, Action { id: Identifier::new("a") }, 2),
        (2, Action { id: Identifier::new("a") }, 3),
        (3, Action { id: Identifier::new("a") }, 0),
        (0, Action { id: Identifier::new("a") }, 4),
    ]);

    let mut state_set_manager = StateSetManager::new(lts.nodes.len());

    let set = calculate_set(&lts, &has_infinite_a_loop, &mut state_set_manager).unwrap();
    assert_eq!(set, state_set_manager.create_from_slice(&[0, 1, 2, 3]));
    // TODO {0, 1, 2, 3}

    let set = calculate_set(&lts, &has_no_deadlock, &mut state_set_manager).unwrap();
    assert!(set.is_empty());

    let set = calculate_set(&lts, &can_do_action, &mut state_set_manager).unwrap();
    // TODO {0, 1, 2, 3}
}
