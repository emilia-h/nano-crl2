//! Implements the Emerson-Lei algorithm for testing labelled transition
//! systems (LTS) for properties, which are specified in mu-calculus formulas.

use crate::analysis::action_formula::matches_action_formula;
use crate::core::diagnostic::{Diagnostic, DiagnosticSeverity};
use crate::core::state_set::{StateSetManager, StateSet};
use crate::core::syntax::{Identifier, SourceRange};
use crate::lts::lts::Lts;
use crate::mu_calculus::action_formula::ActionFormula;
use crate::mu_calculus::regular_formula::RegularFormulaEnum;
use crate::mu_calculus::state_formula::{StateFormula, StateFormulaEnum};

use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::sync::Arc;

/// An error in the semantics of the formula.
/// 
/// Examples are a bound variable being reused, a variable not being bound
/// anywhere, or a non-monotonic formula being written inside of a fixpoint
/// operator.
#[derive(Debug)]
pub struct FormulaError {
    pub message: String,
    pub loc: Option<SourceRange>, // TODO always add loc information
}

impl FormulaError {
    pub fn into_diagnostic(self, file: Option<String>) -> Diagnostic {
        Diagnostic {
            severity: DiagnosticSeverity::Error,
            file,
            loc: self.loc,
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
pub fn verify_lts(lts: &Lts, formula: &StateFormula) -> Result<bool, FormulaError> {
    let mut state_set_manager = StateSetManager::new(lts.nodes.len());
    let satisfying_set = calculate_set(lts, formula, &mut state_set_manager)?;
    Ok(satisfying_set.contains(lts.initial_state))
}

/// Calculates the set of states in a labelled transition system (LTS) that
/// satisfy a mu-calculus formula.
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
    let formula_system = rewrite_state_formula(formula)?;

    let mut state_set_map = vec![None; formula_system.equations.len()];
    let result = calculate_set_impl(
        lts, &formula_system, formula_system.initial,
        state_set_manager, &mut state_set_map,
    )?;
    Ok(result)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum FixpointParity {
    Mu,
    Nu,
    None,
}

impl FixpointParity {
    pub fn negate(&self) -> FixpointParity {
        match self {
            Self::Mu => Self::Nu,
            Self::Nu => Self::Mu,
            Self::None => Self::None,
        }
    }
}

#[derive(Debug)]
struct IrStateFormulaSystem {
    pub initial: usize,
    // (the equation, whether the formula is closed)
    pub equations: Vec<(IrStateFormulaEquation, bool)>,
}

#[derive(Debug)]
enum IrStateFormulaEquation {
    True,
    False,
    Variable {
        fixpoint_index: usize,
    },
    Mu {
        sub_index: usize,
        reset_variables: Vec<usize>,
    },
    Nu {
        sub_index: usize,
        reset_variables: Vec<usize>,
    },
    Implies {
        lhs: usize,
        rhs: usize,
    },
    Or {
        lhs: usize,
        rhs: usize,
    },
    And {
        lhs: usize,
        rhs: usize,
    },
    Not {
        sub_index: usize,
    },
    Box {
        action_formula: Arc<ActionFormula>,
        sub_index: usize,
    },
    Diamond {
        action_formula: Arc<ActionFormula>,
        sub_index: usize,
    },
}

impl IrStateFormulaEquation {
    pub fn get_fixpoint_parity(&self) -> FixpointParity {
        match self {
            IrStateFormulaEquation::Mu { .. } => FixpointParity::Mu,
            IrStateFormulaEquation::Nu { .. } => FixpointParity::Nu,
            _ => FixpointParity::None,
        }
    }
}

fn calculate_set_impl(
    lts: &Lts,
    formula_system: &IrStateFormulaSystem,
    index: usize,
    state_set_manager: &mut StateSetManager,
    formula_map: &mut Vec<Option<StateSet>>,
) -> Result<StateSet, FormulaError> {
    use IrStateFormulaEquation::*;

    let (equation, is_closed) = &formula_system.equations[index];

    if *is_closed {
        // can safely reuse cached value
        if let Some(state_set) = &formula_map[index] {
            return Ok(state_set.clone())
        }
    }

    let result = match equation {
        True => state_set_manager.get_full(),
        False => state_set_manager.get_empty(),
        Variable { fixpoint_index } => {
            formula_map[*fixpoint_index].clone().unwrap()
        },
        Mu { sub_index, reset_variables } => {
            calculate_fixpoint_set(
                lts, formula_system,
                index, FixpointParity::Mu, *sub_index, reset_variables,
                state_set_manager, formula_map,
            )?
        },
        Nu { sub_index, reset_variables } => {
            calculate_fixpoint_set(
                lts, formula_system,
                index, FixpointParity::Nu, *sub_index, reset_variables,
                state_set_manager, formula_map,
            )?
        },
        Implies { lhs, rhs } => {
            let l = calculate_set_impl(
                lts, formula_system, *lhs,
                state_set_manager, formula_map,
            )?;
            let r = calculate_set_impl(
                lts, formula_system, *rhs,
                state_set_manager, formula_map,
            )?;
            l.not().or(r)
        },
        Or { lhs, rhs } => {
            let l = calculate_set_impl(
                lts, formula_system, *lhs,
                state_set_manager, formula_map,
            )?;
            let r = calculate_set_impl(
                lts, formula_system, *rhs,
                state_set_manager, formula_map,
            )?;
            l.or(r)
        },
        And { lhs, rhs } => {
            let l = calculate_set_impl(
                lts, formula_system, *lhs,
                state_set_manager, formula_map,
            )?;
            let r = calculate_set_impl(
                lts, formula_system, *rhs,
                state_set_manager, formula_map,
            )?;
            l.and(r)
        },
        Not { sub_index } => {
            let v = calculate_set_impl(
                lts, formula_system, *sub_index,
                state_set_manager, formula_map,
            )?;
            v.not()
        },
        Box { action_formula, sub_index } => {
            let r = calculate_set_impl(
                lts, formula_system, *sub_index,
                state_set_manager, formula_map,
            )?;

            let mut result = Vec::new();
            for &state in &state_set_manager.get_full() {
                // insert `state` iff for ALL outgoing transitions with label =
                // action, the resulting state satisfies `state_formula`
                let mut found = false;
                for edge in &lts.nodes[state].adj {
                    if !r.contains(edge.target) && matches_action_formula(&edge.label, &action_formula)? {
                        found = true;
                        break;
                    }
                }
                if !found {
                    result.push(state);
                }
            }

            state_set_manager.create_from_slice(&result)
        },
        Diamond { action_formula, sub_index } => {
            let r = calculate_set_impl(
                lts, formula_system, *sub_index,
                state_set_manager, formula_map,
            )?;

            let mut result = Vec::new();
            for &state in &state_set_manager.get_full() {
                // insert `state` iff for ANY outgoing transitions with label =
                // action, the resulting state satisfies `state_formula`
                for edge in &lts.nodes[state].adj {
                    if r.contains(edge.target) && matches_action_formula(&edge.label, &action_formula)? {
                        result.push(state);
                        break;
                    }
                }
            }

            state_set_manager.create_from_slice(&result)
        },
    };
    formula_map[index] = Some(result.clone());
    Ok(result)
}

fn calculate_fixpoint_set(
    lts: &Lts,
    formula_system: &IrStateFormulaSystem,
    index: usize,
    fixpoint_parity: FixpointParity,
    sub_index: usize,
    reset_variables: &Vec<usize>,
    state_set_manager: &mut StateSetManager,
    formula_map: &mut Vec<Option<StateSet>>,
) -> Result<StateSet, FormulaError> {
    for &reset_variable in reset_variables {
        eprintln!("Resetting {}", reset_variable);
        formula_map[reset_variable] = None;
    }

    if formula_map[index].is_none() {
        formula_map[index] = Some(match fixpoint_parity {
            FixpointParity::Mu => state_set_manager.get_empty(),
            FixpointParity::Nu => state_set_manager.get_full(),
            FixpointParity::None => unreachable!(),
        });
    }

    while {
        let new_set = calculate_set_impl(
            lts, formula_system, sub_index,
            state_set_manager, formula_map,
        )?;

        let changed = &new_set != formula_map[index].as_ref().unwrap();
        formula_map[index] = Some(new_set);
        changed
    } {}

    Ok(formula_map[index].clone().unwrap())
}

fn rewrite_state_formula(
    formula: &StateFormula,
) -> Result<IrStateFormulaSystem, FormulaError> {
    let mut id_map = HashMap::new();
    let mut equations = Vec::new();
    equations.push((IrStateFormulaEquation::True, true));
    equations.push((IrStateFormulaEquation::False, true));
    let root = rewrite_state_formula_impl(
        formula,
        &mut id_map, FixpointParity::None, &mut equations,
    )?;
    Ok(IrStateFormulaSystem { initial: root.0, equations })
}

// returns: (index of the equation corresponding to `f`, set of subequations)
fn rewrite_state_formula_impl<'a>(
    f: &'a StateFormula,
    id_map: &mut HashMap<&'a Identifier, usize>,
    enclosing_parity: FixpointParity,
    equations: &mut Vec<(IrStateFormulaEquation, bool)>,
) -> Result<(usize, HashSet<usize>, HashSet<usize>), FormulaError> {
    use StateFormulaEnum::*;

    Ok(match &f.value {
        True => (0, HashSet::new(), HashSet::new()),
        False => (1, HashSet::new(), HashSet::new()),
        Id { id } => {
            if let Some(&fixpoint_index) = id_map.get(id) {
                // an identifier on its own is never a closed formula
                equations.push((
                    IrStateFormulaEquation::Variable { fixpoint_index },
                    false,
                ));

                let mut set = HashSet::new();
                set.insert(fixpoint_index);
                (equations.len() - 1, set, HashSet::new())
            } else {
                return Err(FormulaError {
                    message: format!("Unbound variable {}", id),
                    loc: None,
                });
            }
        },
        Delay { .. } => {
            unimplemented!()
        },
        Yaled { .. } => {
            unimplemented!()
        },
        Mu { id, formula, .. } => {
            rewrite_fixpoint_formula(
                FixpointParity::Mu, id, &formula,
                id_map, enclosing_parity, equations,
            )?
        },
        Nu { id, formula, .. } => {
            rewrite_fixpoint_formula(
                FixpointParity::Nu, id, &formula,
                id_map, enclosing_parity, equations,
            )?
        },
        Forall { ids: _, formula: _ } => {
            todo!("`forall` state formula")
        },
        Exists { ids: _, formula: _ } => {
            todo!("`exists` state formula")
        },
        Implies { lhs, rhs } => {
            rewrite_binary_state_formula(
                lhs, rhs,
                |l, r| IrStateFormulaEquation::Implies { lhs: l, rhs: r },
                id_map, enclosing_parity, equations,
            )?
        },
        Or { lhs, rhs } => {
            rewrite_binary_state_formula(
                lhs, rhs,
                |l, r| IrStateFormulaEquation::Or { lhs: l, rhs: r },
                id_map, enclosing_parity, equations,
            )?
        },
        And { lhs, rhs } => {
            rewrite_binary_state_formula(
                lhs, rhs,
                |l, r| IrStateFormulaEquation::And { lhs: l, rhs: r },
                id_map, enclosing_parity, equations,
            )?
        },
        Not { value } => {
            let (index, set, fixpoint_set) = rewrite_state_formula_impl(
                value,
                id_map, enclosing_parity, equations,
            )?;

            equations.push((
                IrStateFormulaEquation::Not { sub_index: index },
                set.is_empty(),
            ));
            (equations.len() - 1, set, fixpoint_set)
        },
        Box { regular_formula, formula } => {
            let action_formula = match &regular_formula.value {
                RegularFormulaEnum::ActionFormula { value } => value,
                _ => todo!("regular formulas"),
            };

            let (sub_index, id_set, fixpoint_set) = rewrite_state_formula_impl(
                formula,
                id_map, enclosing_parity, equations,
            )?;

            equations.push((
                IrStateFormulaEquation::Box {
                    action_formula: Arc::clone(action_formula),
                    sub_index,
                },
                id_set.is_empty(),
            ));
            (equations.len() - 1, id_set, fixpoint_set)
        },
        Diamond { regular_formula, formula } => {
            let action_formula = match &regular_formula.value {
                RegularFormulaEnum::ActionFormula { value } => value,
                _ => todo!("regular formulas"),
            };

            let (sub_index, id_set, fixpoint_set) = rewrite_state_formula_impl(
                formula,
                id_map, enclosing_parity, equations,
            )?;

            equations.push((
                IrStateFormulaEquation::Diamond {
                    action_formula: Arc::clone(action_formula),
                    sub_index,
                },
                id_set.is_empty(),
            ));
            (equations.len() - 1, id_set, fixpoint_set)
        },
    })
}

fn rewrite_fixpoint_formula<'a>(
    fixpoint_parity: FixpointParity,
    id: &'a Identifier,
    formula: &'a StateFormula,
    id_map: &mut HashMap<&'a Identifier, usize>,
    enclosing_parity: FixpointParity,
    equations: &mut Vec<(IrStateFormulaEquation, bool)>,
) -> Result<(usize, HashSet<usize>, HashSet<usize>), FormulaError> {
    // index value and reset variable set of `formula` cannot be known yet, so
    // it will be set later
    let equation = match fixpoint_parity {
        FixpointParity::Mu => {
            IrStateFormulaEquation::Mu { sub_index: 0, reset_variables: Vec::new() }
        },
        FixpointParity::Nu => {
            IrStateFormulaEquation::Nu { sub_index: 0, reset_variables: Vec::new() }
        },
        FixpointParity::None => unreachable!(),
    };
    equations.push((equation, false));
    let index = equations.len() - 1;

    if id_map.contains_key(id) {
        return Err(FormulaError {
            message: format!("Rebound variable {}", id),
            loc: None,
        });
    }

    id_map.insert(id, index);
    let (sub_index, mut id_set, mut fixpoint_set) = rewrite_state_formula_impl(
        formula,
        id_map, fixpoint_parity, equations,
    )?;
    id_map.remove(id);

    id_set.remove(&index);

    let is_closed = id_set.is_empty();

    // calculate formulas that have to be reset
    let mut reset_variables = Vec::new();
    if fixpoint_parity == enclosing_parity.negate() {
        if !is_closed {
            // if this fixpoint operator is closed, then we do not ever need
            // to reset it, but if it is open, then we may have to
            reset_variables.push(index);
        }

        // for each open fixpoint operator that has the same sign as this
        // operator, also require that it is reset
        for &variable in &fixpoint_set {
            let (equation, closed) = &equations[variable];
            if equation.get_fixpoint_parity() == fixpoint_parity && !*closed {
                reset_variables.push(variable);
            }
        }
    }

    // set index now, thus creating a cycle
    match &mut equations[index] {
        (IrStateFormulaEquation::Mu { sub_index: si, reset_variables: rvs }, closed) => {
            *si = sub_index;
            *rvs = reset_variables;
            *closed = is_closed;
        },
        (IrStateFormulaEquation::Nu { sub_index: si, reset_variables: rvs }, closed) => {
            *si = sub_index;
            *rvs = reset_variables;
            *closed = is_closed;
        },
        _ => unreachable!(),
    }

    fixpoint_set.insert(index);

    Ok((index, id_set, fixpoint_set))
}

fn rewrite_binary_state_formula<'a, F>(
    lhs: &'a StateFormula,
    rhs: &'a StateFormula,
    constructor: F,
    id_map: &mut HashMap<&'a Identifier, usize>,
    enclosing_parity: FixpointParity,
    equations: &mut Vec<(IrStateFormulaEquation, bool)>,
) -> Result<(usize, HashSet<usize>, HashSet<usize>), FormulaError>
where
    F: Fn(usize, usize) -> IrStateFormulaEquation,
{
    let (l_index, mut l_set, mut l_fixpoint_set) = rewrite_state_formula_impl(
        lhs,
        id_map, enclosing_parity, equations,
    )?;
    let (r_index, r_set, r_fixpoint_set) = rewrite_state_formula_impl(
        rhs,
        id_map, enclosing_parity, equations,
    )?;
    l_set.extend(&r_set);
    l_fixpoint_set.extend(r_fixpoint_set);

    equations.push((
        constructor(l_index, r_index),
        l_set.is_empty(),
    ));
    Ok((equations.len() - 1, l_set, l_fixpoint_set))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::proc::Action;
    use crate::core::lexer::tokenize;
    use crate::core::parser::Parser;

    #[test]
    fn test_rewrite_state_formula() {
        let tokens = tokenize("nu X . ([b] X && <a> true)").unwrap();
        let formula = Parser::new(&tokens).parse::<StateFormula>().unwrap();
        let formula_system = rewrite_state_formula(&formula).unwrap();

        for equation in &formula_system.equations {
            match &equation.0 {
                IrStateFormulaEquation::Nu { reset_variables, .. } => {
                    // no resetting should happen, because this is a closed formula
                    assert_eq!(reset_variables.len(), 0);
                },
                IrStateFormulaEquation::Diamond { .. } => {
                    assert!(equation.1); // `<a> true` is closed
                },
                IrStateFormulaEquation::Box { .. } => {
                    assert!(!equation.1); // `[b] X` is not closed
                },
                IrStateFormulaEquation::And { .. } => {
                    assert!(!equation.1); // `[b] X && <a> true` is not closed
                },
                _ => {},
            }
        }

        let tokens = tokenize("nu X . nu Y . (<a> X || <b> Y || <c> true)").unwrap();
        let formula = Parser::new(&tokens).parse::<StateFormula>().unwrap();
        let formula_system = rewrite_state_formula(&formula).unwrap();

        for equation in &formula_system.equations {
            if let IrStateFormulaEquation::Nu { reset_variables, .. } = &equation.0 {
                // no resetting should happen, because the fixpoints have the
                // same sign
                assert_eq!(reset_variables.len(), 0);
            }
        }

        let tokens = tokenize("nu X . mu Y . (<a> X || <b> Y || <c> true)").unwrap();
        let formula = Parser::new(&tokens).parse::<StateFormula>().unwrap();
        let formula_system = rewrite_state_formula(&formula).unwrap();

        for (i, equation) in formula_system.equations.iter().enumerate() {
            if let IrStateFormulaEquation::Nu { reset_variables, .. } = &equation.0 {
                // no resetting should happen for the nu, bcause it is closed
                assert_eq!(reset_variables.len(), 0);
            } else if let IrStateFormulaEquation::Mu { reset_variables, .. } = &equation.0 {
                // the mu should be reset though, because it is not closed and
                // is of a different sign
                assert_eq!(reset_variables.len(), 1);
                assert_eq!(reset_variables[0], i);
            }
        }
    }

    #[test]
    fn test_calculate_set() {
        let tokens = tokenize("nu X . <a> X").unwrap();
        let has_infinite_a_loop = Parser::new(&tokens).parse::<StateFormula>().unwrap();

        let tokens = tokenize("nu X . ([a] X && <a> true)").unwrap();
        let has_no_deadlock = Parser::new(&tokens).parse::<StateFormula>().unwrap();

        let tokens = tokenize("<a> true").unwrap();
        let can_do_action = Parser::new(&tokens).parse::<StateFormula>().unwrap();

        let lts = Lts::from_edge_list(0, 5, vec![
            (0, vec![Action { id: Identifier::new("a"), args: Vec::new() }], 1),
            (1, vec![Action { id: Identifier::new("a"), args: Vec::new() }], 2),
            (2, vec![Action { id: Identifier::new("a"), args: Vec::new() }], 3),
            (3, vec![Action { id: Identifier::new("a"), args: Vec::new() }], 0),
            (0, vec![Action { id: Identifier::new("a"), args: Vec::new() }], 4),
        ]);

        let mut state_set_manager = StateSetManager::new(lts.nodes.len());

        let set = calculate_set(&lts, &has_infinite_a_loop, &mut state_set_manager).unwrap();
        assert_eq!(set, state_set_manager.create_from_slice(&[0, 1, 2, 3]));

        let set = calculate_set(&lts, &has_no_deadlock, &mut state_set_manager).unwrap();
        assert!(set.is_empty());

        let set = calculate_set(&lts, &can_do_action, &mut state_set_manager).unwrap();
        assert_eq!(set, state_set_manager.create_from_slice(&[0, 1, 2, 3]));
    }
}
