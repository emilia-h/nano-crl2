
use crate::ast::formula::{StateFormula, StateFormulaEnum};
use crate::core::error::Mcrl2Error;
use crate::core::syntax::Identifier;
use crate::lts::lts::Lts;

use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::rc::Rc;

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

pub fn check_formula(lts: &Lts, formula: &StateFormula) -> Result<bool, FormulaCheckError> {
    let set = calculate_set(lts, formula)?;
    Ok(set.contains(&lts.initial_state))
}

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

    let result = match &formula.value {
        True => {
            full_set
        },
        False => {
            Rc::new(HashSet::new())
        },
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
        Not { value } => {
            let v = calculate_set_impl(lts, value, full_set.clone(), environment)?;

            Rc::new(full_set.difference(&v).map(|&x| x).collect())
        },
    };
    Ok(result)
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
