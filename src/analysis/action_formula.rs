
use crate::analysis::emerson_lei::FormulaError;
use crate::model::proc::Action;
use crate::mu_calculus::action_formula::{ActionFormula, ActionFormulaEnum};

pub fn matches_action_formula(
    multi_action: &Vec<Action>,
    action_formula: &ActionFormula,
) -> Result<bool, FormulaError> {
    Ok(match &action_formula.value {
        ActionFormulaEnum::Val { value: _ } => {
            todo!("expression rewriting")
        },
        ActionFormulaEnum::MultiAction { values } => {
            if values.len() != multi_action.len() {
                false
            } else {
                let mut same = true;
                for i in 0 .. values.len() {
                    if !matches_action(&multi_action[i], &values[i])? {
                        same = false;
                        break;
                    }
                }
                same
            }
        },
        ActionFormulaEnum::True => true,
        ActionFormulaEnum::False => false,
        ActionFormulaEnum::Forall { ids: _, action_formula: _ } => {
            todo!()
        },
        ActionFormulaEnum::Exists { ids: _, action_formula: _ } => {
            todo!()
        },
        ActionFormulaEnum::Implies { lhs, rhs } => {
            !(matches_action_formula(multi_action, lhs)?) ||
            matches_action_formula(multi_action, rhs)?
        },
        ActionFormulaEnum::Or { lhs, rhs } => {
            matches_action_formula(multi_action, lhs)? ||
            matches_action_formula(multi_action, rhs)?
        },
        ActionFormulaEnum::And { lhs, rhs } => {
            matches_action_formula(multi_action, lhs)? &&
            matches_action_formula(multi_action, rhs)?
        },
        ActionFormulaEnum::Not { value } => {
            !(matches_action_formula(multi_action, value)?)
        },
        ActionFormulaEnum::Time { .. } => {
            unimplemented!();
        },
    })
}

pub fn matches_action(action1: &Action, action2: &Action) -> Result<bool, FormulaError> {
    // TODO: this needs to be based on semantic analysis and seeing if they
    // really refer to the same action declaration
    if action1.id.get_value() != action2.id.get_value() {
        return Ok(false);
    }
    if action1.args.len() != action2.args.len() {
        return Ok(false);
    }

    let same = true;
    for _i in 0 .. action1.args.len() {
        todo!("expression rewriting and matching");
    }
    Ok(same)
}
