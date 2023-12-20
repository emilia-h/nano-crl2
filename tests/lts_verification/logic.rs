
use nano_crl2::core::state_set::{StateSet, StateSetManager};
use nano_crl2::lts::lts::{Lts, parse_aldebaran_lts};
use nano_crl2::lts::verification::calculate_set;
use nano_crl2::parser::lexer::tokenize;
use nano_crl2::parser::parser::Parser;

use std::fs::read_to_string;
use std::path::PathBuf;

fn test_calculate_set(
    lts: &Lts,
    formula_string: &str,
    correct: StateSet,
    state_set_manager: &mut StateSetManager,
) {
    let tokens = tokenize(formula_string).unwrap();
    let formula = Parser::new(&tokens).parse_state_formula().unwrap();
    eprintln!("{:?}", formula);

    let result = calculate_set(&lts, &formula, state_set_manager).unwrap();
    assert_eq!(result, correct);
}

#[test]
fn test_logical_operators() {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("res/tests/lts/basic.aut");

    let lts_string = read_to_string(d).unwrap();
    let lts = parse_aldebaran_lts(&lts_string).unwrap();
    assert_eq!(lts.nodes.len(), 10);
    assert_eq!(
        lts.nodes.iter().map(|x| x.adj.len()).collect::<Vec<_>>(),
        vec![3, 1, 4, 1, 1, 1, 2, 0, 2, 1],
    );

    let mut ssm = StateSetManager::new(lts.nodes.len());
    test_calculate_set(&lts, "false", ssm.get_empty(), &mut ssm);
    test_calculate_set(&lts, "true", ssm.get_full(), &mut ssm);
    test_calculate_set(&lts, "false && false", ssm.get_empty(), &mut ssm);
    test_calculate_set(&lts, "false && true", ssm.get_empty(), &mut ssm);
    test_calculate_set(&lts, "true && false", ssm.get_empty(), &mut ssm);
    test_calculate_set(&lts, "true && true", ssm.get_full(), &mut ssm);
    test_calculate_set(&lts, "false || false", ssm.get_empty(), &mut ssm);
    test_calculate_set(&lts, "false || true", ssm.get_full(), &mut ssm);
    test_calculate_set(&lts, "true || false", ssm.get_full(), &mut ssm);
    test_calculate_set(&lts, "true || true", ssm.get_full(), &mut ssm);
    test_calculate_set(&lts, "false => false", ssm.get_full(), &mut ssm);
    test_calculate_set(&lts, "false => true", ssm.get_full(), &mut ssm);
    test_calculate_set(&lts, "true => false", ssm.get_empty(), &mut ssm);
    test_calculate_set(&lts, "true => true", ssm.get_full(), &mut ssm);
}
