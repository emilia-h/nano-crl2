
use crate::common::read_resource_file;

use nano_crl2::analysis::small_progress_measures::{IterationPolicy, solve_parity_game};
use nano_crl2::parity_game::parity_game::Player;
use nano_crl2::parity_game::pgsolver::parse_pgsolver_game;

use std::time::{Duration, SystemTime};

fn test_solve_parity_game(name: &str, correct: &[usize]) {
    use IterationPolicy::*;

    let mut file_path = String::from("tests/pg/");
    file_path.push_str(name);
    file_path.push_str(".gm");
    let pg_string = read_resource_file(&file_path);
    let pg = parse_pgsolver_game(&pg_string).unwrap();

    let seed = SystemTime::UNIX_EPOCH.elapsed()
        .unwrap_or(Duration::ZERO)
        .as_nanos() as u64;
    for policy in [
        InputOrder,
        RandomOrder { seed },
        DescendingDegreeOrder,
        ReverseBfs,
        PostOrderDfs,
    ] {
        let won_by_even = solve_parity_game(&pg, Player::Even, policy);
        assert_eq!(won_by_even, correct);
    }
}

#[test]
fn test_small_games() {
    test_solve_parity_game("small1", &[3]);
    test_solve_parity_game("small2", &[3]);
    test_solve_parity_game("small3", &[0, 4, 5]);
    test_solve_parity_game("small4", &[0, 1, 2, 3]);
    test_solve_parity_game("small5", &[0, 1, 2, 3, 4, 5, 6, 7]);
    test_solve_parity_game("small6", &[3]);
    test_solve_parity_game("small7", &[]);
    test_solve_parity_game("small8", &[0, 1, 2, 3, 4, 5, 6, 7]);
    test_solve_parity_game("small9", &[0, 1, 2, 4]);
}
