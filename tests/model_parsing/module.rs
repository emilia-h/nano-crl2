
use crate::common::parse_module;

#[test]
fn test_general1_module() {
    let x = parse_module("should_compile/general1");
    x.unwrap();
}

#[test]
fn test_sets_module() {
    let x = parse_module("should_compile/sets");
    x.unwrap();
}
