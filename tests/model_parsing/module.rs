
use crate::common::read_resource_file;

use nano_crl2::core::lexer::tokenize;
use nano_crl2::core::parser::Parser;
use nano_crl2::model::module::Module;

fn parse_module(name: &str) -> Option<Module> {
    let mut file_path = String::from("tests/mcrl2/");
    file_path.push_str(name);
    file_path.push_str(".mcrl2");
    let module_string = read_resource_file(&file_path);

    let tokens = tokenize(&module_string).ok()?;
    let ast = Parser::new(&tokens).parse::<Module>().unwrap();
    Some(ast)
}

#[test]
fn test_general1_module() {
    let x = parse_module("general1");
    x.unwrap();
}

#[test]
fn test_sets_module() {
    let x = parse_module("sets");
    x.unwrap();
}
