
use std::fs::read_to_string;
use std::path::PathBuf;

use nano_crl2::analysis::context::AnalysisContext;
use nano_crl2::core::lexer::tokenize;
use nano_crl2::core::parser::Parser;
use nano_crl2::core::syntax::ModuleId;
use nano_crl2::model::module::Module;

pub fn read_resource_file(file: &str) -> String {
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.push("res");
    p.push(file);

    read_to_string(p).unwrap()
}

pub fn parse_module(name: &str) -> Option<Module> {
    let mut file_path = String::from("tests/");
    file_path.push_str(name);
    file_path.push_str(".mcrl2");
    let module_string = read_resource_file(&file_path);

    let tokens = tokenize(&module_string).ok()?;
    let ast = Parser::new(&tokens).parse::<Module>().ok()?;
    Some(ast)
}

pub fn create_context_from_file(name: &str) -> (AnalysisContext, ModuleId) {
    let mut file_path = String::from("tests/");
    file_path.push_str(name);
    file_path.push_str(".mcrl2");
    let module_string = read_resource_file(&file_path);
    let mut context = AnalysisContext::new();
    let module_id = context.add_model_input("test".to_owned(), module_string);
    (context, module_id)
}
