
use nano_crl2::analysis::ir_conversion::module::query_ir_module;

use crate::common::create_context_from_file;

fn check_compilation(name: &str) {
    let (context, module_id) = create_context_from_file(name);
    let ir_module = query_ir_module(&context, module_id);
    ir_module.unwrap();
    // TODO actual checking
}

#[test]
fn test_general1() {
    check_compilation("should_compile/general1");
}

#[test]
fn test_sets() {
    check_compilation("should_compile/sets");
}

#[test]
fn test_structs() {
    check_compilation("should_compile/structs");
}
