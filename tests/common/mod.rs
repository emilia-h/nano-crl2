
use std::fs::read_to_string;
use std::path::PathBuf;

pub fn read_resource_file(file: &str) -> String {
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.push("res");
    p.push(file);

    read_to_string(p).unwrap()
}
