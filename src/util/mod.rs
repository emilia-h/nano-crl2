//! Defines macros, functions and data structures that are useful to the rest
//! of the crate, but not part of the functionality of the tool/library.

pub mod hashing;
pub mod parsing;

// https://stackoverflow.com/questions/34953711/unwrap-inner-type-when-enum-variant-is-known
#[macro_export]
macro_rules! unwrap_pattern {
    ($value:expr, $pattern:pat => $extracted_value:expr) => {
        match $value {
            $pattern => $extracted_value,
            _ => panic!("Pattern doesn't match"),
        }
    };
}
