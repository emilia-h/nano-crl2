//! This module defines macros, functions and data structures that are useful to the rest
//! of the crate, but not part of the functionality of the tool/library.

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

/// Extracts the success value of a `Result`, assuming it is [`Ok`].
/// 
/// # Panics
/// Panics if `x` is [`Err`] instead.
/// 
/// [`Ok`]: https://doc.rust-lang.org/std/result/enum.Result.html#variant.Ok
/// [`Err`]: https://doc.rust-lang.org/std/result/enum.Result.html#variant.Err
pub fn unwrap_result<T, E>(x: Result<T, E>) -> T
where
    E: std::fmt::Debug
{
    match x {
        Ok(result) => result,
        Err(err) => panic!("{:?}", err),
    }
}
