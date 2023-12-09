
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

#[cfg(test)]
pub fn unwrap_result<T, E>(x: Result<T, E>) -> T
where
    E: std::fmt::Debug
{
    match x {
        Ok(result) => result,
        Err(err) => panic!("{:?}", err),
    }
}
