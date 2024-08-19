
pub fn chain_result<T1, T2>(
    result1: Result<T1, ()>,
    result2: Result<T2, ()>,
) -> Result<(T1, T2), ()> {
    match (result1, result2) {
        (Ok(value1), Ok(value2)) => Ok((value1, value2)),
        (Ok(_), Err(error2)) => Err(error2),
        (Err(error1), Ok(_)) => Err(error1),
        (Err(error1), Err(_)) => Err(error1),
    }
}

pub fn chain_option<T1, T2>(
    option1: Option<T1>,
    option2: Option<T2>,
) -> Option<(T1, T2)> {
    match (option1, option2) {
        (Some(value1), Some(value2)) => Some((value1, value2)),
        _ => None,
    }
}
