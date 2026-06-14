use std::iter::successors;

pub fn collatz(n: u64) -> Option<u64> {
    if n == 0 {
        return None;
    }
    let steps = successors(Some(n), |&n| match n {
        1 => None,
        n if n % 2 == 0 => Some(n / 2),
        n => Some(3 * n + 1),
    })
    .count() as u64;
    Some(steps - 1)
}
