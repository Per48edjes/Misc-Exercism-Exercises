use std::iter as it;

pub fn is_armstrong_number(num: u32) -> bool {
    let digits: Vec<u32> = it::successors(Some(num), |&n| (n >= 10).then_some(n / 10))
        .map(|n| n % 10)
        .collect();

    let l = digits.len() as u32;

    let total = digits
        .iter()
        .try_fold(0u32, |acc, &d| acc.checked_add(d.checked_pow(l)?));

    total == Some(num)
}
