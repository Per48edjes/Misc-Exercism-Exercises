/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    match parse_number(code) {
        Some(nums) if nums.len() > 1 => validate_number(nums),
        _ => false,
    }
}

fn parse_number(code: &str) -> Option<Vec<u32>> {
    code.chars()
        .filter(|c| !c.is_whitespace())
        .map(|c| c.to_digit(10))
        .collect()
}

fn validate_number(mut nums: Vec<u32>) -> bool {
    nums.iter_mut()
        .rev()
        .enumerate()
        .map(|(i, v)| {
            if i % 2 == 1 {
                *v *= 2;
                if *v > 9 {
                    *v -= 9;
                }
            }
        })
        .for_each(drop);

    nums.into_iter().sum::<u32>() % 10 == 0
}
