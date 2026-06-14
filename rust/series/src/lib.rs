use std::iter::repeat_n;

pub fn series(digits: &str, len: usize) -> Vec<String> {
    if len == 0 {
        repeat_n("".to_owned(), digits.len() + 1).collect()
    } else {
        digits
            .chars()
            .collect::<Vec<char>>()
            .windows(len)
            .map(|w| w.iter().collect())
            .collect()
    }
}
