use std::collections::HashSet;

pub fn is_pangram(sentence: &str) -> bool {
    let letters: HashSet<char> = sentence
        .chars()
        .filter_map(|c| c.is_alphabetic().then(|| c.to_ascii_lowercase()))
        .collect();
    letters.len() == 26
}
