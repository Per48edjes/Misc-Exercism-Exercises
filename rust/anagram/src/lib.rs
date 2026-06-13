use std::collections::{HashMap, HashSet};

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let lowered_word = word.to_lowercase();
    let bagged_word = bagger(&lowered_word);
    possible_anagrams
        .iter()
        .copied()
        .filter(|cand| {
            let lower = cand.to_lowercase();
            bagger(&lower) == bagged_word && lower != lowered_word
        })
        .collect()
}

fn bagger(word: &str) -> HashMap<char, usize> {
    word.chars().fold(HashMap::new(), |mut acc, c| {
        *acc.entry(c).or_insert(0) += 1;
        acc
    })
}
