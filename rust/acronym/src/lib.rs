pub fn abbreviate(phrase: &str) -> String {
    phrase
        .replace('-', " ")
        .chars()
        .filter(|c| c.is_alphabetic() || c.is_whitespace())
        .collect::<String>()
        .split_whitespace()
        .flat_map(|s| {
            let mut letters = Vec::new();
            let mut prev = None;
            for c in s.chars() {
                let is_first = prev.is_none();
                let is_hump = prev.is_some_and(|p: char| p.is_lowercase()) && c.is_uppercase();
                if is_first || is_hump {
                    letters.push(c.to_ascii_uppercase());
                }
                prev = Some(c);
            }
            letters
        })
        .collect()
}
