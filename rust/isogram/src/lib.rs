use std::collections::HashSet;

pub fn check(candidate: &str) -> bool {
    let mut record: HashSet<char> = HashSet::new();
    for char in candidate
        .chars()
        .filter(|c| c.is_alphabetic())
        .map(|c| c.to_ascii_lowercase())
    {
        if record.contains(&char) {
            return false;
        }
        record.insert(char);
    }
    true
}
