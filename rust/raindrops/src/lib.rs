pub fn raindrops(n: u32) -> String {
    let pairs: Vec<(u32, &str)> = vec![(3, "Pling"), (5, "Plang"), (7, "Plong")];

    let result = pairs
        .iter()
        .map(|(k, v)| if n % k == 0 { &v } else { "" })
        .collect::<String>();

    if result.is_empty() {
        n.to_string()
    } else {
        result
    }
}
