use std::fmt::Write;

const NUM_MAP: [&str; 11] = [
    "no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
];

fn bottles(n: u32) -> &'static str {
    if n == 1 { "bottle" } else { "bottles" }
}

fn capitalize(word: &str) -> String {
    let mut chars = word.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

pub fn recite(start_bottles: u32, take_down: u32) -> String {
    let mut buffer = String::new();
    for n in ((start_bottles - take_down + 1)..=start_bottles).rev() {
        let count = NUM_MAP[n as usize];
        let next = NUM_MAP[(n - 1) as usize];

        writeln!(
            &mut buffer,
            "{cap} green {bottles} hanging on the wall,",
            cap = capitalize(count),
            bottles = bottles(n)
        )
        .unwrap();
        writeln!(
            &mut buffer,
            "{cap} green {bottles} hanging on the wall,",
            cap = capitalize(count),
            bottles = bottles(n)
        )
        .unwrap();
        writeln!(
            &mut buffer,
            "And if one green bottle should accidentally fall,"
        )
        .unwrap();
        writeln!(
            &mut buffer,
            "There'll be {next} green {bottles} hanging on the wall.",
            bottles = bottles(n - 1)
        )
        .unwrap();

        if n > start_bottles - take_down + 1 {
            writeln!(&mut buffer).unwrap();
        }
    }

    buffer
}
