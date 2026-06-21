use std::collections::HashMap;

const STUDENTS: [&str; 12] = [
    "Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph",
    "Kincaid", "Larry",
];

const PLANTS: [&str; 4] = ["grass", "clover", "radishes", "violets"];

pub fn plants(diagram: &str, student: &str) -> Vec<&'static str> {
    let student_map: HashMap<&str, usize> = STUDENTS
        .into_iter()
        .enumerate()
        .map(|(i, s)| (s, i))
        .collect();

    let plant_map: HashMap<char, &str> = PLANTS
        .into_iter()
        .map(|p| (p.chars().next().unwrap().to_ascii_uppercase(), p))
        .collect();

    let pos = 2 * student_map.get(student).unwrap();

    diagram
        .lines()
        .flat_map(|r| {
            [
                *plant_map.get(&r.chars().nth(pos).unwrap()).unwrap(),
                *plant_map.get(&r.chars().nth(pos + 1).unwrap()).unwrap(),
            ]
        })
        .collect()
}
