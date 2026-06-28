use std::collections::{HashMap, HashSet};

use regex_lite::Regex;

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    let (summands, sum) = parse_input(input)?;

    let leading: HashSet<char> = summands
        .iter()
        .chain(std::iter::once(&sum))
        .filter(|w| w.len() > 1)
        .filter_map(|w| w.chars().next())
        .collect();

    let distinct: HashSet<char> = summands
        .iter()
        .chain(std::iter::once(&sum))
        .flat_map(|w| w.chars())
        .collect();

    if distinct.len() > 10 {
        return None;
    }

    let max_cols = summands
        .iter()
        .map(|w| w.len())
        .max()
        .unwrap_or(0)
        .max(sum.len());

    let columns = build_columns(&summands, sum, max_cols);

    let mut assignment: HashMap<char, u8> = HashMap::new();
    let mut used = [false; 10];

    if solve_columns(0, 0, &columns, &leading, &mut assignment, &mut used) {
        Some(assignment)
    } else {
        None
    }
}

fn parse_input(input: &str) -> Option<(Vec<&str>, &str)> {
    let re = Regex::new(r"[A-Z]+").unwrap();
    let mut summands: Vec<&str> = re.find_iter(input).map(|m| m.as_str()).collect();
    let sum = summands.pop()?;
    Some((summands, sum))
}

struct Column {
    summands: HashMap<char, u32>,
    result: Option<char>,
}

fn build_columns(summands: &[&str], sum: &str, max_cols: usize) -> Vec<Column> {
    (0..max_cols)
        .map(|c| {
            let mut counts: HashMap<char, u32> = HashMap::new();
            for letter in summands.iter().filter_map(|w| w.chars().rev().nth(c)) {
                *counts.entry(letter).or_insert(0) += 1;
            }
            Column {
                summands: counts,
                result: sum.chars().rev().nth(c),
            }
        })
        .collect()
}

fn solve_columns(
    c: usize,
    carry: u32,
    columns: &[Column],
    leading: &HashSet<char>,
    assignment: &mut HashMap<char, u8>,
    used: &mut [bool; 10],
) -> bool {
    if c == columns.len() {
        return carry == 0;
    }

    let column = &columns[c];

    if let Some(&letter) = column.summands.keys().find(|l| !assignment.contains_key(l)) {
        // 1st recursion: generate assignment of letters -> digits for given column
        for digit in 0u8..=9 {
            if used[digit as usize] {
                continue;
            }
            if digit == 0 && leading.contains(&letter) {
                continue;
            }
            used[digit as usize] = true;
            assignment.insert(letter, digit);
            if solve_columns(c, carry, columns, leading, assignment, used) {
                return true;
            }
            used[digit as usize] = false;
            assignment.remove(&letter);
        }
        false
    } else {
        // 2nd recursion: confirm invariant on the column
        let total: u32 = carry
            + column
                .summands
                .iter()
                .map(|(letter, &count)| assignment[letter] as u32 * count)
                .sum::<u32>();
        let expected: u8 = (total % 10) as u8;
        let carry_out: u32 = total / 10;

        let Some(sum_letter) = column.result else {
            return false;
        };
        match assignment.get(&sum_letter) {
            Some(&digit) => {
                expected == digit
                    && solve_columns(c + 1, carry_out, columns, leading, assignment, used)
            }
            _ => {
                if expected == 0 && leading.contains(&sum_letter) || used[expected as usize] {
                    return false;
                }
                used[expected as usize] = true;
                assignment.insert(sum_letter, expected);
                if solve_columns(c + 1, carry_out, columns, leading, assignment, used) {
                    return true;
                }
                used[expected as usize] = false;
                assignment.remove(&sum_letter);
                false
            }
        }
    }
}
