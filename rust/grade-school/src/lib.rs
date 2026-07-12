use std::collections::{BTreeMap, BTreeSet};

pub struct School {
    roster: BTreeMap<u32, BTreeSet<String>>,
    students: BTreeSet<String>,
}

impl School {
    pub fn new() -> School {
        School {
            roster: BTreeMap::new(),
            students: BTreeSet::new(),
        }
    }

    pub fn add(&mut self, grade: u32, student: &str) {
        if !self.students.contains(student) {
            self.roster
                .entry(grade)
                .or_default()
                .insert(student.to_owned())
                .then(|| self.students.insert(student.to_owned()));
        }
    }

    pub fn grades(&self) -> Vec<u32> {
        self.roster.keys().cloned().collect()
    }

    // If `grade` returned a reference, `School` would be forced to keep a `Vec<String>`
    // internally to lend out. By returning an owned vector of owned `String`s instead,
    // the internal structure can be completely arbitrary. The tradeoff is that some data
    // must be copied each time `grade` is called.
    pub fn grade(&self, grade: u32) -> Vec<String> {
        self.roster
            .get(&grade)
            .map(|students| students.iter().cloned().collect())
            .unwrap_or_default()
    }
}
