use std::{collections::HashSet, hash::Hash};

#[derive(Debug, PartialEq, Eq)]
pub struct CustomSet<T: Clone + PartialEq + Eq + Hash> {
    hs: HashSet<T>,
}

impl<T: Clone + PartialEq + Eq + Hash> CustomSet<T> {
    pub fn new(input: &[T]) -> Self {
        let mut result = CustomSet { hs: HashSet::new() };
        for elem in input.iter() {
            result.add((*elem).clone());
        }
        result
    }

    pub fn contains(&self, element: &T) -> bool {
        self.hs.contains(element)
    }

    pub fn add(&mut self, element: T) {
        self.hs.insert(element);
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        self.hs.is_subset(&other.hs)
    }

    pub fn is_empty(&self) -> bool {
        self.hs.is_empty()
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.hs.is_disjoint(&other.hs)
    }

    #[must_use]
    pub fn intersection(&self, other: &Self) -> Self {
        CustomSet {
            hs: self
                .hs
                .intersection(&other.hs)
                .cloned()
                .collect::<HashSet<T>>(),
        }
    }

    #[must_use]
    pub fn difference(&self, other: &Self) -> Self {
        CustomSet {
            hs: self
                .hs
                .difference(&other.hs)
                .cloned()
                .collect::<HashSet<T>>(),
        }
    }

    #[must_use]
    pub fn union(&self, other: &Self) -> Self {
        CustomSet {
            hs: self.hs.union(&other.hs).cloned().collect::<HashSet<T>>(),
        }
    }
}
