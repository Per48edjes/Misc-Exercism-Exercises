use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(first_list: &[T], second_list: &[T]) -> Comparison {
    match first_list.len().cmp(&second_list.len()) {
        Ordering::Equal if first_list == second_list => Comparison::Equal,
        Ordering::Less if second_list.contains_sub(first_list) => Comparison::Sublist,
        Ordering::Greater if first_list.contains_sub(second_list) => Comparison::Superlist,
        _ => Comparison::Unequal,
    }
}

trait SubListExt<T> {
    fn contains_sub(&self, smaller: &[T]) -> bool;
}

impl<T: PartialEq> SubListExt<T> for [T] {
    fn contains_sub(&self, smaller: &[T]) -> bool {
        smaller.is_empty() || self.windows(smaller.len()).any(|w| w == smaller)
    }
}
