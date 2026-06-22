use std::cmp::Ordering;

// Assumes input slice is sorted
pub fn find<T: Ord, C: AsRef<[T]>>(array: C, key: T) -> Option<usize> {
    let array = array.as_ref();
    let (mut l, mut r): (usize, usize) = (0, array.len().checked_sub(1)?);
    while l <= r {
        let guess: usize = l + (r - l) / 2;
        match key.cmp(&array[guess]) {
            Ordering::Equal => return Some(guess),
            Ordering::Greater => l = guess.checked_add(1)?,
            Ordering::Less => r = guess.checked_sub(1)?,
        }
    }
    None
}
