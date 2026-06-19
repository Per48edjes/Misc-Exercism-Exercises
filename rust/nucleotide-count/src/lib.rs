use std::collections::HashMap;

pub const NUCLEOTIDES: [char; 4] = ['A', 'T', 'C', 'G'];

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    nucleotide_counts(dna)?
        .get(&nucleotide)
        .copied()
        .ok_or(nucleotide)
}

pub fn nucleotide_counts(dna: &str) -> Result<HashMap<char, usize>, char> {
    let counter: HashMap<char, usize> = NUCLEOTIDES.into_iter().map(|k| (k, 0)).collect();
    dna.chars().try_fold(counter, |mut acc, c| {
        acc.get_mut(&c).map(|freq| *freq += 1).ok_or(c)?;
        Ok(acc)
    })
}
