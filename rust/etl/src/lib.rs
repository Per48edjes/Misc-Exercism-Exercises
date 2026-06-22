use std::collections::BTreeMap;

pub fn transform(h: &BTreeMap<i32, Vec<char>>) -> BTreeMap<char, i32> {
    h.iter().fold(BTreeMap::new(), |mut acc, (&k, v)| {
        v.iter().for_each(|&l| {
            acc.insert(l.to_lowercase().next().unwrap(), k);
        });
        acc
    })
}
