use std::collections::BTreeMap;

#[derive(Debug)]
pub struct HighScores {
    scores: BTreeMap<u32, u32>,
    insertions: Vec<u32>,
}

impl HighScores {
    pub fn new(scores: &[u32]) -> Self {
        let mut data: BTreeMap<u32, u32> = BTreeMap::new();
        let insertions: Vec<u32> = Vec::from(scores);

        scores.iter().for_each(|&score| {
            data.entry(score).and_modify(|curr| *curr += 1).or_insert(1);
        });

        HighScores {
            scores: data,
            insertions,
        }
    }

    pub fn scores(&self) -> &[u32] {
        &self.insertions
    }

    pub fn latest(&self) -> Option<u32> {
        self.insertions.last().copied()
    }

    pub fn personal_best(&self) -> Option<u32> {
        match self.scores.last_key_value() {
            Some((&k, _)) => Some(k),
            _ => None,
        }
    }

    pub fn personal_top_three(&self) -> Vec<u32> {
        let mut result: Vec<u32> = Vec::new();
        let mut top: u32 = 3;
        let mut scores_iter = self.scores.iter().rev();

        while top > 0 {
            match scores_iter.next() {
                Some((&k, &v)) => {
                    (0..v.min(top)).for_each(|_| result.push(k));
                    top = top.saturating_sub(v)
                }
                _ => {
                    break;
                }
            }
        }

        result
    }
}
