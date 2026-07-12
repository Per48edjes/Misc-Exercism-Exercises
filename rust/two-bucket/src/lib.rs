use num_integer::gcd;
use std::cmp::{max, min};

#[derive(PartialEq, Eq, Debug)]
pub enum Bucket {
    One,
    Two,
}

/// A struct to hold your results in.
#[derive(PartialEq, Eq, Debug)]
pub struct BucketStats {
    /// The total number of "moves" it should take to reach the desired number of liters, including
    /// the first fill.
    pub moves: u8,
    /// Which bucket should end up with the desired number of liters? (Either "one" or "two")
    pub goal_bucket: Bucket,
    /// How many liters are left in the other bucket?
    pub other_bucket: u8,
}

#[derive(Debug)]
struct BucketState {
    filler: (Bucket, u8, u8),
    drainer: (Bucket, u8, u8),
}

impl BucketState {
    fn transition(&mut self) {
        match (self.filler.1, self.filler.2, self.drainer.1, self.drainer.2) {
            // Filler is empty: fill it.
            (0, fill_cap, _, _) => {
                self.filler.1 = fill_cap;
            }
            // Drainer is full: empty it.
            (_, _, drain_lvl, drain_cap) if drain_lvl == drain_cap => {
                self.drainer.1 = 0;
            }
            // Otherwise: pour filler into drainer until one is empty/full.
            (fill_lvl, _, drain_lvl, drain_cap) => {
                let quantity = min(drain_cap - drain_lvl, fill_lvl);
                self.filler.1 -= quantity;
                self.drainer.1 += quantity;
            }
        }
    }
}

pub fn solve(
    capacity_1: u8,
    capacity_2: u8,
    goal: u8,
    start_bucket: &Bucket,
) -> Option<BucketStats> {
    if goal > max(capacity_1, capacity_2) || !goal.is_multiple_of(gcd(capacity_1, capacity_2)) {
        return None;
    }

    let (filler, drainer) = match start_bucket {
        Bucket::One => ((Bucket::One, 0, capacity_1), (Bucket::Two, 0, capacity_2)),
        Bucket::Two => ((Bucket::Two, 0, capacity_2), (Bucket::One, 0, capacity_1)),
    };

    let mut state = BucketState { filler, drainer };

    // When the goal equals the drainer's capacity, filling it directly beats pouring.
    if goal == state.drainer.2 {
        return Some(BucketStats {
            moves: 2,
            goal_bucket: state.drainer.0,
            other_bucket: state.filler.2,
        });
    }

    let mut moves = 0u8;
    while state.filler.1 != goal && state.drainer.1 != goal {
        state.transition();
        moves += 1
    }

    let (goal_bucket, other_bucket) = if state.filler.1 == goal {
        (state.filler.0, state.drainer.1)
    } else {
        (state.drainer.0, state.filler.1)
    };

    Some(BucketStats {
        moves,
        goal_bucket,
        other_bucket,
    })
}
