// `self` in the import brings the `fmt` module name itself into scope, so we can
// write `fmt::Result` below without shadowing the prelude's `Result<T, E>`.
use std::fmt::{self, Display, Formatter};

// `Debug` (derived from the fields) is independent of our hand-written `Display`.
// It exists here only because `assert_eq!`/`assert_ne!` print operands via `{:?}`
// on failure, which requires `Debug`.
#[derive(Debug, PartialEq, Eq)]
pub struct Clock {
    hour: i32,
    minute: i32,
}

impl Display for Clock {
    // `Formatter` holds a mutable borrow of the caller's output buffer; the `<'_>`
    // is that buffer-borrow's lifetime, left anonymous because we never name it.
    // We write *into* `f` rather than returning a String, so one buffer is threaded
    // through the whole format tree with no intermediate allocations.
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.hour, self.minute)
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let total = hours * 60 + minutes;
        // `rem_euclid` is true modulo (always non-negative), unlike `%` which keeps
        // the dividend's sign; needed so negative inputs wrap correctly.
        let minutes_in_day = total.rem_euclid(24 * 60);
        Clock {
            hour: minutes_in_day / 60,
            minute: minutes_in_day % 60,
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock::new(self.hour, self.minute + minutes)
    }
}

