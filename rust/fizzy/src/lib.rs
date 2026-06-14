pub struct Matcher<T> {
    // TYPE ERASURE: Each closure has a unique anonymous type, but we need to store
    // heterogeneous closures in a Vec. Box<dyn Fn(T) -> bool> erases the concrete
    // type behind a vtable, giving uniform layout at the cost of dynamic dispatch.
    matcher: Box<dyn Fn(T) -> bool>,
    // Owned String (not &str) avoids lifetime params on the struct. Accept flexible
    // input types at the API boundary (Into<String>), store concrete types internally.
    subs: String,
}

impl<T> Matcher<T> {
    // F is generic (monomorphized per closure type) — lets callers pass bare closures.
    // Inside new(), Box::new(matcher) erases F's concrete type into the trait object.
    // 'static bound: the boxed closure can't borrow short-lived data (Box<dyn ...> is
    // implicitly 'static). Closures that only capture owned/Copy values satisfy this.
    // S: Into<String> lets callers pass &str or String without .to_string() at call site.
    pub fn new<F: Fn(T) -> bool + 'static, S: Into<String>>(matcher: F, subs: S) -> Matcher<T> {
        Matcher {
            matcher: Box::new(matcher),
            subs: subs.into(),
        }
    }
}

pub struct Fizzy<T> {
    matchers: Vec<Matcher<T>>,
}

// Trait bounds go on impl blocks/methods that need them, not on struct definitions.
// The struct is just a layout — it doesn't use T's traits. This avoids polluting every
// consumer with bounds they don't need.
impl<T> Fizzy<T> {
    pub fn new() -> Self {
        Fizzy {
            matchers: Vec::new(),
        }
    }

    // CONSUMING BUILDER: `mut self` (not `&mut self`) takes ownership, mutates, returns.
    // This enables chaining: Fizzy::new().add_matcher(...).add_matcher(...)
    // Returning Self (owned) lets the final result be assigned to a Fizzy<T> binding.
    // #[must_use] warns if the returned value is discarded (since self was consumed).
    #[must_use]
    pub fn add_matcher(mut self, matcher: Matcher<T>) -> Self {
        self.matchers.push(matcher);
        self
    }

    // I: Iterator<Item = T> connects "what iter yields" to "what matchers accept"
    // T: Copy — each element is passed to multiple matchers by value; without Copy,
    // the first matcher would consume it.
    // T: ToString — fallback when no matchers fire.
    pub fn apply<I>(self, iter: I) -> impl Iterator<Item = String>
    where
        I: Iterator<Item = T>,
        T: Copy + ToString,
    {
        // `move` transfers ownership of self (and its matchers Vec) into the closure.
        // self isn't dropped here — it lives inside the returned Map iterator.
        // It's only dropped when the iterator is fully consumed/dropped.
        iter.map(move |element| {
            let result: String = self
                .matchers
                .iter() // borrow matchers (not into_iter) — reused for every element
                .filter(|m| (m.matcher)(element)) // (m.matcher)(x) — parens needed to call field, not method
                .map(|m| m.subs.clone()) // clone because we're borrowing m via .iter()
                .collect(); // collect String fragments via FromIterator (concatenation)
            if result.is_empty() {
                element.to_string()
            } else {
                result
            }
        })
    }
}

// More bounds than apply because we're *constructing* specific closures here:
// Rem<Output = T> — the % operator, output must be T for comparison
// PartialEq — the == in `n % 3 == 0`
// Default — T::default() gives zero for the comparison target
// From<u8> — T::from(3u8) constructs divisors generically (no integer literals for T)
// 'static — closures capturing T values (via From) get boxed; must satisfy Box lifetime
pub fn fizz_buzz<T>() -> Fizzy<T>
where
    T: Copy + Default + From<u8> + PartialEq + std::ops::Rem<Output = T> + ToString + 'static,
{
    Fizzy::new()
        .add_matcher(Matcher::new(
            |n: T| n % T::from(3u8) == T::default(),
            "fizz",
        ))
        .add_matcher(Matcher::new(
            |n: T| n % T::from(5u8) == T::default(),
            "buzz",
        ))
}
