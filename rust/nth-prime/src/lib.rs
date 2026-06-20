pub fn nth(n: u32) -> u32 {
    let k = (n + 1) as usize; // 1-indexed rank of the prime we want

    let upper = prime_upper_bound(k);
    let lower = prime_lower_bound(k);
    let base_primes = base_primes_up_to(integer_sqrt(upper));

    count_primes_below(lower, &base_primes)
        .find_kth_in_tail(k, upper, &base_primes)
        .expect("proven prime bounds must contain the k-th prime at or below upper") as u32
}

// The count of primes in the closed range `[2, up_to]`.
#[derive(Clone, Copy)]
struct PrimePrefix {
    up_to: usize,
    count: usize,
}

impl PrimePrefix {
    // Resume the prime count from `up_to + 1` through `hi`, returning the value
    // at which the running count reaches `k`. `None` if no such prime is in range.
    fn find_kth_in_tail(self, k: usize, hi: usize, base_primes: &[usize]) -> Option<usize> {
        let mut count = self.count;
        for_each_prime_in(self.up_to + 1, hi, base_primes, |value| {
            count += 1;
            (count == k).then_some(value)
        })
    }
}

// Count the primes in [2, below - 1], returned as a PrimePrefix covering that range.
fn count_primes_below(below: usize, base_primes: &[usize]) -> PrimePrefix {
    let up_to = below.saturating_sub(1);
    let mut count = 0;
    for_each_prime_in(2, up_to, base_primes, |_| {
        count += 1;
        None::<usize>
    });
    PrimePrefix { up_to, count }
}

// Drive a segmented sieve over [lo, hi], invoking `f` on each prime in order.
// Short-circuits and returns the first `Some` that `f` produces.
fn for_each_prime_in<F>(lo: usize, hi: usize, base_primes: &[usize], mut f: F) -> Option<usize>
where
    F: FnMut(usize) -> Option<usize>,
{
    let low = lo.max(2);
    if low > hi {
        return None;
    }

    let mut is_prime = vec![true; hi - low + 1];

    for &p in base_primes {
        if p * p > hi {
            break;
        }
        let mut multiple = (low.div_ceil(p) * p).max(p * p); // first multiple of p at/above low
        while multiple <= hi {
            is_prime[multiple - low] = false;
            multiple += p;
        }
    }

    for (offset, &prime) in is_prime.iter().enumerate() {
        if prime {
            let value = low + offset;
            if let Some(found) = f(value) {
                return Some(found);
            }
        }
    }

    None
}

fn prime_upper_bound(k: usize) -> usize {
    if k < 6 {
        return 13; // covers the first 5 primes: 2,3,5,7,11
    }
    let kf = k as f64;
    let bound = kf * (kf.ln() + kf.ln().ln());
    bound.ceil() as usize + 1
}

fn prime_lower_bound(k: usize) -> usize {
    if k < 6 {
        return 2; // start from the very beginning for the first few primes
    }
    let kf = k as f64;
    let bound = kf * kf.ln();
    (bound.floor() as usize).max(2)
}

fn integer_sqrt(n: usize) -> usize {
    (n as f64).sqrt() as usize + 1
}

fn base_primes_up_to(limit: usize) -> Vec<usize> {
    if limit < 2 {
        return Vec::new();
    }
    let mut is_prime = vec![true; limit + 1];
    is_prime[0] = false;
    is_prime[1] = false;
    let mut p = 2;
    while p * p <= limit {
        if is_prime[p] {
            let mut multiple = p * p;
            while multiple <= limit {
                is_prime[multiple] = false;
                multiple += p;
            }
        }
        p += 1;
    }
    (2..=limit).filter(|&i| is_prime[i]).collect()
}
