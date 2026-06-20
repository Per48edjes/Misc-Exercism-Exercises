pub fn factors(n: u64) -> Vec<u64> {
    let mut result = vec![];
    let mut remaining = n;

    for k in sieve(remaining.isqrt()) {
        if k > remaining {
            break;
        }
        while remaining.is_multiple_of(k) {
            result.push(k);
            remaining /= k;
        }
    }

    // at most 1 prime factor can be larger than sqrt(n)
    if remaining > 1 {
        result.push(remaining);
    }

    result
}

fn sieve(r: u64) -> Vec<u64> {
    if r < 2 {
        return vec![];
    }

    let limit = r as usize;
    let mut is_prime = vec![true; limit + 1];
    is_prime[0] = false;
    is_prime[1] = false;

    let mut p = 2;
    while p * p <= limit {
        if is_prime[p] {
            for multiple in (p * p..=limit).step_by(p) {
                is_prime[multiple] = false;
            }
        }
        p += 1;
    }

    (2..=limit)
        .filter(|&i| is_prime[i])
        .map(|i| i as u64)
        .collect()
}
