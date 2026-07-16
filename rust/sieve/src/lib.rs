use std::collections::HashSet;

pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    let mut primes: Vec<u64> = Vec::new();

    if upper_bound < 2 {
        return primes;
    } else {
        let mut p: u64 = 2;
        let mut composites: HashSet<u64> = HashSet::from([0, 1]);

        while p <= upper_bound {
            if composites.contains(&p) {
                p += 1;
                continue;
            }

            primes.push(p);

            let mut multiple = p * p;
            while multiple <= upper_bound {
                composites.insert(multiple);
                multiple += p;
            }

            p += 1;
        }
    }

    primes
}
