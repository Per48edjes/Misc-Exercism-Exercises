#include "prime_factors.h"
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

static inline uint64_t max_possible_divisor(uint64_t n)
{
    return (uint64_t)floor(sqrt((double)n));
}

static uint64_t next_prime_divisor(uint64_t n, uint64_t k)
{
    for (uint64_t k_bound = max_possible_divisor(n); k <= k_bound; k++)
    {
        if (n % k == 0)
        {
            return k;
        }
    }
    return n;
}

size_t find_factors(uint64_t n, uint64_t factors[static MAXFACTORS])
{
    size_t len = 0;
    uint64_t k = 2;
    while (n > 1 && len < MAXFACTORS)
    {
        factors[len++] = (k = next_prime_divisor(n, k));
        n /= k;
    }
    return len;
}
