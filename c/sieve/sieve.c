#include "sieve.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>


uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes)
{
    /* Initialize boolean mask array */
    bool *is_prime = malloc((limit + 1) * sizeof(bool));
    if (!is_prime)
    {
        return 0;
    }
    memset(is_prime, true, (limit + 1) * sizeof(bool));

    uint32_t primes_idx = 0;
    for (uint32_t p = 2; (p <= limit) && (primes_idx < max_primes); primes_idx++)
    {
        /* Mark multiples of `p` as composite */
        for (uint32_t k = p; (p * k) <= limit; k++)
        {
            is_prime[p * k] = false;
        }

        /* Add prime `p` to `primes` array */
        primes[primes_idx] = p++;

        /* Find the next prime `p` */
        while ((p <= limit) && (!is_prime[p]))
        {
            ++p;
        }
    }
    free(is_prime);
    return primes_idx;
}
