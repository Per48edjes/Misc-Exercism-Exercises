#include "sieve.h"
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>


uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes)
{
   if (!primes)
   {
      return EXIT_FAILURE;
   }

   /* Initialize boolean mask array */
   bool is_prime[limit + 1];
   memset(is_prime, true, (limit + 1)*sizeof(*is_prime));

   uint32_t i = 0;
   for (uint32_t p = 2, k; p <= limit && i < max_primes; i++)
   {
      /* Mark multiples of `p` as composite */
      k = p;
      while ((p * k) <= limit)
      {
         is_prime[p * (k++)] = false;
      }

      /* Add prime `p` to `primes` array */
      primes[i] = p++;

      /* Find the next prime `p` */
      while (!is_prime[p])
      {
         ++p;
      }
   }
   return limit < 2 ? 0 : i;
}
