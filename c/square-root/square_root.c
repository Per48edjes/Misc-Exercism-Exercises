#include "square_root.h"
#include <math.h>

#define TOLERANCE 1e-10

unsigned int square_root(unsigned int a)
{
    double guess = 0.5 * (1 + a);
    while (fabs(a - (guess * guess)) > TOLERANCE)
    {
        /* Implementing Newton's Method; converges quadratically */
        guess = (guess + a / guess) / 2;
    }
    return round(guess);
}
