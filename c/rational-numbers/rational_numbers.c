#include "rational_numbers.h"
#include <math.h>

typedef rational_t binary_func(rational_t r1, rational_t r2);
typedef binary_func *BinaryFunction;

static const rational_t one = {1, 1};


static inline int max(int a, int b)
{
    return ((a) > (b) ? (a) : (b));
}

static int16_t gcd(int16_t a, int16_t b)
{
    if (a == 0 || b == 0)
    {
        return max(a, b);
    }
    return a > b ? gcd(b, a % b) : gcd(a, b % a);
}

static rational_t negate(rational_t r)
{
    return reduce((rational_t){ -1 * r.numerator, r.denominator });
}

static rational_t mult_inverse(rational_t r)
{
    return reduce((rational_t){ r.denominator, r.numerator });
}

static rational_t repeater(
    BinaryFunction func, rational_t r, int16_t n)
{
    if (n == 1)
    {
        return reduce(r);
    }
    return reduce(func(r, repeater(func, r, n - 1)));
}

rational_t reduce(rational_t r)
{
    int16_t _gcd = gcd(abs(r.numerator), abs(r.denominator));
    rational_t s = { r.numerator / _gcd, r.denominator / _gcd };
    if (s.denominator < 0)
    {
        s.numerator *= -1;
        s.denominator *= -1;
    }
    return s;
}

rational_t absolute(rational_t r)
{
    return reduce((rational_t){ abs(r.numerator), abs(r.denominator) });
}

rational_t add(rational_t r1, rational_t r2)
{
    return reduce((rational_t) { (r1.numerator * r2.denominator) +
                                 (r2.numerator * r1.denominator),
                                 (r1.denominator * r2.denominator) });
}

rational_t subtract(rational_t r1, rational_t r2)
{
    return reduce(add(r1, negate(r2)));
}

rational_t multiply(rational_t r1, rational_t r2)
{
    return reduce((rational_t){ r1.numerator *r2.numerator,
                                r1.denominator *r2.denominator });
}

rational_t divide(rational_t r1, rational_t r2)
{
    return reduce(multiply(r1, mult_inverse(r2)));
}

rational_t exp_rational(rational_t r, int16_t n)
{
    if (n == 0)
    {
        return one;
    }
    rational_t s = repeater(&multiply, r, abs(n));
    return reduce(n > 0 ? s : mult_inverse(s));
}

float exp_real(uint16_t x, rational_t r)
{
    if (r.numerator == 0)
    {
        return 1.0;
    }
    return powf(x, (float)r.numerator / r.denominator);
}
