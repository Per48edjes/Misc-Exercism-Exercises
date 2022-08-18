#include "rational_numbers.h"
#include <math.h>

#define MAX(A, B) ((A) > (B) ? (A) : (B))

static const rational_t one = {1, 1};

static int16_t gcd(int16_t a, int16_t b);
static rational_t negate(rational_t r);
static rational_t mult_inverse(rational_t r);
static rational_t repeater(rational_t ( *func )(
                               rational_t r1,
                               rational_t r2),
                           rational_t    r,
                           int16_t       n);

static int16_t gcd(int16_t a, int16_t b)
{
    if (a == 0 || b == 0)
    {
        return MAX(a, b);
    }
    return a > b ? gcd(b, a % b) : gcd(a, b % a);
}

static rational_t negate(rational_t r)
{
    rational_t s = { -1 * r.numerator, r.denominator };
    return reduce(s);
}

static rational_t mult_inverse(rational_t r)
{
    rational_t s = { r.denominator, r.numerator };
    return reduce(s);
}

static rational_t repeater(rational_t ( *func )(
                               rational_t r1,
                               rational_t r2),
                           rational_t    r,
                           int16_t       n)
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
    rational_t s = { abs(r.numerator), abs(r.denominator) };
    return reduce(s);
}

rational_t add(rational_t r1, rational_t r2)
{
    rational_t r =
    {
        (r1.numerator * r2.denominator) + (r2.numerator * r1.denominator),
        (r1.denominator * r2.denominator)
    };
    return reduce(r);
}

rational_t subtract(rational_t r1, rational_t r2)
{
    return reduce(add(r1, negate(r2)));
}

rational_t multiply(rational_t r1, rational_t r2)
{
    rational_t r =
    {
        r1.numerator * r2.numerator, r1.denominator * r2.denominator
    };
    return reduce(r);
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
    return pow(pow(x, r.numerator), 1.0 / r.denominator);
}
