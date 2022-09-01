#include "binary.h"


int convert(const char *input)
{
    int base_10_repr = 0;
    while (*input)
    {
        if (!(*input == '0' || *input == '1'))
        {
            return INVALID;
        }
        base_10_repr = (base_10_repr << 1) + (*(input++) == '1');
    }
    return base_10_repr;
}
