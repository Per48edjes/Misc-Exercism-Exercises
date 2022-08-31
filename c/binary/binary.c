#include "binary.h"
#include <string.h>
#include <math.h>


int convert(const char *input)
{
    int base_10_repr = 0;
    for (int i = strlen(input) - 1, k = 0; i >= 0 ; i--, k++)
    {
        if (!(input[i] == '0' || input[i] == '1') )
        {
            return INVALID;
        }
        base_10_repr += (input[i] == '1' ? 1 : 0) * (int)pow(2, k);
    }
    return base_10_repr;
}
