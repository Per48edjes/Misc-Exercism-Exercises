#include "reverse_string.h"
#include <stdlib.h>
#include <string.h>

char* reverse(const char* value)
{
    if (value == NULL)
    {
        exit(EXIT_FAILURE);
    }
    size_t length = strlen(value) + 1;
    char* reversed_str = malloc(length * sizeof(char));
    if (reversed_str == NULL)
    {
        exit(EXIT_FAILURE);
    }
    reversed_str[length - 1] = '\0';
    for (size_t src = 0; src < length - 1; src++)
    {
        size_t trgt = length - src - 2;
        reversed_str[trgt] = value[src];
    }
    return reversed_str;
}
