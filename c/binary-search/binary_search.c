#include "binary_search.h"


int *binary_search(int value, const int *arr, size_t length)
{
    /* Base cases */
    if (length == 0)
    {
        return NULL;
    }

    /* Recursive cases */
    size_t i = length / 2;
    if (value == arr[i])
    {
        return (int *)&arr[i];
    }
    return (value > arr[i]) ?
           binary_search(value, &arr[i + 1], length - i - 1) :
           binary_search(value, arr, i);
}
