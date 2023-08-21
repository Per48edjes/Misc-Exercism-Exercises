#include "triangle.h"
#include "stdbool.h"
#include <stdlib.h>

static int compare_doubles(const void* a, const void* b)
{
    double diff = (*(double*)a) - (*(double*)b);
    if (diff < 0)
        return -1;
    if (diff > 0)
        return 1;
    return 0;
}

static bool is_triangle(triangle_t sides)
{
    double side_arr[] = {sides.a, sides.b, sides.c};
    qsort(side_arr, 3, sizeof(double), compare_doubles);
    return side_arr[0] + side_arr[1] > side_arr[2];
}

// All sides are the same length
bool is_equilateral(triangle_t sides)
{
    return is_triangle(sides) &&
           (sides.a == sides.b && sides.b == sides.c && sides.a == sides.c);
}

// At least two sides are the same
bool is_isosceles(triangle_t sides)
{
    return is_triangle(sides) &&
           (sides.a == sides.b || sides.b == sides.c || sides.a == sides.c);
}

// All side lengths are unique
bool is_scalene(triangle_t sides)
{
    return is_triangle(sides) && !is_isosceles(sides);
}
