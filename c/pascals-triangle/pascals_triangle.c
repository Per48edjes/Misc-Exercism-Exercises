#include "pascals_triangle.h"
#include <stdlib.h>
#include <string.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))

static void fill_triangle(uint8_t **triangle, size_t rows);
static void fill_triangle_row(uint8_t **triangle, size_t rows);


static void fill_triangle(uint8_t **triangle, size_t rows)
{
    /* Base cases */
    if (rows <= 0)
    {
        return;
    }
    if (rows == 1)
    {
        triangle[0][0] = 1;
        return;
    }

    /* Recursive case */
    else
    {
        fill_triangle(triangle, rows - 1);
        fill_triangle_row(triangle, rows);
    }
}

static void fill_triangle_row(uint8_t **triangle, size_t row)
{
    triangle[row - 1][0] = 1;
    for (size_t col = 1; col < row; ++col)
    {
        triangle[row - 1][col] = triangle[row - 2][col - 1] + triangle[row - 2][col];
    }
}

void free_triangle(uint8_t **triangle, size_t rows)
{
    if (!triangle)
    {
        return;
    }

    /* Deallocate rows; deallocate array */
    for (size_t i = 0; i < rows; ++i)
    {
        if (triangle[i])
        {
            free(triangle[i]);
            triangle[i] = NULL;
        }
    }
    free(triangle);
    triangle = NULL;
}

uint8_t **create_triangle(size_t rows)
{
    /* Allocate array of row pointers */
    uint8_t **triangle = malloc(sizeof(size_t) * (MAX(1, rows)));
    if (!triangle)
    {
        return NULL;
    }

    /* Allocate row pointers; assign to array */
    for (size_t i = 0; i < MAX(1, rows); ++i)
    {
        triangle[i] = calloc(MAX(1, rows), sizeof(size_t));
        if (!triangle[i])
        {
            free_triangle(triangle, rows);
            return NULL;
        }
    }
    fill_triangle(triangle, rows);
    return triangle;
}
