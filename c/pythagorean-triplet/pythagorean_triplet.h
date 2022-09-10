#ifndef PYTHAGOREAN_TRIPLET_H
#define PYTHAGOREAN_TRIPLET_H

#include <stddef.h>

typedef struct{
    int a, b, c;
} triplet_t;

typedef struct
{
    size_t count;
    triplet_t triplets[];
} triplets_t;

triplets_t *triplets_with_sum(int sum);
void free_triplets(triplets_t *triplets);

#endif
