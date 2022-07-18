#include "circular_buffer.h"
#include <stdlib.h>
#include <errno.h>

struct circular_buffer_s
{
    size_t capacity;
    size_t read_from_idx;
    size_t write_to_idx;
    size_t filled_slots;
    buffer_value_t elements[];
};

static void move_to_next_idx(size_t *idx, size_t capacity);

circular_buffer_t *new_circular_buffer(size_t capacity)
{
    circular_buffer_t *buffer = malloc(
        sizeof(circular_buffer_t) + capacity * sizeof(buffer_value_t));

    if (!buffer)
    {
        return NULL;
    }

    buffer->capacity = capacity;

    clear_buffer(buffer);

    return buffer;
}


void delete_buffer(circular_buffer_t *buffer)
{
    free(buffer);
}


void clear_buffer(circular_buffer_t *buffer)
{
    buffer->filled_slots = 0;
    buffer->read_from_idx = 0;
    buffer->write_to_idx = 0;
}


size_t read(circular_buffer_t *buffer, buffer_value_t *read_value)
{
    if (!buffer || !read_value || buffer->filled_slots == 0)
    {
        errno = ENODATA;
        return EXIT_FAILURE;
    }

    *read_value = buffer->elements[buffer->read_from_idx];
    move_to_next_idx(&buffer->read_from_idx, buffer->capacity);
    buffer->filled_slots--;

    return EXIT_SUCCESS;
}


size_t write(circular_buffer_t *buffer, buffer_value_t write_value)
{
    if (!buffer)
    {
        errno = ENODATA;
        return EXIT_FAILURE;
    }

    if (buffer->filled_slots == buffer->capacity)
    {
        errno = ENOBUFS;
        return EXIT_FAILURE;
    }

    return overwrite(buffer, write_value);
}


size_t overwrite(circular_buffer_t *buffer, buffer_value_t write_value)
{
    if (!buffer)
    {
        errno = ENODATA;
        return EXIT_FAILURE;
    }

    buffer->elements[buffer->write_to_idx] = write_value;
    move_to_next_idx(&buffer->write_to_idx, buffer->capacity);

    if (buffer->filled_slots < buffer->capacity)
    {
        buffer->filled_slots++;
    }
    else
    {
        buffer->read_from_idx = buffer->write_to_idx;
    }

    return EXIT_SUCCESS;
}


void move_to_next_idx(size_t *idx, size_t capacity)
{
    *idx = (*idx + 1) % capacity;
}
