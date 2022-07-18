#include "circular_buffer.h"
#include <stdlib.h>
#include <errno.h>


circular_buffer_t *new_circular_buffer(uint16_t capacity)
{
    buffer_value_t *elements =
        (buffer_value_t *)malloc(capacity * sizeof(buffer_value_t));
    circular_buffer_t *buffer =
        (circular_buffer_t *)malloc(sizeof(circular_buffer_t));

    if (!(elements && buffer))
    {
        return NULL;
    }

    buffer->capacity = capacity;
    buffer->elements = elements;

    clear_buffer(buffer);

    return buffer;
}


void delete_buffer(circular_buffer_t *buffer)
{
    free(buffer->elements);
    free(buffer);
}


void clear_buffer(circular_buffer_t *buffer)
{
    buffer->filled_slots = 0;
    buffer->read_from_idx = 0;
    buffer->write_to_idx = 0;
}


int16_t read(circular_buffer_t *buffer, buffer_value_t *read_value)
{
    if (!buffer || !read_value || buffer->filled_slots == 0)
    {
        errno = ENODATA;
        return EXIT_FAILURE;
    }

    *read_value = buffer->elements[buffer->read_from_idx];
    buffer->read_from_idx = (buffer->read_from_idx + 1) % buffer->capacity;
    buffer->filled_slots--;

    return EXIT_SUCCESS;
}


int16_t write(circular_buffer_t *buffer, buffer_value_t write_value)
{
    if (buffer->filled_slots == buffer->capacity)
    {
        errno = ENOBUFS;
        return EXIT_FAILURE;
    }

    return overwrite(buffer, write_value);
}


int16_t overwrite(circular_buffer_t *buffer, buffer_value_t write_value)
{
    buffer->elements[buffer->write_to_idx] = write_value;
    buffer->write_to_idx = (buffer->write_to_idx + 1) % buffer->capacity;

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
