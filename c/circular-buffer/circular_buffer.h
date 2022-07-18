#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H

#include <stddef.h>
#include <stdint.h>

typedef int buffer_value_t;

typedef struct circular_buffer_s circular_buffer_t;

circular_buffer_t *new_circular_buffer(size_t capacity);
void delete_buffer(circular_buffer_t *buffer);
void clear_buffer(circular_buffer_t *buffer);

size_t read(circular_buffer_t *buffer, buffer_value_t *read_value);
size_t write(circular_buffer_t *buffer, buffer_value_t write_value);
size_t overwrite(circular_buffer_t *buffer, buffer_value_t write_value);

#endif
