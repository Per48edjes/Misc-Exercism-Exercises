#include <stddef.h>
#include <stdint.h>

typedef int buffer_value_t;
typedef struct
{
    int16_t capacity;
    buffer_value_t *elements;
    buffer_value_t read_from_idx;
    buffer_value_t write_to_idx;
    uint16_t filled_slots;
} circular_buffer_t;

circular_buffer_t *new_circular_buffer(uint16_t capacity);
void delete_buffer(circular_buffer_t *buffer);
void clear_buffer(circular_buffer_t *buffer);

int16_t read(circular_buffer_t *buffer, buffer_value_t *read_value);
int16_t write(circular_buffer_t *buffer, buffer_value_t write_value);
int16_t overwrite(circular_buffer_t *buffer, buffer_value_t write_value);
