#include "run_length_encoding.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static size_t decimal_int_to_strlen(size_t n)
{
    size_t digits = 1;
    while (n /= 10)
    {
        digits++;
    }
    return digits;
}

static size_t decimal_int_to_str(size_t n, char* position)
{
    size_t count = 0;
    for (char* ptr = position + decimal_int_to_strlen(n) - 1; ptr >= position;
         ptr--)
    {
        *ptr = (n % 10) + '0';
        count++;
        n /= 10;
    }
    return count;
}

static void compressed_length_callback(void* env, size_t count, char letter)
{
    (void)letter;
    size_t* compressed_length = env;
    *compressed_length += count == 1 ? 1 : (decimal_int_to_strlen(count) + 1);
}

static void writer_callback(void* env, size_t count, char letter)
{
    char** writer_head = env;
    if (count > 1)
    {
        size_t num_chars_written = decimal_int_to_str(count, *writer_head);
        *writer_head += num_chars_written;
    }
    **writer_head = letter;
    (*writer_head)++;
}

static void run_length_visitor(const char* text, void* env,
                               void (*callback)(void* env, size_t count,
                                                char letter))
{
    char last_seen = text[0];
    size_t count = 1;
    for (const char* ptr = text + 1; *ptr; ptr++)
    {
        char letter = *ptr;
        if (letter == last_seen)
        {
            count++;
        }
        else
        {
            callback(env, count, last_seen);
            last_seen = letter;
            count = 1;
        }
    }
    callback(env, count, last_seen);
}

char* encode(const char* text)
{
    if (text == NULL)
    {
        exit(EXIT_FAILURE);
    }
    char* encoded_text;
    if (text[0] == '\0')
    {
        encoded_text = calloc(1, sizeof(char));
    }
    else
    {
        size_t compressed_length = 0;
        run_length_visitor(text, &compressed_length,
                           compressed_length_callback);
        encoded_text = calloc((1 + compressed_length), sizeof(char));
        char* writer_head = encoded_text;
        run_length_visitor(text, &writer_head, writer_callback);
    }
    return encoded_text;
}

char* decode(const char* data);
