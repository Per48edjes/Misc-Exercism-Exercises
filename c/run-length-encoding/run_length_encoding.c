#include "run_length_encoding.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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

//CALLBACKS <3

static void encoded_length_callback(void* env, size_t count, char letter)
{
    (void)letter;
    size_t* compressed_length = env;
    *compressed_length += count == 1 ? 1 : (decimal_int_to_strlen(count) + 1);
}

static void encoded_writer_callback(void* env, size_t count, char letter)
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

static void decoded_length_callback(void* env, size_t count, char letter) {
    
    (void)letter;
    size_t* decompressed_length = env;
    *decompressed_length += count;
}

static void decoded_writer_callback(void* env, size_t count, char letter)
{
    char** writer_head = env;
    for(char* bound = *writer_head + count;*writer_head < bound; (*writer_head)++) {
        **writer_head = letter;
    }
}

//VISITORS <3

static void decoded_run_length_visitor(const char* text, void* env,
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

static void encoded_run_length_visitor(
    const char* encoded_text, void* env,
    void (*callback)(void* env, size_t count,char letter)
    ) {
    size_t count = 0;
    for (const char* ptr = encoded_text; *ptr; ptr++)
    {
        char letter = *ptr;
        if (isdigit(letter)) {
            count = count*10 + (letter - '0');
        }
        else
        {
            callback(env, count != 0 ? count : 1, letter);
            count = 0;
        }
    }
}

//THE COMPRESSION STUFF <3

char* encode_or_decode(
    const char* input_text,
    void (*visitor)(const char*, void*, void(*callback)(void*, size_t, char)),
    void (*length_callback)(void*, size_t, char),
    void (*writer_callback)(void*, size_t, char)
    )
{
    if (input_text == NULL)
    {
        exit(EXIT_FAILURE);
    }
    char* output_text;
    if (input_text[0] == '\0')
    {
        output_text = calloc(1, sizeof(char));
    }
    else
    {
        size_t length = 0;
        visitor(input_text, &length,
                           length_callback);
        output_text = calloc((1 + length), sizeof(char));
        char* writer_head = output_text;
        visitor(input_text, &writer_head, writer_callback);
    }
    return output_text;
}

char* encode(const char* text) {
    return encode_or_decode(
        text,
        decoded_run_length_visitor,
        encoded_length_callback,
        encoded_writer_callback
    );
}

char* decode(const char* text) {
    return encode_or_decode(
        text,
        encoded_run_length_visitor,
        decoded_length_callback,
        decoded_writer_callback
    );
}
