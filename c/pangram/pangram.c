#include "pangram.h"
#include <ctype.h>
#include <string.h>

#define ALPHABET_LEN 26

bool is_pangram(const char *sentence)
{
    if (!sentence)
    {
        return 0;
    }

    int alphabet_hashmap[ALPHABET_LEN] = { 0 };
    int unused_letters = ALPHABET_LEN;
    const int sentence_len = strlen(sentence);

    for (int i = 0, l; i < sentence_len; ++i)
    {
        if (isalpha(sentence[i]))
        {
            l = toupper(sentence[i]) - 'A';
            if (!alphabet_hashmap[l])
            {
                alphabet_hashmap[l]++;
                unused_letters--;
            }
        }
    }

    if (unused_letters == 0)
    {
        return true;
    }
    return false;
}
