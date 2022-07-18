#include "isogram.h"
#include <string.h>
#include <stdbool.h>
#include <ctype.h>


bool is_isogram(const char phrase[])
{
   // Degenerate cases
   if (phrase == NULL)
   {
      return false;
   }

   // O(n^2) time complexity; O(1) space complexity
   int phrase_len = strlen(phrase);
   for (int i = 0; i < phrase_len; ++i)
   {
      for (int j = i + 1; j < phrase_len; ++j)
      {
         if ((tolower(phrase[i]) == tolower(phrase[j])) && isalpha(phrase[i]) && isalpha(phrase[j]))
         {
            return false;
         }
      }
   }

   return true;
}
