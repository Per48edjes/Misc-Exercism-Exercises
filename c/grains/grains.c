#include "grains.h"
#define CHESS_SQUARES 64

uint64_t square(uint8_t index)
{
   if (index < 1 || index > CHESS_SQUARES)
   {
      return 0;
   }
   return (uint64_t)1 << (index - 1);
}

uint64_t total(void)
{
   int count = 0;
   for (int i = 1; i <= CHESS_SQUARES; ++i)
   {
      count += square(i);
   }
   return count;
}
