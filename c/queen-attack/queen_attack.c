#include "queen_attack.h"
#include <stdbool.h>

#define BOARD_LEN 8

static bool is_legal_position(position_t queen_1, position_t queen_2)
{
   return (
      /* Not the same queen */
      !((queen_1.row == queen_2.row) && (queen_1.column == queen_2.column)) &&
      /* On the board */
      (queen_1.column < BOARD_LEN) &&
      (queen_2.column < BOARD_LEN) && 
      (queen_1.row < BOARD_LEN) &&
      (queen_2.row < BOARD_LEN)
   );
}

attack_status_t can_attack(position_t queen_1, position_t queen_2)
{
   if (is_legal_position(queen_1, queen_2))
   {
      if 
         (
            /* Shared laterals */
            (queen_1.column == queen_2.column) ||
            (queen_1.row == queen_2.row) ||
            /* Shared diagonals */
            (queen_1.row + queen_1.column == queen_2.row + queen_2.column) ||
            (queen_1.row + (BOARD_LEN - queen_1.column) == queen_2.row + (BOARD_LEN - queen_2.column))
         )
      {
         return CAN_ATTACK;
      }
      else 
      {
         return CAN_NOT_ATTACK;
      }
   }
   else
   {
      return INVALID_POSITION;
   }
}
