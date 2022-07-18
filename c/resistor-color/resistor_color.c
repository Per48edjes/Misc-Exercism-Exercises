#include "resistor_color.h"

resistor_band_t band_color_array[] = {BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE};

int color_code(resistor_band_t color)
{
	return color;
}

resistor_band_t *colors(void)
{
   return band_color_array;
}

