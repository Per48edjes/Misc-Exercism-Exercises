#include "resistor_color_trio.h"
#include <math.h>
#include <stddef.h>


resistor_value_t color_code(resistor_band_t bands[])
{
    size_t resistance = (10 * bands[0] + bands[1]) * pow(10, bands[2]);
    return (resistor_value_t){
               resistance / 1000 > 0 ? resistance / 1000 : resistance,
               resistance > 1000 ? KILOOHMS : OHMS
    };
}
