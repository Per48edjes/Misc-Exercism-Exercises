#include "resistor_color_trio.h"
#include <math.h>
#include <stddef.h>


resistor_value_t color_code(resistor_band_t bands[static 3])
{
    uint32_t resistance = (10 * bands[0] + bands[1]) * pow(10, bands[2]);
    uint32_t units = resistance >= 1000 ? KILOOHMS : OHMS;
    return (resistor_value_t){ resistance / units, units };
}
