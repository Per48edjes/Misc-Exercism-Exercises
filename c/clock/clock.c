#include "clock.h"
#include <stdio.h>
#include <string.h>

#define MINUTES_PER_HOUR 60
#define HOURS_PER_DAY 24 

typedef struct
{
    int hour;
    int minute;
} basictime_t;

static inline int pmod(int n, int m)
{
   /* Mimic Python's modulo operator */
   return ((n % m) + m) % m;
}

static basictime_t clock_to_time(clock_t clock)
{
   int hour, minute;
   sscanf(clock.text, "%d:%d", &hour, &minute);
   return (basictime_t){ hour, minute };
}

clock_t clock_create(int hour, int minute)
{
    /* Initialize clock */
    clock_t clock;

    /* Handle rollover */
    int total_minutes = minute + (MINUTES_PER_HOUR * hour);
    hour = pmod(total_minutes, MINUTES_PER_HOUR * HOURS_PER_DAY) / MINUTES_PER_HOUR;
    minute = pmod(minute, MINUTES_PER_HOUR);

    /* Assemble string representation */
    sprintf(clock.text, "%02d:%02d", hour, minute);

    return clock;
}

clock_t clock_add(clock_t clock, int minute_add)
{
   basictime_t time = clock_to_time(clock);
   return clock_create(time.hour, time.minute + minute_add);
}

clock_t clock_subtract(clock_t clock, int minute_subtract)
{
   basictime_t time = clock_to_time(clock);
   return clock_create(time.hour, time.minute - minute_subtract);
}

bool clock_is_equal(clock_t a, clock_t b)
{
   return strcmp(a.text, b.text) == 0; 
}
