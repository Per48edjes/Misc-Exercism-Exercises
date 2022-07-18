#include "armstrong_numbers.h"
#include <math.h>
#include <stdio.h>


bool is_armstrong_number(int candidate)
{
	/* Count the number of digits */
	int num_digits =  candidate > 0 ? log10(candidate) + 1 : 0;

	/* Check if Armstrong number by performing calculation */
	int sum_power = 0;
	int _candidate = candidate;
	while (_candidate > 0)
	{
		sum_power += pow(_candidate % 10, num_digits);
		_candidate /= 10;
	}

	return (sum_power == candidate);
}
