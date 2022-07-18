#include "hamming.h"
#include <string.h>

int compute(const char *lhs, const char *rhs)
{
	int lhs_len = strlen(lhs);
	int rhs_len = strlen(rhs);

	if (lhs_len != rhs_len)
	{
		return -1;
	}

	int h_distance = 0;
	for (int i = 0; i < lhs_len; ++i) {
		(lhs[i] != rhs[i]) ? ++h_distance : 0;
	}
	return h_distance;
}
