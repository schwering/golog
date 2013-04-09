#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <HsFFI.h>

typedef double (*func1_t)(HsStablePtr);
typedef double (*func2_t)(HsStablePtr, int);
typedef double (*func3_t)(HsStablePtr, int, int);

#define SIZE 128

double lru_cache1(int z, func1_t f, HsStablePtr a)
{
	return f(a);
}


double lru_cache2(int z, func2_t f, HsStablePtr a, int b)
{
	return f(a, b);
}


double lru_cache3(int z, func3_t f, HsStablePtr a, int b, int c)
{
	typedef unsigned long long date_t;
	struct entry {
		int z;
		HsStablePtr a;
		int b;
		int c;
		double d;
		date_t date;
	};

	static struct entry buf[SIZE];
	static int size = 0;
	static date_t now = 0;

	++now;

	int oldest_i = 0;
	date_t oldest_date = ULLONG_MAX;

	int i;
	for (i = 0; i < size; ++i) {
		if (buf[i].z == z && buf[i].a == a && buf[i].b == b && buf[i].c == c) {
			buf[i].date = now;
			return buf[i].d;
		}
		if (buf[i].date < oldest_date) {
			oldest_date = buf[i].date;
			oldest_i = i;
		}
	}

	const double d = f(a, b, c); /* this leads to recursive cache lookups! */

	const bool replace = size == SIZE;

	if (replace) {
		if (oldest_date == buf[oldest_i].date) {
			i = oldest_i;
		} else {
			return d;
		}
		if (buf[i].a) {
			hs_free_stable_ptr(buf[i].a);
			buf[i].a = NULL;
		}
	} else {
		i = size;
		++size;
	}

	buf[i].z = z;
	buf[i].a = a;
	buf[i].b = b;
	buf[i].c = c;
	buf[i].d = d;
	buf[i].date = now;
	return d;
}

