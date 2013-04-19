#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <HsFFI.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/times.h>

typedef double (*func1_t)(HsStablePtr);
typedef double (*func2_t)(HsStablePtr, int);
typedef double (*func3_t)(HsStablePtr, int, int);

#define SIZE1 64
#define SIZE2 64
#define SIZE3 256

static clock_t ticks = 0;

#define TIMER_START \
	struct tms start, end;\
	times(&start);
#define TIMER_END(ticks) \
	times(&end); \
	const clock_t c_start = start.tms_stime + start.tms_cstime; \
	const clock_t c_end = end.tms_stime + end.tms_cstime; \
	ticks += c_end - c_start;


double lru_cache1(int z, func1_t f, HsStablePtr a)
{
	typedef unsigned long long date_t;
	struct entry {
		int z;
		HsStablePtr a;
		double b;
		date_t date;
	};

	static struct entry buf[SIZE1];
	static int size = 0;
	static date_t now = 0;

	//TIMER_START;

	++now;

	int oldest_i = 0;
	date_t oldest_date = ULLONG_MAX;

	int i;
	for (i = 0; i < size; ++i) {
		if (buf[i].z == z && buf[i].a == a) {
			buf[i].date = now;
			//TIMER_END(ticks);
			return buf[i].b;
		}
		if (buf[i].date < oldest_date) {
			oldest_date = buf[i].date;
			oldest_i = i;
		}
	}

	const double b = f(a); /* this leads to recursive cache lookups! */

	const bool replace = size == SIZE1;

	if (replace) {
		if (oldest_date == buf[oldest_i].date) {
			i = oldest_i;
		} else {
			//TIMER_END(ticks);
			return b;
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
	buf[i].date = now;
	//TIMER_END(ticks);
	return b;
}


double lru_cache2(int z, func2_t f, HsStablePtr a, int b)
{
	typedef unsigned long long date_t;
	struct entry {
		int z;
		HsStablePtr a;
		int b;
		double c;
		date_t date;
	};

	static struct entry buf[SIZE2];
	static int size = 0;
	static date_t now = 0;

	//TIMER_START;

	++now;

	int oldest_i = 0;
	date_t oldest_date = ULLONG_MAX;

	int i;
	for (i = 0; i < size; ++i) {
		if (buf[i].z == z && buf[i].a == a && buf[i].b == b) {
			buf[i].date = now;
			//TIMER_END(ticks);
			return buf[i].c;
		}
		if (buf[i].date < oldest_date) {
			oldest_date = buf[i].date;
			oldest_i = i;
		}
	}

	const double c = f(a, b); /* this leads to recursive cache lookups! */

	const bool replace = size == SIZE2;

	if (replace) {
		if (oldest_date == buf[oldest_i].date) {
			i = oldest_i;
		} else {
			//TIMER_END(ticks);
			return c;
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
	buf[i].date = now;
	//TIMER_END(ticks);
	return c;
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

	static struct entry buf[SIZE3];
	static int size = 0;
	static date_t now = 0;

	//TIMER_START;

	++now;

	int oldest_i = 0;
	date_t oldest_date = ULLONG_MAX;

	int i;
	for (i = 0; i < size; ++i) {
		if (buf[i].z == z && buf[i].a == a && buf[i].b == b && buf[i].c == c) {
			buf[i].date = now;
			//TIMER_END(ticks);
			return buf[i].d;
		}
		if (buf[i].date < oldest_date) {
			oldest_date = buf[i].date;
			oldest_i = i;
		}
	}

	const double d = f(a, b, c); /* this leads to recursive cache lookups! */

	const bool replace = size == SIZE3;

	if (replace) {
		if (oldest_date == buf[oldest_i].date) {
			i = oldest_i;
		} else {
			//TIMER_END(ticks);
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
	//TIMER_END(ticks);
	return d;
}


double memo_cache_time_cost(void)
{
	return (double) (ticks / sysconf(_SC_CLK_TCK));
}


uint64_t memo_cache_ticks_cost(void)
{
	return ticks;
}

