#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <HsFFI.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/times.h>

typedef unsigned long long date_t;


#ifdef PROFILE_CACHE

static clock_t ticks = 0;

#define TIMER_START \
	struct tms start, end;\
	times(&start);
#define TIMER_END(ticks) \
	times(&end); \
	const clock_t c_start = start.tms_stime + start.tms_cstime; \
	const clock_t c_end = end.tms_stime + end.tms_cstime; \
	ticks += c_end - c_start;

#else

#define TIMER_START
#define TIMER_END(x)

#endif


#define COMMA 		,
#define SEMICOLON 	;

/* Defines a new LRU cache.
 *
 * Name: the resulting function is called <Name>_lru_cache(...)
 * Size: number cache lines
 * ArgTypeList: a macro which takes a delimiter argument and lists the types
 *              of the arguments, e.g., int <delim> float <delim> double
 * ArgNameList: as ArgTypeList, but with variable names instead of types
 * ArgTypeNameList: combination of ArgTypeList and ArgNameList
 * ReturnType: the result of the cache
 * Assign: a macro which takes a cache entry and copies the variables
 * Free: a macro which frees memory allocated by a cache entry (and since it
 *       sets the respective pointer to 0 to remember the deallocation)
 * Equal: a macro which takes a cache entry and returns true if its entries
 *        are equal to the current arguments
 */
#define LRU_CACHE(Name, Size,\
		  ArgTypeList, ArgNameList, ArgTypeNameList, ReturnType,\
		  Assign, Free, Equal)\
struct Name ## _entry {\
	int z;\
	ArgTypeNameList(SEMICOLON);\
	ReturnType result;\
	date_t date;\
};\
typedef ReturnType (*Name ## _func_t)(ArgTypeList(COMMA));\
ReturnType Name ## _lru_cache(int z, Name ## _func_t f, ArgTypeNameList(COMMA))\
{\
	static struct Name ## _entry buf[Size];\
	static int size = 0;\
	static date_t now = 0;\
\
	TIMER_START;\
\
	++now;\
\
	int oldest_i = 0;\
	date_t oldest_date = ULLONG_MAX;\
\
	int i;\
	for (i = 0; i < size; ++i) {\
		if (buf[i].z == z && Equal(&buf[i])) {\
			buf[i].date = now;\
			TIMER_END(ticks);\
			return buf[i].result;\
		}\
		if (buf[i].date < oldest_date) {\
			oldest_date = buf[i].date;\
			oldest_i = i;\
		}\
	}\
\
	const ReturnType result = f(ArgNameList(COMMA));\
\
	const bool replace = size == Size;\
\
	if (replace) {\
		if (oldest_date == buf[oldest_i].date) {\
			i = oldest_i;\
		} else {\
			TIMER_END(ticks);\
			return result;\
		}\
		Free(&buf[i]);\
	} else {\
		i = size;\
		++size;\
	}\
\
	buf[i].z = z;\
	Assign(&buf[i]);\
	buf[i].result = result;\
	buf[i].date = now;\
	TIMER_END(ticks);\
	return result;\
}



#define TYPE_LIST(delim)	HsStablePtr
#define NAME_LIST(delim)	a
#define TYPE_NAME_LIST(delim)	HsStablePtr a
#define RETURN_TYPE		double
#define ASSIGN(e)		(e)->a = a
#define EQUAL(e)		(e)->a == a
#define FREE(e)			if ((e)->a) { hs_free_stable_ptr((e)->a); (e)->a = NULL; }

LRU_CACHE(PtrDbl, 8,
		TYPE_LIST, NAME_LIST, TYPE_NAME_LIST, RETURN_TYPE,
		ASSIGN, FREE, EQUAL)

#undef TYPE_LIST
#undef NAME_LIST
#undef TYPE_NAME_LIST
#undef RETURN_TYPE
#undef ASSIGN
#undef EQUAL
#undef FREE


#define TYPE_LIST(delim)	HsStablePtr delim int
#define NAME_LIST(delim)	a delim b
#define TYPE_NAME_LIST(delim)	HsStablePtr a delim int b
#define RETURN_TYPE		int
#define ASSIGN(e)		(e)->a = a; (e)->b = b
#define EQUAL(e)		(e)->a == a && (e)->b == b
#define FREE(e)			if ((e)->a) { hs_free_stable_ptr((e)->a); (e)->a = NULL; }

LRU_CACHE(PtrIntInt, 8,
		TYPE_LIST, NAME_LIST, TYPE_NAME_LIST, RETURN_TYPE,
		ASSIGN, FREE, EQUAL)

#undef TYPE_LIST
#undef NAME_LIST
#undef TYPE_NAME_LIST
#undef RETURN_TYPE
#undef ASSIGN
#undef EQUAL
#undef FREE


#define TYPE_LIST(delim)	HsStablePtr delim int delim int
#define NAME_LIST(delim)	a delim b delim c
#define TYPE_NAME_LIST(delim)	HsStablePtr a delim int b delim int c
#define RETURN_TYPE		double
#define ASSIGN(e)		(e)->a = a; (e)->b = b; (e)->c = c
#define EQUAL(e)		(e)->a == a && (e)->b == b && (e)->c == c
#define FREE(e)			if ((e)->a) { hs_free_stable_ptr((e)->a); (e)->a = NULL; }

LRU_CACHE(PtrIntIntDbl, 256,
		TYPE_LIST, NAME_LIST, TYPE_NAME_LIST, RETURN_TYPE,
		ASSIGN, FREE, EQUAL)

#undef TYPE_LIST
#undef NAME_LIST
#undef TYPE_NAME_LIST
#undef RETURN_TYPE
#undef ASSIGN
#undef EQUAL
#undef FREE


double memo_cache_time_cost(void)
{
#ifdef PROFILE_CACHE
	return (double) (ticks / sysconf(_SC_CLK_TCK));
#else
	return 0.0;
#endif
}


uint64_t memo_cache_ticks_cost(void)
{
#ifdef PROFILE_CACHE
	return ticks;
#else
	return 0.0;
#endif
}

