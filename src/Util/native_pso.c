/* Particle Swarm Optimization for Double -> Double functions.
 *
 *  * X stands for the current position of a certain particle,
 *
 *  * P stands for the optimal position of a certain particle found thus far,
 *
 *  * V stands for the current velocity of a certain particle,
 *
 *  * G stands for the optimal position of all particles found thus far.
 */


/* --------------------------------------------------------------------- *
 * Random number generator (copied from Mercury copied from Knuth TAOCP) *
 * --------------------------------------------------------------------- */

#include <assert.h>
#include <stdio.h>

typedef unsigned int random_t;

static const unsigned int A = 9301;
static const unsigned int C = 49297;
static const unsigned int M = 233280;

/* Greatest number possible. */
inline static unsigned int rand_max(void)
{
	return M - 1;
}

/* Count of distinct random numbers. */
inline static unsigned int rand_count(void)
{
	return M;
}

inline static void rand_init(unsigned int seed, random_t *r)
{
	*r = seed;
}

/* C % sucks.
 * http://stackoverflow.com/questions/4003232/how-to-code-a-modulo-operator-in-c-c-obj-c-that-handles-negative-numbers */
int mod (int a, int b)
{
	if (b < 0)
		return mod(-a, -b);   
	int ret = a % b;
	if (ret < 0)
		ret += b;
	return ret;
}

inline static unsigned int rand_next(random_t *r)
{
	//int n = mod(((*r) * A) + C, M);
	unsigned int n = (((*r) * A) + C) % M;
	if (n < 0) { n *= -1; }
	*r = n;
	return n;
}

/* In range [lo, hi]. */
inline static unsigned int rand_next_in_range(unsigned int lo, unsigned int hi, random_t *r)
{
	const unsigned int range = hi + 1 - lo;
	const unsigned int n = rand_next(r);
	const unsigned int m = lo + (range * n) / rand_count();
	return m;
}

/* In range [0.0, 1.0]. */
inline static double rand_next_float(random_t *r)
{
	const unsigned int n = rand_next(r);
	const double f = (double) n;
	const double m = (double) rand_max();
	const double g = f / m;
	return g;
}

/* In range [lo, hi]. */
inline static double rand_next_float_in_range(double lo, double hi, random_t *r)
{
	const double f = rand_next_float(r);
	const double g = f * (hi - lo) + lo;
	return g;
}


/* ------------------------------------------------------------- *
 * Particle Swarm Optimization, one-dimensional, only for double *
 * ------------------------------------------------------------- */

#define MAX_PARTICLES 100

typedef double (*objective_t)(double);

struct particle {
	double x;  /* Current position. */
	double v;  /* Current velocity. */
	double p;  /* Best position. */
	double fp; /* f-Value of best position. */
};

inline static void pso_init(
		const int n,		/* Number of particles (in). */
		const double lo,	/* Lower bound (in). */
		const double hi,	/* Higher bound (in). */
		objective_t f,		/* Objective function (in). */
		struct particle *ps,	/* Particles (out). */
		double *g,		/* Global optimum so far (out). */
		double *fg,		/* f-Value of global optimum (out). */
		random_t *r)		/* Random number generator (in/out). */
{
	int i;
	for (i = 0; i < n; ++i) {
		ps[i].x = rand_next_float_in_range(lo, hi, r);
		ps[i].v = rand_next_float_in_range(lo-hi, hi-lo, r);
		ps[i].p = ps[i].x;
		ps[i].fp = f(ps[i].p);
		if (i == 0 || ps[i].fp > *fg) {
			*g = ps[i].p;
			*fg = ps[i].fp;
		}
		assert(lo <= ps[i].x && ps[i].x <= hi);
	}
}


inline static void pso_update(
		const double iw,	/* Inertia weight (in). */
		const double cp,	/* Cognitive param (in). */
		const double sp,	/* Social parameter (in). */
		const double lo,	/* Lower bound (in). */
		const double hi,	/* Higher bound (in). */
		objective_t f,		/* Objective function (in). */
		struct particle *p,	/* Particle (in/out). */
		double *g,		/* Global optimum so far (in/out). */
		double *fg,		/* f-Value of global optimum (in/out). */
		random_t *r)		/* Random number generator (in/out). */
{
	const double rp = rand_next_float_in_range(0.0, 1.0, r);
	const double rg = rand_next_float_in_range(0.0, 1.0, r);
	double fx;
	p->v = iw * p->v + (cp*rp) * (p->p - p->x) + (sp*rg) * ((*g) - p->x);
	p->x = p->x + p->v;
	if (lo <= p->x && p->x <= hi && (fx = f(p->x)) > p->fp) {
		p->p = p->x;
		p->fp = fx;
		if (fx > *fg) {
			*g = p->x;
			*fg = fx;
		}
	}
}

inline static void pso_iteration(
		int n,			/* Number of particles (in). */
		const double iw,	/* Inertia weight (in). */
		const double cp,	/* Cognitive param (in). */
		const double sp,	/* Social parameter (in). */
		const double lo,	/* Lower bound (in). */
		const double hi,	/* Higher bound (in). */
		objective_t f,		/* Objective function (in). */
		struct particle *ps,	/* Particles (in/out). */
		double *g,		/* Global optimum so far (in/out). */
		double *fg,		/* f-Value of global optimum (in/out). */
		random_t *r)		/* Random number generator (in/out). */
{
	int i;
	for (i = 0; i < n; ++i) {
		pso_update(iw, cp, sp, lo, hi, f, &ps[i], g, fg, r);
	}
}

double pso(				/* Returns global optimum (out). */
		const int seed,		/* Random number generator seed (in). */
		const int m,		/* Number of iterations (in). */
		const int n,		/* Number of particles (in). */
		const double iw,	/* Inertia weight (in). */
		const double cp,	/* Cognitive param (in). */
		const double sp,	/* Social parameter (in). */
		const double lo,	/* Lower bound (in). */
		const double hi,	/* Higher bound (in). */
		objective_t f)		/* Objective function. */
{
	struct particle ps[MAX_PARTICLES];
	double g; /* Global optimum. */
	double fg; /* f-Value of global optimum. */
	random_t r;
	int i;

	rand_init(seed, &r);
	pso_init(n, lo, hi, f, ps, &g, &fg, &r);
	for (i = 0; i < m; ++i) {
		pso_iteration(n, iw, cp, sp, lo, hi, f, ps, &g, &fg, &r);
	}
	return g;
}

