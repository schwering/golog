#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
	double last_speed_x;
	double speed_x;
	double last_speed_y;
	double speed_y;
	double last_angle;
	double angle;
	double last_accel;
	double last_brake;
	double last_steer;
} example_t;

typedef struct {
	double last_speed_x;
	double last_speed_y;
	double last_angle;
	double last_accel;
	double last_brake;
	double last_steer;
} weights_t;

#define N_EXAMPLES (1024*1024)

static example_t examples[N_EXAMPLES];
static int n_examples = 0;

static inline double step0(double in)
{
	return in >= 0.0 ? 1.0 : 0.0;
}

static const double learning_rate = 0.25;

static void init_weights(weights_t *w)
{
	memset((void *) w, 0, sizeof(*w));
}

static double expected_output(const example_t *ex)
{
	return ex->angle - ex->last_angle;
}

static double observe_output(const weights_t *w, const example_t *ex)
{
	double weighted_input = 0.0;
	weighted_input += w->last_speed_x * ex->last_speed_x;
	weighted_input += w->last_speed_y * ex->last_speed_y;
	weighted_input += w->last_angle * ex->last_angle;
	weighted_input += w->last_accel * ex->last_accel;
	weighted_input += w->last_brake * ex->last_brake;
	weighted_input += w->last_steer * ex->last_steer;
	return step0(weighted_input);
}

static void update_weights(weights_t *w, const example_t *ex, double o, double t)
{
	const double error = t - o;
	w->last_speed_x += learning_rate * ex->last_speed_x * error;
	w->last_speed_y += learning_rate * ex->last_speed_y * error;
	w->last_angle += learning_rate * ex->last_angle * error;
	w->last_accel += learning_rate * ex->last_accel * error;
	w->last_brake += learning_rate * ex->last_brake * error;
	w->last_steer += learning_rate * ex->last_steer * error;
}

static void epoch(weights_t *w)
{
	int i;

	for (i = 0; i < n_examples; ++i) {
		const example_t *ex = &examples[i];
		const double t = expected_output(ex);
		const double o = observe_output(w, ex);
		update_weights(w, ex, o, t);
	}
}

static void epochs(int n, weights_t *w)
{
	int i;

	init_weights(w);
	for (i = 0; i < n; ++i) {
		epoch(w);
	}
}

static void load_file(FILE *fp)
{
	while (!feof(fp)) {
		example_t ex;
		int j;

		j = fscanf(fp, "%lf %lf %lf %lf %lf %lf %lf %lf %lf\n",
				&ex.last_speed_x, &ex.speed_x, 
				&ex.last_speed_y, &ex.speed_y, 
				&ex.last_angle, &ex.angle, 
				&ex.last_accel, &ex.last_brake,
				&ex.last_steer);
		assert(j == 9);
		assert(n_examples < N_EXAMPLES);
		memcpy((void *) &examples[n_examples++], (void *) &ex, sizeof(ex));
	}
}

static void load_files(int argc, char *argv[])
{
	int i;

	for (i = 1; i < argc; ++i) {
		FILE *fp = fopen(argv[i], "r");
		assert(fp != NULL);
		load_file(fp);
		fclose(fp);
	}
}

static void eval(weights_t *w)
{
	int i;
	double err;

	err = 0.0;
	for (i = 0; i < n_examples; ++i) {
		example_t *ex = &examples[i];
		const double t = expected_output(ex);
		const double o = observe_output(w, ex);
		const double e = t - o;
		printf("t = %3.8lf  o = %3.8lf  e = %3.8lf\n", t, o, e);
		err += fabs(e);
	}
	printf("mean error: %lf\n", err / n_examples);
}

int main(int argc, char *argv[])
{
	weights_t w;

	load_files(argc, argv);

	init_weights(&w);
	epochs(5, &w);

	eval(&w);

	printf("last_speed_x = %lf\n", w.last_speed_x);
	printf("last_speed_y = %lf\n", w.last_speed_y);
	printf("last_angle   = %lf\n", w.last_angle);
	printf("last_accel   = %lf\n", w.last_accel);
	printf("last_brake   = %lf\n", w.last_brake);
	printf("last_steer   = %lf\n", w.last_steer);

	return 0;
}

