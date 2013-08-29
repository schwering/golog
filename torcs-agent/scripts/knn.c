#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define K		5
#define MIN(x,y) 	((x) < (y) ? (x) : (y))
#define MAX(x,y) 	((x) > (y) ? (x) : (y))
#define N_EXAMPLES 	(1024*1024)

typedef struct {
	/* For speed_x estimation */
	double in_speed_x;
	double in_accel;
	double in_brake;
	double out_speed_x_diff;
	/* For angle estimation */
	double in_steer;
	double out_angle_diff;
} example_t;

static example_t examples[N_EXAMPLES];
static int n_examples = 0;

static double estimate_speed_x_diff(double in_speed_x, double in_accel, double in_brake)
{
	static bool init = false;
	static double min_speed_x = 0.0;
	static double max_speed_x = 0.0;
	double dists[n_examples];
	int nn[K];
	double weights[K], weight_sum;
	double out_speed_x_diff;
	int i;

	if (!init) {
		min_speed_x = examples[0].in_speed_x;
		max_speed_x = examples[0].in_speed_x;
		for (i = 1; i < n_examples; ++i) {
			min_speed_x = MIN(min_speed_x, examples[i].in_speed_x);
			max_speed_x = MAX(max_speed_x, examples[i].in_speed_x);
		}
	}

	for (i = 0; i < n_examples; ++i) {
		const double x = (in_speed_x - examples[i].in_speed_x) / (max_speed_x - min_speed_x);
		const double y = in_accel - examples[i].in_accel;
		const double z = in_brake - examples[i].in_brake;
		dists[i] = sqrt(x*x + y*y + z*z);
	}

	for (i = 0; i < K; ++i) {
		int j, jj = 0;
		for (j = 0; j < n_examples; j++) {
			int l;
			for (l = 0; l < i; l++) {
				if (j == nn[l]) {
					goto next_example;
				}
			}
			if (dists[j] < dists[jj]) {
				jj = j;
			}
next_example:		continue;
		}
		nn[i] = jj;
	}

	weight_sum = 0.0;
	for (i = 0; i < K; ++i) {
		weights[i] = (dists[i] != 0) ? 1 / dists[i] : 10000;
		weight_sum += weights[i];
	}

	out_speed_x_diff = 0.0;
	for (i = 0; i < K; ++i) {
		const example_t *ex = &examples[nn[i]];
		out_speed_x_diff += ex->out_speed_x_diff * weights[i];
	}
	out_speed_x_diff /= weight_sum;

	return out_speed_x_diff;
}

static double estimate_angle_diff(double in_steer)
{
	double dists[n_examples];
	int nn[K];
	double weights[K], weight_sum;
	double out_angle_diff;
	int i;

	for (i = 0; i < n_examples; ++i) {
		const double x = in_steer - examples[i].in_steer;
		dists[i] = fabs(x);
	}

	for (i = 0; i < K; ++i) {
		int j, jj = 0;
		for (j = 0; j < n_examples; j++) {
			int l;
			for (l = 0; l < i; l++) {
				if (j != nn[l]) {
					goto next_example;
				}
			}
			if (dists[j] < dists[jj]) {
				jj = j;
			}
next_example:		continue;
		}
		nn[i] = jj;
	}

	weight_sum = 0.0;
	for (i = 0; i < K; ++i) {
		weights[i] = (dists[i] != 0) ? 1 / dists[i] : 10000;
		weight_sum += weights[i];
	}

	out_angle_diff = 0.0;
	for (i = 0; i < K; ++i) {
		const example_t *ex = &examples[nn[i]];
		out_angle_diff += ex->out_angle_diff * weights[i];
	}
	out_angle_diff /= weight_sum;

	return out_angle_diff;
}

static void load_file(FILE *fp)
{
	while (!feof(fp)) {
		char buf[1024];
		double last_speed_x, speed_x, last_speed_y, speed_y, last_accel, last_brake;
		double last_angle, angle, last_steer;
		int j;

		fgets(buf, sizeof(buf), fp);
		j = sscanf(buf, "%lf %lf %lf %lf %lf %lf %lf %lf %lfn",
				&last_speed_x, &speed_x, 
				&last_speed_y, &speed_y, /* unused */
				&last_angle, &angle, 
				&last_accel, &last_brake,
				&last_steer);
		assert(j == 9);
		last_accel = MAX(0, MIN(1, last_accel));
		last_brake = MAX(0, MIN(1, last_brake));
		last_steer = MAX(-1, MIN(1, last_steer));
		assert(n_examples < N_EXAMPLES);
		examples[n_examples].in_speed_x       = last_speed_x;
		examples[n_examples].in_accel         = last_accel;
		examples[n_examples].in_brake         = last_brake;
		examples[n_examples].out_speed_x_diff = speed_x - last_speed_x;
		examples[n_examples].in_steer         = last_steer;
		examples[n_examples].out_angle_diff   = angle - last_angle;
		++n_examples;
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

int main(int argc, char *argv[])
{
	load_files(argc, argv);

#if 0
	if (1) {
		int i;
		for (i = 0; i < n_examples; ++i) {
			const example_t *ex = &examples[i];
			printf("(%lf | %lf | %lf) -> %lf \t\t"
			       "(%lf) -> %lf\n",
			       ex->in_speed_x, ex->in_accel, ex->in_brake, ex->out_speed_x_diff,
			       ex->in_steer, ex->out_angle_diff);
		}
	}
#endif

#if 1
	if (1) {
		int i;
		for (i = 0; i < n_examples; ++i) {
			const example_t *ex = &examples[i];
			const double est = estimate_speed_x_diff(ex->in_speed_x, ex->in_accel, ex->in_brake);
			const double err = fabs(est - ex->out_speed_x_diff);
			printf("(%lf | %lf | %lf) -> %lf | %lf | %lf\n",
			       ex->in_speed_x, ex->in_accel, ex->in_brake,
			       ex->out_speed_x_diff, est, err);
		}
	}
#endif

#if 1
	if (1) {
		double in_speed_x = 50.0;
		double in_accel = 0.0;
		double in_brake = 0.95;
		for (in_speed_x = 0.0; in_speed_x <= 300.0; in_speed_x += 10.0) {
			for (in_accel = 1.0; in_accel <= 1.0; in_accel += 0.1) {
				in_brake = 1.0 - in_accel;
				//for (in_brake = 0.0; in_brake <= 1.0; in_brake += 0.1) {
					const double est = estimate_speed_x_diff(in_speed_x, in_accel, in_brake);
					printf("(%lf | %lf | %lf) -> %lf\n",
					       in_speed_x, in_accel, in_brake, est);
				//}
			}
		}
	}
#endif

#if 0
	if (1) {
		int i;
		for (i = 0; i < n_examples; ++i) {
			const example_t *ex = &examples[i];
			const double est = estimate_angle_diff(ex->in_steer);
			const double err = fabs(est - ex->out_angle_diff);
			printf("(%lf) -> %lf | %lf | %lf\n",
			       ex->in_steer, ex->out_angle_diff, est, err);
		}
	}
#endif

#if 0
	if (1) {
		double in_steer;
		for (in_steer = -1.0; in_steer <= 1.0; in_steer += 0.05) {
			const double est = estimate_angle_diff(in_steer);
			printf("(%lf) -> %lf\n",
			       in_steer, est/M_PI*180);
		}
	}
#endif

	return 0;
}

