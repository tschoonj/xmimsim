#include "xmi_spline.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>

int main(int argc, char *argv[]) {

	double results[] = {
		100, 99.7732, 99.2571, 98.1625, 96.2, 93.0804, 88.5143, 82.2125, 73.8857, 63.2446,
		50, 34.0518, 16.0571, -3.1375, -22.6857, -41.7411, -59.4571, -74.9875, -87.4857, -96.1054,
		-100, -98.5804, -92.2857, -81.8125, -67.8571, -51.1161, -32.2857, -12.0625, 8.85714, 29.7768,
		50, 68.9696, 86.6857, 103.287, 118.914, 133.705, 147.8, 161.338, 174.457, 187.298, 200.0
	};

	double x[5] = {0, 1, 2, 3, 4};
	double y[5] = {100, 50, -100, 50, 200};

	xmi_cubic_spline *spline = xmi_cubic_spline_init(x, y, 5);

	size_t i;

	for (i = 0; i < 41; i++) {
		double eval = xmi_cubic_spline_eval(spline, i / 10.0);
		fprintf(stdout, "%f vs %f\n", results[i], eval);
		assert(fabs(results[i] - eval) < 1.0);
	}

	xmi_cubic_spline_free(spline);

	return 0;
}


