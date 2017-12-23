/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "xmi_spline.h"
#include "xmi_aux.h"
#include <stdio.h>
#include <glib.h>

struct xmi_cubic_spline_coeffs {
	double a;
	double b;
	double c;
	double d;
	double x;
};

struct xmi_cubic_spline {
	size_t n;
	double *x;
	double *y;
	struct xmi_cubic_spline_coeffs *all_coeffs;
};

struct xmi_cubic_spline *xmi_cubic_spline_init(double *x, double *y, size_t n) {

	struct xmi_cubic_spline *rv = (struct xmi_cubic_spline *) g_malloc(sizeof(struct xmi_cubic_spline));

	rv->n = n-1;
	rv->x = (double *) xmi_memdup(x, sizeof(double) * n);
	rv->y = (double *) xmi_memdup(y, sizeof(double) * n);

	double *a = (double *) xmi_memdup(y, sizeof(double) * n);
	double *b = (double *) g_malloc(sizeof(double) * rv->n);
	double *d = (double *) g_malloc(sizeof(double) * rv->n);
	double *h = (double *) g_malloc(sizeof(double) * rv->n);

	int i, j;

	for (i = 0 ; i < rv->n ; ++i) {
		h[i] = x[i+1] - x[i];
	}

	double *alpha = (double *) g_malloc(sizeof(double) * rv->n);
	alpha[0] = 0.0;

	for (i = 1 ; i < rv->n ; ++i) {
		alpha[i] = 3*(a[i+1]-a[i])/h[i] - 3*(a[i]-a[i-1])/h[i-1];
	}

	double *c = (double *) g_malloc(sizeof(double) * n);
	double *l = (double *) g_malloc(sizeof(double) * n);
	double *mu = (double *) g_malloc(sizeof(double) * n);
	double *z = (double *) g_malloc(sizeof(double) * n);

	l[0] = 1.0;
	mu[0] = 0.0;
	z[0] = 0.0;

	for (i = 1 ; i < rv->n ; ++i) {
		l[i] = 2 *(x[i+1]-x[i-1])-h[i-1]*mu[i-1];
		mu[i] = h[i]/l[i];
		z[i] = (alpha[i]-h[i-1]*z[i-1])/l[i];
	}

	l[rv->n] = 1.0;
	z[rv->n] = 0.0;
	c[rv->n] = 0.0;

	for (j = rv->n - 1 ; j >= 0 ; --j) {
		c[j] = z[j] - mu[j] * c[j+1];
		b[j] = (a[j+1]-a[j])/h[j]-h[j]*(c[j+1]+2*c[j])/3;
		d[j] = (c[j+1]-c[j])/3/h[j];
	}

	rv->all_coeffs = (struct xmi_cubic_spline_coeffs *) g_malloc(sizeof(struct xmi_cubic_spline_coeffs) * rv->n);

	for (i = 0 ; i < rv->n ; ++i) {
		rv->all_coeffs[i].a = a[i];
		rv->all_coeffs[i].b = b[i];
		rv->all_coeffs[i].c = c[i];
		rv->all_coeffs[i].d = d[i];
		rv->all_coeffs[i].x = x[i];
	}

	g_free(a);
	g_free(b);
	g_free(c);
	g_free(d);
	g_free(l);
	g_free(mu);
	g_free(z);
	g_free(alpha);
	g_free(h);

	return rv;
}

void xmi_cubic_spline_free(struct xmi_cubic_spline *spline) {
	g_free(spline->x);
	g_free(spline->y);
	g_free(spline->all_coeffs);
	g_free(spline);
}

double xmi_cubic_spline_eval(struct xmi_cubic_spline *spline, double x) {
	int j;
	for (j = 0 ; j < spline->n ; j++) {
		if (spline->all_coeffs[j].x > x) {
			if (j == 0) {
				j++;
			}
			break;
		}
	}
	j--;

	double dx = x - spline->all_coeffs[j].x;
	double y =
		spline->all_coeffs[j].a +
		spline->all_coeffs[j].b * dx +
		spline->all_coeffs[j].c * dx* dx +
		spline->all_coeffs[j].d * dx* dx * dx;

	return y;
}
