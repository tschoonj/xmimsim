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

/* Based on the code found at https://github.com/JamesBremner/raven-cSpline */

#ifndef XMI_SPLINE_H
#define XMI_SPLINE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>

typedef struct _xmi_cubic_spline xmi_cubic_spline;

xmi_cubic_spline *xmi_cubic_spline_init(double *x, double *y, size_t n);

void xmi_cubic_spline_free(xmi_cubic_spline *spline);

double xmi_cubic_spline_eval(xmi_cubic_spline *spline, double x);

#ifdef __cplusplus
}
#endif

#endif
