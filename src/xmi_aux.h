/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_AUX_X
#define XMI_AUX_X

#include <stddef.h>
#include "xmi_data_structs.h"

//returns NULL on error
void *xmi_memdup(const void *mem, size_t bytes);

//returns an array with at least one element. If an error occurred, this element will have a value of -1
//works similar to the IDL function carrying the same name... the array that is returned contains indices!!
int *xmi_sort_idl_int(int *array,int n_elements);

//inverse will be column-major!! intended to be used from fortran only
void xmi_inverse_matrix(double x[3], double y[3], double z[3], double **inverse);

//calculates the sum of array containing n elements
double xmi_sum_double(double *array, int n); 

//multiplies each element of array with scale_factor
void xmi_scale_double(double *array, int n, double scale_factor);


//calculates the norm of the vector array
double xmi_norm_double(double *array, int n);

//returns the maximum value of the vector array
double xmi_maxval_double(double *array, int n);

//returns the location of the maximum in vector array
int xmi_maxloc_double(double *array, int n);

//normalizes the vector array
void xmi_normalize_vector_double(double *array, int n);

//returns an IDL style dindgen array
double *xmi_dindgen(int n);

//returns minimum value in array
double xmi_minval_double(double *array, int n);

//increase array with value (can also be negative of course)
void xmi_add_val_to_array_double(double *array, int n, double increment);


//to be used in qsort or bsearch for comparing integers
int xmi_cmp_int(const void *a, const void *b);

struct compoundData *xmi_layer2compoundData(struct xmi_layer *xl);

struct xmi_layer *compoundData2xmi_layer( struct compoundData *cd);

#endif
