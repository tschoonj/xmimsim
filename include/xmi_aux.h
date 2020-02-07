/*
Copyright (C) 2010-2020 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_AUX_H
#define XMI_AUX_H

#include <stddef.h>
#include "xmi_data_structs.h"
#include <stdio.h>
#include <xraylib.h>

#ifdef __cplusplus
extern "C" {
#endif



#define XMI_CASCADE_FULL 3
#define XMI_CASCADE_RADIATIVE 2
#define XMI_CASCADE_NONRADIATIVE 1
#define XMI_CASCADE_NONE 0


//returns NULL on error
void *xmi_memdup(const void *mem, size_t bytes);

//returns an array with at least one element. If an error occurred, this element will have a value of -1
//works similar to the IDL function carrying the same name... the array that is returned contains indices!!
int *xmi_sort_idl_int(int *array,int n_elements);

//calculates the sum of array containing n elements
double xmi_sum_double(double *array, int n);
int xmi_sum_int(int *array, int n);

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

struct compoundData *xmi_layer2compoundData(xmi_layer *xl);

xmi_layer *compoundData2xmi_layer( struct compoundData *cd);
xmi_layer *compoundDataNIST2xmi_layer( struct compoundDataNIST *cd);

int xmi_cmp_struct_xmi_energy_discrete(const void *a, const void *b);
int xmi_cmp_struct_xmi_energy_continuous(const void *a, const void *b);
int compare_string(const void *a, const void *b);



char *xmi_version_string(void);

int xmi_omp_get_max_threads(void);

void xmi_init_hdf5(void);

enum {
	XMI_HDF5_INVALID = 0,
	XMI_HDF5_DATA,
	XMI_HDF5_SOLID_ANGLES,
	XMI_HDF5_ESCAPE_RATIOS
};

int xmi_copy_between_hdf5_files(int kind, char *file_from, char *file_to, char **groups, int force);

int xmi_get_hdf5_kind(char *name);

//wrapper around free -> necessary to avoid trouble on Windows and perhaps with C++ too
void xmi_free(void *ptr);

//similar for malloc and realloc
void *xmi_malloc(size_t size);
void *xmi_realloc(void *ptr, size_t size);

void xmi_print_progress(char *string, int progress);

gchar* xmi_get_xmimsim_path(void);

#ifndef __GI_SCANNER__
GError* xmi_error_convert_xrl_to_glib(xrl_error *error);

gpointer xmi_object_ref(gpointer obj, const gchar *strloc);
void xmi_object_unref(gpointer obj, gchar *strloc);
#endif

gint xmi_row_major_array_get_offset(GArray *dims, GArray *indices);
GArray* xmi_row_major_array_get_indices(GArray *dims, int offset);

#ifdef __cplusplus
}
#endif
#endif
