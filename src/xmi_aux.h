#ifndef XMI_AUX_X
#define XMI_AUX_X

#include <stddef.h>
#include "xmi_data_structs.h"

//returns NULL on error
void *xmi_memdup(const void *mem, size_t bytes);

//returns an array with at least one element. If an error occurred, this element will have a value of -1
//works similar to the IDL function carrying the same name... the array is returned contains indices!!
int *xmi_sort_idl_int(int *array,int n_elements);

//inverse will be column-major!! intended to be used from fortran only
void xmi_inverse_matrix(double x[3], double y[3], double z[3], double **inverse);

//calculates the sum of array containing n elements
double xmi_sum_double(double *array, int n); 

//multiplies each element of array with scale_factor
void xmi_scale_double(double *array, int n, double scale_factor);

//to be used in qsort or bsearch for comparing integers
int xmi_cmp_int(const void *a, const void *b);

struct compoundData *xmi_layer2compoundData(struct xmi_layer *xl);

struct xmi_layer *compoundData2xmi_layer( struct compoundData *cd);

#endif
