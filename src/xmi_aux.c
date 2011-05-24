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

#include "xmi_aux.h"
#include "xmi_data_structs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_linalg.h>
#include <xraylib-parser.h>




void *xmi_memdup(const void *mem, size_t bytes) {
	void *temp;

	if (mem == NULL || bytes == 0) {
#if DEBUG == 1
		fprintf(stdout,"Warning: xmi_memdup returns NULL\n");
#endif
		return NULL;
	}
	
	temp = (void*) malloc(bytes);
	if (temp == NULL) {
#if DEBUG == 1
		fprintf(stdout,"Warning: xmi_memdup returns NULL\n");
#endif
		return NULL;
	}
	memcpy(temp,mem,bytes);

	return temp;
}

int *xmi_sort_idl_int(int *array,int n_elements) {
	int *rv;
	int *array_copy;
	int i,j;
	int *res;

	if (n_elements < 1) {
		rv = (int *) malloc(sizeof(int));
		rv[0] = -1;
		return rv;
	}

	array_copy = (int *) xmi_memdup(array,n_elements*sizeof(int));
	qsort(array_copy,(size_t) n_elements, sizeof(int),xmi_cmp_int);

#if DEBUG == 2
	fprintf(stdout,"xmi_sort_idl_int check\n");
	for (i=0 ; i < n_elements ; i++)
		fprintf(stdout,"%i\n",array_copy[i]);
#endif




	rv = (int *) malloc(sizeof(int)*n_elements);

	for (i = 0 ; i < n_elements ; i++) {
		for (j = 0 ; j < n_elements ; j++)
			if (array_copy[i] == array[j]) {
				rv[i] = j;
				break;
			}
	}

	free(array_copy);

	return rv;
} 



int xmi_cmp_int(const void *a, const void *b) {
	return *((int *) a) - *((int *) b);
}


void xmi_inverse_matrix(double x[3], double y[3], double z[3], double **inverseF) {

	gsl_matrix *m = gsl_matrix_alloc(3,3);
	gsl_matrix *inverse = gsl_matrix_alloc(3,3);
	gsl_permutation *p = gsl_permutation_calloc(3);
	int signum;
	double *rv;
	int i,j,k;


	gsl_matrix_set(m,0,0, x[0]);
	gsl_matrix_set(m,1,0, x[1]);
	gsl_matrix_set(m,2,0, x[2]);

	gsl_matrix_set(m,0,1, y[0]);
	gsl_matrix_set(m,1,1, y[1]);
	gsl_matrix_set(m,2,1, y[2]);

	gsl_matrix_set(m,0,2, z[0]);
	gsl_matrix_set(m,1,2, z[1]);
	gsl_matrix_set(m,2,2, z[2]);

#if DEBUG == 2
	fprintf(stdout,"input matrix\n");
	gsl_matrix_fprintf(stdout,m , "%g");
#endif

	//invert the sucker
	gsl_linalg_LU_decomp(m,p,&signum);
	gsl_linalg_LU_invert(m,p,inverse);
#if DEBUG == 2
	fprintf(stdout,"inverted matrix\n");
	gsl_matrix_fprintf(stdout,inverse , "%g");
#endif

	gsl_matrix_transpose(inverse);
#if DEBUG == 2
	fprintf(stdout,"transposed inverted matrix\n");
	gsl_matrix_fprintf(stdout,inverse , "%g");
#endif

	rv = (double *) malloc(9*sizeof(double));

	k = 0;

	for (i = 0 ; i < 3 ; i++)
		for (j = 0 ; j < 3 ; j++) 
			rv[k++] = gsl_matrix_get(inverse, i,j);

	gsl_matrix_free(m);
	gsl_matrix_free(inverse);
	gsl_permutation_free(p);

	*inverseF = rv;
}

void xmi_determinant_matrix(double x[3], double y[3], double z[3]) {
	gsl_matrix *m = gsl_matrix_alloc(3,3);
	gsl_permutation *p = gsl_permutation_calloc(3);
	int signum;
	int i,j,k;
	double det;
	gsl_matrix_set(m,0,0, x[0]);
	gsl_matrix_set(m,1,0, x[1]);
	gsl_matrix_set(m,2,0, x[2]);

	gsl_matrix_set(m,0,1, y[0]);
	gsl_matrix_set(m,1,1, y[1]);
	gsl_matrix_set(m,2,1, y[2]);

	gsl_matrix_set(m,0,2, z[0]);
	gsl_matrix_set(m,1,2, z[1]);
	gsl_matrix_set(m,2,2, z[2]);
	gsl_linalg_LU_decomp(m,p,&signum);
	det=gsl_linalg_LU_det(m,signum);

	fprintf(stdout,"Determinant is: %lf\n",det);
	return;

}
struct compoundData *xmi_layer2compoundData(struct xmi_layer *xl) {
	struct compoundData *rv;

	rv = (struct compoundData *) malloc(sizeof(struct compoundData));

	if (xl != NULL) {
		rv->nElements = xl->n_elements;
		rv->Elements = (int *) xmi_memdup(xl->Z, sizeof(int)*xl->n_elements);
		rv->massFractions= (double *) xmi_memdup(xl->weight, sizeof(double)*xl->n_elements);
	}
	else {
		rv->nElements = 0; 
	}
	return rv;
}

struct xmi_layer *compoundData2xmi_layer( struct compoundData *cd) {
	struct xmi_layer *rv;

	rv = (struct xmi_layer *) malloc(sizeof(struct xmi_layer));

		rv->n_elements = cd->nElements;
		rv->Z = (int *) xmi_memdup(cd->Elements, sizeof(int)*cd->nElements);
		rv->weight = (double *) xmi_memdup(cd->massFractions, sizeof(double)*cd->nElements);
	return rv;
}

double *xmi_dindgen(int n) {
	double *rv;
	int i;

	if (n < 1) {
		fprintf(stderr,"xmi_dindgen requires a strictly positive argument\n");
		return NULL;
	}

	rv = (double *) malloc(sizeof(double)*n);
	
	for (i = 0 ; i < n ; i++) {
		rv[i] = (double )i;
	}

	return rv;
}


