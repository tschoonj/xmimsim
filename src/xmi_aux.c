#include "xmi_aux.h"
#include "xmi_data_structs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


static int xmi_cmp_int(const void *a, const void *b);

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
	int i;
	int *res;

	if (n_elements < 1) {
		rv = (int *) malloc(sizeof(int));
		rv[0] = -1;
		return rv;
	}

	array_copy = (int *) xmi_memdup(array,n_elements*sizeof(int));
	qsort(array_copy,(size_t) n_elements, sizeof(int),xmi_cmp_int);




	rv = (int *) malloc(sizeof(int)*n_elements);

	for (i = 0 ; i < n_elements ; i++) {
		res = bsearch(array+i,array_copy, (size_t) n_elements, sizeof(int), xmi_cmp_int);
		rv[i] = res-array_copy;
	}

	free(array_copy);

	return rv;
} 



static int xmi_cmp_int(const void *a, const void *b) {
	return *((int *) a) - *((int *) b);
}

