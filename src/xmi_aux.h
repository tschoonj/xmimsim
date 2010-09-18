#ifndef XMI_AUX_X
#define XMI_AUX_X

#include <stddef.h>

//returns NULL on error
void *xmi_memdup(const void *mem, size_t bytes);

//returns an array with at least one element. If an error occurred, this element will have a value of -1
int *xmi_sort_int(int *array,int n_elements);


#endif
