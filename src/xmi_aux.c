#include "xmi_aux.h"
#include <stdlib.h>
#include <string.h>

void *xmi_memdup(const void *mem, size_t bytes) {
	void *temp;

	if (mem == NULL)
		return NULL;
	
	temp = (void*) malloc(bytes);
	if (temp == NULL)
		return NULL;

	memcpy(temp,mem,bytes);

	return temp;
}
