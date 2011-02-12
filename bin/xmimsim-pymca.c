#include "xmi_pymca.h"
#include "xmi_data_structs.h"
#include <stdlib.h>


int main (int argc, char *argv[]) {

	struct xmi_input *pymca_input = NULL;
	struct xmi_pymca *xp = NULL ;

	if (argc != 2)
		return 1;

	xmi_read_input_pymca(argv[1], &pymca_input, &xp);

#if DEBUG == 1
	xmi_print_input(stdout,pymca_input);
#endif


	return 0;
}


