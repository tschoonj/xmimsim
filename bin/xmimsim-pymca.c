#include "xmi_pymca.h"
#include "xmi_data_structs.h"


int main (int argc, char *argv[]) {

	struct xmi_input *pymca_input;

	if (argc != 2)
		return 1;

	xmi_read_input_pymca(argv[1], &pymca_input);



	return 0;
}


