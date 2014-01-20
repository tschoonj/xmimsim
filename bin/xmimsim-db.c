#include "xmi_data.h"
#include "xmi_aux.h"

int main(int argc, char *argv[]) {
	int rv;

	xmi_init_hdf5();

	if (argc != 1) {
		rv = xmi_db(argv[1]);
	}
	else {
		rv = xmi_db("xmimsimdata.h5");
	}
	return 0;
}
