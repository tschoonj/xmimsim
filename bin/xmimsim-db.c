#include "xmi_hdf5.h"

int main(int argc, char *argv[]) {
	int rv;

	if (argc != 1) {
		rv = xmi_db2(argv[1]);
	}
	else {
		rv = xmi_db2("xmimsimdata.h5");
	}
	return 0;
}
