#include "xmi_data.h"
#include "xmi_aux.h"
#include "xmi_private.h"

XMI_MAIN
	int rv;

	xmi_init_hdf5();

	if (argc != 1) {
		rv = xmi_db(argv[1]);
	}
	else {
		rv = xmi_db("xmimsimdata.h5");
	}
	if (rv == 1)
		rv = 0;
	else if (rv == 0)
		rv = 1;

	return rv;
}
