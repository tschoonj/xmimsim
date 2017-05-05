#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include <glib.h>
#include <unistd.h>




int main(int argc, char *argv[]) {
	char *xmimsim_hdf5_solid_angles = "xmimsim-solid-angles.h5";
	struct xmi_input *input;
	xmi_inputFPtr inputFPtr;
	struct xmi_solid_angle *solid_angle_def = NULL;
	struct xmi_main_options options;
	gchar *xmi_input_string;
	
	// set options
	options = xmi_get_default_main_options();
	options.use_opencl = 1;
	options.verbose = 1;

	// init test
	g_assert(test_init() == 1);

	// set environment variable
	g_assert(g_setenv("XMIMSIM_CL_LIB", XMIMSIM_CL_LIB, TRUE) == TRUE);

	// download file
	g_assert(test_download_file(TEST_XMSI_URL) == 1);

	// read the file
	g_assert(xmi_read_input_xml(TEST_XMSI, &input) == 1);

	// copy to the corresponding fortran variable
	xmi_input_C2F(input, &inputFPtr);

	// initialization
	g_assert(xmi_init_input(&inputFPtr) == 1);

	// create new solid angles file
	g_assert(xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 1);

	// check if solid angles are already precalculated
	g_assert(xmi_find_solid_angle_match(xmimsim_hdf5_solid_angles, input, &solid_angle_def, options) == 1);

	// check solid_angle_def is NULL
	g_assert(solid_angle_def == NULL);

	// convert input to string
	g_assert(xmi_write_input_xml_to_string(&xmi_input_string,input) == 1);

	// run the actual calculation
	xmi_solid_angle_calculation(inputFPtr, &solid_angle_def, xmi_input_string, options);

	// confirm that there was no fallback to Fortran -> otherwise skip test
	if (g_getenv("XMIMSIM_CL_FALLBACK") != NULL)
		return 77;

	// write to hdf5 file
	g_assert(xmi_update_solid_angle_hdf5_file(xmimsim_hdf5_solid_angles, solid_angle_def) == 1);

	// free solid angles
	xmi_free_solid_angle(solid_angle_def);

	// and read them back in -> they should exist at this point
	g_assert(xmi_find_solid_angle_match(xmimsim_hdf5_solid_angles, input, &solid_angle_def, options) == 1);
	g_assert(solid_angle_def != NULL);

	// cleanup
	unlink(xmimsim_hdf5_solid_angles);
	xmi_free_input(input);

	// xmi_free_input_F(&inputFPtr); // this currently does not work since I do not read in the HDF5 data file
	xmi_free_solid_angle(solid_angle_def);
	g_free(xmi_input_string);

	return 0;
}
