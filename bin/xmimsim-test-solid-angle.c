#include "xmi_solid_angle.h"
#include "xmi_xml.h"
#include "xmi_random.h"
#include <stdlib.h>


int main(int argc, char *argv[]) {
	int rv;
	xmi_inputFPtr inputFPtr;
	struct xmi_input *input;
	struct xmi_solid_angle *solid_angle;
	xmi_hdf5FPtr hdf5FPtr;
	struct xmi_solid_angle *solid_angle_h5;
	int n_solid_angle_h5;
	struct xmi_input **solid_angle_inputs;
	int i;
/*
	if (argc != 2)
		return 1;


	xmi_start_random_acquisition();


	rv = xmi_read_input_xml(argv[1],&input);

	if (rv != 1) {
		return 1;
	}

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputFPtr);
	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}

	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(XMIMSIM_HDF5_DEFAULT,inputFPtr,&hdf5FPtr) == 0) {
		fprintf(stderr,"Could not initialize from hdf5 data file\n");
		return 1;
	}	
	xmi_solid_angle_calculation(inputFPtr, &solid_angle, argv[1]);

	//update hdf5 file
	if( xmi_update_solid_angle_hdf5_file(XMIMSIM_HDF5_SOLID_ANGLES, solid_angle) == 0)
		return 1;
*/

	//read solid angles HDF5 file
	if (xmi_read_solid_angle_hdf5_file(XMIMSIM_HDF5_SOLID_ANGLES, &solid_angle_h5, &n_solid_angle_h5) == 0)
		return 1;

	solid_angle_inputs = (struct xmi_input **) malloc(sizeof(struct xmi_input *)*n_solid_angle_h5);

	for (i = 0 ; i < n_solid_angle_h5 ; i++)  {
		fprintf(stdout,"xmlfile: %s\n",solid_angle_h5[i].xmi_input_string);
		if (xmi_read_input_xml_from_string(solid_angle_h5[i].xmi_input_string, &solid_angle_inputs[i]) == 0)
			return 1;
	}
		


	xmi_end_random_acquisition();

	return 0;
}
