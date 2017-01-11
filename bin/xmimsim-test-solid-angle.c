/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "xmi_solid_angle.h"
#include "xmi_xml.h"
#include "xmi_random.h"
#include <unistd.h>


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
	struct xmi_solid_angle *solid_angle_def;

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

	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);
/*
	xmi_solid_angle_calculation(inputFPtr, &solid_angle, argv[1]);

	//update hdf5 file
	if( xmi_update_solid_angle_hdf5_file(XMIMSIM_HDF5_SOLID_ANGLES, solid_angle) == 0)
		return 1;
*/
/*

	//read solid angles HDF5 file
	if (xmi_read_solid_angle_hdf5_file(XMIMSIM_HDF5_SOLID_ANGLES, &solid_angle_h5, &n_solid_angle_h5) == 0)
		return 1;

	solid_angle_inputs = (struct xmi_input **) g_malloc(sizeof(struct xmi_input *)*n_solid_angle_h5);

	for (i = 0 ; i < n_solid_angle_h5 ; i++)  {
		//fprintf(stdout,"xmlfile: %s\n",solid_angle_h5[i].xmi_input_string);
		if (xmi_read_input_xml_from_string(solid_angle_h5[i].xmi_input_string, &solid_angle_inputs[i]) == 0)
			return 1;
		//look for matches
		fprintf(stdout,"match: %i\n",xmi_check_solid_angle_match(input,solid_angle_inputs[i]));
	}
*/
	if (xmi_find_solid_angle_match(XMIMSIM_HDF5_SOLID_ANGLES, input, &solid_angle_def) == 0)
		return 0;

	if (solid_angle_def == NULL)
		fprintf(stdout,"should not be NULL pointer\n");
	else {
		fprintf(stdout,"solid_angle_def->grid_dims_r_n: %li\n",solid_angle_def->grid_dims_r_n);
		fprintf(stdout,"solid_angle_def->grid_dims_theta_n: %li\n",solid_angle_def->grid_dims_theta_n);
	}




	xmi_end_random_acquisition();

	return 0;
}
