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

#include "xmi_data_structs.h"
#include "xmi_detector.h"
#include <stdlib.h>

void xmi_escape_ratios_calculation_fortran(xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr, struct xmi_escape_ratios **escape_ratios, char *input_string);

void xmi_escape_ratios_calculation(struct xmi_input *inputPtr, struct xmi_escape_ratios **escape_ratios, char *input_string, char *hdf5_file) {

	struct xmi_input *esc_ratio_inputPtr;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;


	//copy input structure
	xmi_copy_input(inputPtr, &esc_ratio_inputPtr);

	xmi_free_composition(esc_ratio_inputPtr->composition);

	xmi_copy_abs_or_crystal2composition(esc_ratio_inputPtr->detector->crystal_layers, esc_ratio_inputPtr->detector->n_crystal_layers, &(esc_ratio_inputPtr->composition));
	esc_ratio_inputPtr->composition->reference_layer = 1;

	//modify geometry
	esc_ratio_inputPtr->geometry->d_sample_source = 1.0;	
	esc_ratio_inputPtr->geometry->n_sample_orientation[0]=0.0;
	esc_ratio_inputPtr->geometry->n_sample_orientation[1]=0.0;
	esc_ratio_inputPtr->geometry->n_sample_orientation[2]=1.0;

	esc_ratio_inputPtr->general->n_interactions_trajectory = 1;
	esc_ratio_inputPtr->general->n_photons_line = 100000;


	//copy to fortran variable
	xmi_input_C2F(esc_ratio_inputPtr,&inputFPtr);

	//initialize
	if (xmi_init_input_escape_ratios(&inputFPtr) == 0) {
		exit(1);
	}
	
	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr) == 0) {
		fprintf(stderr,"Could not initialize from hdf5 data file\n");
		exit(1);
	}	

	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);

	//do the actual calculation...
	xmi_escape_ratios_calculation_fortran(inputFPtr, hdf5FPtr, escape_ratios, input_string);

}
