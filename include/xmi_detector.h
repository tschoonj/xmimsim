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

#ifndef XMI_DETECTOR_H
#define XMI_DETECTOR_H

#include "xmi_data_structs.h"

struct xmi_escape_ratios {
        int  n_elements;
        int  n_fluo_input_energies;
        int  n_compton_input_energies;
        int  n_compton_output_energies;
        int *Z;
        double *fluo_escape_ratios;
        double *fluo_escape_input_energies;
        double *compton_escape_ratios;
        double *compton_escape_input_energies;
        double *compton_escape_output_energies;
	char *xmi_input_string;
};

void xmi_escape_ratios_calculation(struct xmi_input *inputPtr, struct xmi_escape_ratios **escape_ratios, char *input_string, char *hdf5_file);

void xmi_create_empty_escape_ratios_hdf5_file(char *hdf5_file);

int xmi_update_escape_ratios_hdf5_file(char *hdf5_file, struct xmi_escape_ratios *escape_ratios);


//return 1 on success, 0 on no match
int xmi_check_escape_ratios_match(struct xmi_input *input_in, struct xmi_input *input_h5);

int xmi_find_escape_ratios_match(char *hdf5_file, struct xmi_input *A, struct xmi_escape_ratios **rv);

void xmi_free_escape_ratios(struct xmi_escape_ratios *escape_ratios);

int xmi_init_input_escape_ratios(xmi_inputFPtr *inputFPtr);

#endif
