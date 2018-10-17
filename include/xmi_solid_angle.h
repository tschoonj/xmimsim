/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_SOLID_ANGLE_H
#define XMI_SOLID_ANGLE_H

#include "xmi_data_structs.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _xmi_solid_angle xmi_solid_angle;
struct _xmi_solid_angle {
	double *solid_angles;
	long int grid_dims_r_n;
	long int grid_dims_theta_n;
	double *grid_dims_r_vals;
	double *grid_dims_theta_vals;
	char *xmi_input_string;
};

int xmi_get_solid_angle_file(char **file, int create_file);

void xmi_solid_angle_calculation(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *options);

int xmi_create_empty_solid_angle_hdf5_file(char *hdf5_file);

int xmi_update_solid_angle_hdf5_file(char *hdf5_file, xmi_solid_angle *solid_angle);

//return 1 on success, 0 on no match
int xmi_check_solid_angle_match(xmi_input *input_in, xmi_input *input_h5);

int xmi_find_solid_angle_match(char *hdf5_file, xmi_input *A, xmi_solid_angle **rv, xmi_main_options *options);

void xmi_free_solid_angle(xmi_solid_angle *solid_angle);

#define XMI_SOLID_ANGLES_MIN_VERSION 3.1

#ifdef __cplusplus
}
#endif

#endif
