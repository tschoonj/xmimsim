#ifndef XMI_SOLID_ANGLE_H
#define XMI_SOLID_ANGLE_H

#include "xmi_data_structs.h"

struct xmi_solid_angle {
	double *solid_angles;
	long int grid_dims_r_n;
	long int grid_dims_theta_n;
	double *grid_dims_r_vals;
	double *grid_dims_theta_vals;
	char *xmi_input_string;
};

void xmi_solid_angle_calculation(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_file);

void xmi_create_empty_solid_angle_hdf5_file(char *hdf5_file);

int xmi_update_solid_angle_hdf5_file(char *hdf5_file, struct xmi_solid_angle *solid_angle);



#endif
