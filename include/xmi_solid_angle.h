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

void xmi_solid_angle_calculation(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string);

void xmi_create_empty_solid_angle_hdf5_file(char *hdf5_file);

int xmi_update_solid_angle_hdf5_file(char *hdf5_file, struct xmi_solid_angle *solid_angle);


//these functions could be made far more efficient...
int xmi_read_solid_angle_hdf5_file(char *hdf5_file, struct xmi_solid_angle **solid_angles, int *n_solid_angles);

//return 1 on success, 0 on no match
int xmi_check_solid_angle_match(struct xmi_input *input_in, struct xmi_input *input_h5);

int xmi_find_solid_angle_match(char *hdf5_file, struct xmi_input *A, struct xmi_solid_angle **rv);

void xmi_free_solid_angle(struct xmi_solid_angle *solid_angle);

#endif
