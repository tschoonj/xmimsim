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
#include <hdf5.h>
#include <glib.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "xmi_aux.h"
#include "xmi_xml.h"

static herr_t xmi_read_single_solid_angle( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data);

void xmi_create_empty_solid_angle_hdf5_file(char *hdf5_file) {

	hid_t       file_id;   /* file identifier */
	herr_t      status;


	/* Create a new file using default properties. */
	file_id = H5Fcreate(hdf5_file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);


	/* Terminate access to the file. */
	status = H5Fclose(file_id); 
	return;
}


int xmi_update_solid_angle_hdf5_file(char *hdf5_file, struct xmi_solid_angle *solid_angle) {
	hid_t file_id;
	char buffer[1024];
	hid_t group_id;
	hid_t dspace_id;
	hid_t dset_id;
	hsize_t dims[2]; 
	hsize_t xmi_input_strlen;
	int i;
	gchar *timestring;
	GTimeVal time;



	file_id = H5Fopen(hdf5_file, H5F_ACC_RDWR , H5P_DEFAULT);
	if (file_id < 0 ) {
		fprintf(stderr,"Cannot open file %s for read/write\n",hdf5_file);
		return 0;
	}



	//create group name based on user and timestamp
	g_get_current_time(&time);
        timestring = g_time_val_to_iso8601(&time);
	
	sprintf(buffer,"%s %s",g_get_user_name(),timestring);

	g_free(timestring);

	//create group
	group_id = H5Gcreate(file_id, buffer, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	//add solid_angles dataset
	dims[1] = solid_angle->grid_dims_r_n;
	dims[0] = solid_angle->grid_dims_theta_n;


#if DEBUG == 2
	fprintf(stdout,"r_values:\n");
	for (i = 0 ; i < 5 ; i++)
		fprintf(stdout,"%lf\n",solid_angle->grid_dims_r_vals[i]);

	fprintf(stdout,"theta_values:\n");
	for (i = 0 ; i < 5 ; i++)
		fprintf(stdout,"%lf\n",solid_angle->grid_dims_theta_vals[i]);
#endif
	dspace_id = H5Screate_simple(2, dims, dims);
	dset_id = H5Dcreate(group_id, "solid_angles",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);	
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, solid_angle->solid_angles );	
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dspace_id = H5Screate_simple(1, &dims[1], &dims[1]);
	dset_id = H5Dcreate(group_id, "grid_dims_r_vals",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);	
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, solid_angle->grid_dims_r_vals );	
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dspace_id = H5Screate_simple(1, &dims[0], &dims[0]);
	dset_id = H5Dcreate(group_id, "grid_dims_theta_vals",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);	
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, solid_angle->grid_dims_theta_vals );	
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	xmi_input_strlen = strlen(solid_angle->xmi_input_string)+1;
	dspace_id = H5Screate_simple(1, &xmi_input_strlen, &xmi_input_strlen);
	dset_id = H5Dcreate(group_id, "xmi_input_string",H5T_NATIVE_CHAR, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);	
	H5Dwrite(dset_id, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL,H5P_DEFAULT, solid_angle->xmi_input_string);	
	H5Sclose(dspace_id);
	H5Dclose(dset_id);


	H5Gclose(group_id);

	//look for open objects
#if DEBUG == 2
	fprintf(stdout,"open objects: %i\n",(int) H5Fget_obj_count(file_id, H5F_OBJ_ALL));
#endif


	H5Fflush(file_id, H5F_SCOPE_GLOBAL);
#if DEBUG == 2
	fprintf(stdout,"open objects: %i\n",(int) H5Fget_obj_count(file_id, H5F_OBJ_ALL));
#endif

	H5Fclose(file_id);

	return 1;
}

struct multiple_solid_angles {
	struct xmi_solid_angle *solid_angles;
	int n_solid_angles;
};


int xmi_read_solid_angle_hdf5_file(char *hdf5_file, struct xmi_solid_angle **solid_angles, int *n_solid_angles) {

	hid_t file_id;
	char buffer[1024];
	ssize_t n_groups;
	size_t n_groups_max = -1;
	hid_t *groups;
	int i;
	struct multiple_solid_angles *msa;
	herr_t iterate_rv;

	//open the hdf5 file read-only!
	file_id = H5Fopen(hdf5_file, H5F_ACC_RDONLY , H5P_DEFAULT);
	if (file_id < 0 ) {
		fprintf(stderr,"Cannot open file %s for reading\n",hdf5_file);
		return 0;
	}

	msa = (struct multiple_solid_angles *) malloc(sizeof(struct multiple_solid_angles));
	msa->solid_angles = NULL;
	msa->n_solid_angles = 0;


	//examine each individual group
	//use H5Literate
	iterate_rv = H5Literate(file_id, H5_INDEX_NAME, H5_ITER_INC, NULL, xmi_read_single_solid_angle,(void *) msa);

	if (iterate_rv < 0)
		return 0;

	*solid_angles = msa->solid_angles;
	*n_solid_angles = msa->n_solid_angles;

	H5Fclose(file_id);
	return 1;
}

static herr_t xmi_read_single_solid_angle( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
	hid_t dset_id, dapl_id, dspace_id;
	hsize_t dims[2], dims_string[1]; 
	hid_t group_id;
	struct multiple_solid_angles *msa = (struct multiple_solid_angles *) op_data;

#if DEBUG == 2
	fprintf(stdout,"Group name: %s\n",name);
#endif


	msa->solid_angles = (struct xmi_solid_angle *) realloc(msa->solid_angles, sizeof(struct xmi_solid_angle)*++msa->n_solid_angles);


	//open group
	group_id = H5Gopen(g_id,name, H5P_DEFAULT);
	if (group_id < 0) {
		fprintf(stderr,"Error opening group %s\n",name);
		return -1;
	}

	//open dataset solid_angles
	dset_id = H5Dopen(group_id, "solid_angles", H5P_DEFAULT);
	
	//get dimensions of solid_angles dataset
	dspace_id = H5Dget_space(dset_id);
	if (H5Sget_simple_extent_ndims(dspace_id) != 2) {
		fprintf(stderr,"Number of dimensions of solid angles dataset must be equal to 2\n");
		return -1;
	}
	H5Sget_simple_extent_dims(dspace_id, dims, NULL);
	
	//allocate memory
	msa->solid_angles[msa->n_solid_angles-1].solid_angles = (double *) malloc(sizeof(double)*dims[0]*dims[1]);
	msa->solid_angles[msa->n_solid_angles-1].grid_dims_r_n = dims[1];
	msa->solid_angles[msa->n_solid_angles-1].grid_dims_theta_n = dims[0];

	H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,msa->solid_angles[msa->n_solid_angles-1].solid_angles );
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

#if DEBUG == 2
	fprintf(stdout,"solid angles processed\n");
#endif


	dset_id = H5Dopen(group_id, "grid_dims_r_vals", H5P_DEFAULT);
	dspace_id = H5Dget_space(dset_id);
	msa->solid_angles[msa->n_solid_angles-1].grid_dims_r_vals = (double *) malloc(sizeof(double)*dims[1]);
	H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,msa->solid_angles[msa->n_solid_angles-1].grid_dims_r_vals );
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dset_id = H5Dopen(group_id, "grid_dims_theta_vals", H5P_DEFAULT);
	dspace_id = H5Dget_space(dset_id);
	msa->solid_angles[msa->n_solid_angles-1].grid_dims_theta_vals = (double *) malloc(sizeof(double)*dims[0]);
	H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,msa->solid_angles[msa->n_solid_angles-1].grid_dims_theta_vals );
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dset_id = H5Dopen(group_id, "xmi_input_string", H5P_DEFAULT);
	dspace_id = H5Dget_space(dset_id);
	H5Sget_simple_extent_dims(dspace_id, dims_string, NULL);
#if DEBUG == 2
	fprintf(stdout,"dims_string: %i\n",dims_string[0]);
#endif
	msa->solid_angles[msa->n_solid_angles-1].xmi_input_string = (char *) malloc(sizeof(char)*dims_string[0]);
	H5Dread(dset_id, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,msa->solid_angles[msa->n_solid_angles-1].xmi_input_string );
	H5Sclose(dspace_id);
	H5Dclose(dset_id);



	H5Gclose(group_id);
	
	return 0;
}

int xmi_check_solid_angle_match(struct xmi_input *A, struct xmi_input *B) {
	int i;

	//composition
	if (A->composition->n_layers != B->composition->n_layers) {
		return 0;
	}
	if (A->composition->reference_layer != B->composition->reference_layer) {
		return 0;
	}
	for (i = 0 ; i < A->composition->n_layers ; i++) {
		if (fabsl(A->composition->layers[i].thickness- B->composition->layers[i].thickness)/A->composition->layers[i].thickness > XMI_COMPARE_THRESHOLD) {
			return 0;
		}
	}
	//
	//geometry
#define XMI_IF_COMPARE_GEOMETRY(a) if (fabsl(A->geometry->a - B->geometry->a)/A->geometry->a > XMI_COMPARE_THRESHOLD){\
	return 0;\
	}	
#define XMI_IF_COMPARE_GEOMETRY2(a) if (fabsl(A->geometry->a - B->geometry->a) > XMI_COMPARE_THRESHOLD){\
	return 0;\
	}	

	//should compare normalized orientations...
	xmi_normalize_vector_double(A->geometry->n_sample_orientation, 3);
	xmi_normalize_vector_double(B->geometry->n_sample_orientation, 3);
	XMI_IF_COMPARE_GEOMETRY2(n_sample_orientation[0])
	XMI_IF_COMPARE_GEOMETRY2(n_sample_orientation[1])
	XMI_IF_COMPARE_GEOMETRY2(n_sample_orientation[2])
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[0])
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[1])
	if (fabsl((A->geometry->p_detector_window[2]-A->geometry->d_sample_source)-(B->geometry->p_detector_window[2]-B->geometry->d_sample_source)) > XMI_COMPARE_THRESHOLD)
		return 0;
	//should compare normalized orientations...
	xmi_normalize_vector_double(A->geometry->n_detector_orientation, 3);	
	xmi_normalize_vector_double(B->geometry->n_detector_orientation, 3);	
	XMI_IF_COMPARE_GEOMETRY2(n_detector_orientation[0])
	XMI_IF_COMPARE_GEOMETRY2(n_detector_orientation[1])
	XMI_IF_COMPARE_GEOMETRY2(n_detector_orientation[2])
	XMI_IF_COMPARE_GEOMETRY(area_detector)
	XMI_IF_COMPARE_GEOMETRY2(collimator_height)
	XMI_IF_COMPARE_GEOMETRY2(collimator_diameter)


	return 1;
}

int xmi_find_solid_angle_match(char *hdf5_file, struct xmi_input *A, struct xmi_solid_angle **rv) {

	struct xmi_solid_angle *hdf5_solid_angles;
	int n_hdf5_solid_angles;
	struct xmi_input **hdf5_inputs;
	int i;

	if (xmi_read_solid_angle_hdf5_file(hdf5_file, &hdf5_solid_angles, &n_hdf5_solid_angles) == 0 )
		return 0;

	if (n_hdf5_solid_angles == 0) {
		//not found
		*rv = NULL;
		return 1;
	}

	//compare
	hdf5_inputs = (struct xmi_input **) malloc(sizeof(struct xmi_input *)*n_hdf5_solid_angles);

	*rv = NULL;

	for (i = 0 ; i < n_hdf5_solid_angles ; i++) {
		if (*rv == NULL) {
			if (xmi_read_input_xml_from_string(hdf5_solid_angles[i].xmi_input_string, &hdf5_inputs[i]) == 0)
				return 0;
			//check if it matches.
			if (xmi_check_solid_angle_match(A, hdf5_inputs[i]) == 1) {
				//match!
				*rv = hdf5_solid_angles+i;
			}		
			else {
				//no match
				xmi_free_solid_angle(hdf5_solid_angles+i);
			}
			xmi_free_input(hdf5_inputs[i]);
		}
		else {
			xmi_free_solid_angle(&hdf5_solid_angles[i]);
		}
	}


	return 1;
}


void xmi_free_solid_angle(struct xmi_solid_angle *solid_angle) {

	free(solid_angle->solid_angles);
	free(solid_angle->grid_dims_r_vals);
	free(solid_angle->grid_dims_theta_vals);
	free(solid_angle->xmi_input_string);
}


