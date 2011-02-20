#include "xmi_solid_angle.h"
#include <hdf5.h>
#include <glib.h>
#include <strings.h>
#include <math.h>
#include <stdlib.h>

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



	//to open the hdf5_file, we will probably have to change euid
	//let's try first without...
	file_id = H5Fopen(hdf5_file, H5F_ACC_RDWR , H5P_DEFAULT);
	if (file_id < 0 ) {
		fprintf(stderr,"Cannot open file %s for read/write\n",hdf5_file);
		return 0;
	}



	//create group name based on user and timestamp
	sprintf(buffer,"%s %s",g_get_user_name(),g_date_time_format(g_date_time_new_now_local(),"%F %H:%M:%S (%Z)"));

	//create group
	group_id = H5Gcreate(file_id, buffer, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	//add solid_angles dataset
	dims[1] = solid_angle->grid_dims_r_n;
	dims[0] = solid_angle->grid_dims_theta_n;


#if DEBUG == 1
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

	*solid_angles = msa->solid_angles;
	*n_solid_angles = msa->n_solid_angles;

	return 1;
}

static herr_t xmi_read_single_solid_angle( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
	hid_t dset_id, dapl_id, dspace_id;
	hsize_t dims[2], dims_string[1]; 
	hid_t group_id;
	struct multiple_solid_angles *msa = (struct multiple_solid_angles *) op_data;

#if DEBUG == 1
	fprintf(stdout,"Group name: %s\n",name);
#endif


	msa->solid_angles = (struct xmi_solid_angle *) realloc(msa->solid_angles, sizeof(struct xmi_solid_angle *)*++msa->n_solid_angles);


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
#if DEBUG == 1
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



	return 1;
}


