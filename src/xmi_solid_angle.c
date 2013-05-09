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

#include "config.h"
#include "xmi_solid_angle.h"
#include "xmi_data_structs.h"
#include <hdf5.h>
#include <glib.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "xmi_aux.h"
#include "xmi_xml.h"
#include <sys/stat.h>
#include <xraylib.h>
#include <glib/gstdio.h>
#ifdef MAC_INTEGRATION
	#import <Foundation/Foundation.h>
#endif

#define MIN_VERSION 2.0


struct xmi_solid_angles_data{
	struct xmi_solid_angle **solid_angles;
	struct xmi_input *input;
	struct xmi_main_options options;
};

static herr_t xmi_read_single_solid_angle( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data);

int xmi_create_empty_solid_angle_hdf5_file(char *hdf5_file) {

	hid_t file_id;   /* file identifier */
	hid_t root_group_id;	
	hid_t attribute_id;
	hid_t dataspace_id;
	herr_t status;

	float version = (float) g_ascii_strtod(VERSION, NULL);
	

	/* Create a new file using default properties. */
	file_id = H5Fcreate(hdf5_file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) {
		fprintf(stderr,"Could not create solid angle HDF5 file %s\n",hdf5_file);
		return 0;
	}
	root_group_id = H5Gopen(file_id, "/", H5P_DEFAULT);

	dataspace_id = H5Screate(H5S_SCALAR);
	attribute_id = H5Acreate(root_group_id, "version", H5T_NATIVE_FLOAT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attribute_id, H5T_NATIVE_FLOAT, &version);

	
	H5Aclose(attribute_id);
	H5Sclose(dataspace_id);
	H5Gclose(root_group_id);


	/* Terminate access to the file. */
	status = H5Fclose(file_id); 
	return 1;
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



static herr_t xmi_read_single_solid_angle( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
	hid_t dset_id, dapl_id, dspace_id;
	hsize_t dims[2], dims_string[1]; 
	hid_t group_id;
	char *xmi_input_string;
	struct xmi_solid_angles_data *data = (struct xmi_solid_angles_data *) op_data;
	struct xmi_input *temp_input;
	struct xmi_solid_angle *solid_angles;

#if DEBUG == 2
	fprintf(stdout,"Group name: %s\n",name);
#endif

	if (data->options.extra_verbose) {
		fprintf(stdout,"Checking solid angle grid group with name %s\n", name);
	} 


	//open group
	group_id = H5Gopen(g_id,name, H5P_DEFAULT);
	if (group_id < 0) {
		fprintf(stderr,"Error opening group %s\n",name);
		return -1;
	}

	//open xmi_input_string
	dset_id = H5Dopen(group_id, "xmi_input_string", H5P_DEFAULT);
	dspace_id = H5Dget_space(dset_id);
	H5Sget_simple_extent_dims(dspace_id, dims_string, NULL);
	xmi_input_string = (char *) malloc(sizeof(char)*dims_string[0]);
	H5Dread(dset_id, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,xmi_input_string );
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	if (xmi_read_input_xml_from_string(xmi_input_string, &temp_input) == 0)
		return -1;


	if (xmi_check_solid_angle_match(temp_input, data->input) == 1) {
		//match
		//read in this group completely
		xmi_free_input(temp_input);
		*(data->solid_angles) = (struct xmi_solid_angle *) malloc(sizeof(struct xmi_solid_angle));
		solid_angles = *(data->solid_angles);
		solid_angles->xmi_input_string  = xmi_input_string;


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
		solid_angles->solid_angles = (double *) malloc(sizeof(double)*dims[0]*dims[1]);
		solid_angles->grid_dims_r_n = dims[1];
		solid_angles->grid_dims_theta_n = dims[0];

		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,solid_angles->solid_angles );
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

#if DEBUG == 2
		fprintf(stdout,"solid angles processed\n");
#endif


		dset_id = H5Dopen(group_id, "grid_dims_r_vals", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		solid_angles->grid_dims_r_vals = (double *) malloc(sizeof(double)*dims[1]);
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,solid_angles->grid_dims_r_vals );
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dset_id = H5Dopen(group_id, "grid_dims_theta_vals", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		solid_angles->grid_dims_theta_vals = (double *) malloc(sizeof(double)*dims[0]);
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,solid_angles->grid_dims_theta_vals );
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		H5Gclose(group_id);
		if (data->options.extra_verbose)
			fprintf(stdout,"Match in solid angle grid\n");
	}
	else {
		//no match -> continue looking...
		H5Gclose(group_id);
		xmi_free_input(temp_input);
		free(xmi_input_string);
		if (data->options.extra_verbose)
			fprintf(stdout,"No match in solid angle grid\n");
		return 0;
	}
	
	return 1;
}
//A -> from HDF5 file (existing - old)
//B -> from XMSI file (new)
int xmi_check_solid_angle_match(struct xmi_input *A, struct xmi_input *B) {
	int i;
	double *thickness_along_Z_a, *thickness_along_Z_b;
	double *Z_coord_begin_a, *Z_coord_begin_b;
	double *Z_coord_end_a, *Z_coord_end_b;

	//composition
	//new approach: compare extremes of the layer system
	xmi_normalize_vector_double(A->geometry->n_sample_orientation, 3);
	xmi_normalize_vector_double(B->geometry->n_sample_orientation, 3);
	thickness_along_Z_a = (double *) malloc(sizeof(double) * A->composition->n_layers);
	Z_coord_begin_a = (double *) malloc(sizeof(double) * A->composition->n_layers);
	Z_coord_end_a = (double *) malloc(sizeof(double) * A->composition->n_layers);
	thickness_along_Z_b = (double *) malloc(sizeof(double) * B->composition->n_layers);
	Z_coord_begin_b = (double *) malloc(sizeof(double) * B->composition->n_layers);
	Z_coord_end_b = (double *) malloc(sizeof(double) * B->composition->n_layers);


	for (i = 0 ; i < A->composition->n_layers ; i++) {
		thickness_along_Z_a[i] = fabs(A->composition->layers[i].thickness/A->geometry->n_sample_orientation[2]);
	}
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		thickness_along_Z_b[i] = fabs(B->composition->layers[i].thickness/B->geometry->n_sample_orientation[2]);
	}

	Z_coord_begin_a[A->composition->reference_layer-1] = 0.0;
	Z_coord_end_a[A->composition->reference_layer-1] = thickness_along_Z_a[A->composition->reference_layer-1];

	for (i = A->composition->reference_layer-1+1 ; i < A->composition->n_layers ; i++) {
		Z_coord_begin_a[i] = Z_coord_end_a[i-1];
		Z_coord_end_a[i] = Z_coord_begin_a[i]+thickness_along_Z_a[i];
	}

	for (i = A->composition->reference_layer-1-1 ; i == 0 ; i--) {
		Z_coord_end_a[i] = Z_coord_begin_a[i+1];
		Z_coord_begin_a[i] = Z_coord_end_a[i]-thickness_along_Z_a[i];
	}

	Z_coord_begin_b[B->composition->reference_layer-1] = 0.0;
	Z_coord_end_b[B->composition->reference_layer-1] = thickness_along_Z_b[B->composition->reference_layer-1];

	for (i = B->composition->reference_layer-1+1 ; i < B->composition->n_layers ; i++) {
		Z_coord_begin_b[i] = Z_coord_end_b[i-1];
		Z_coord_end_b[i] = Z_coord_begin_b[i]+thickness_along_Z_b[i];
	}

	for (i = B->composition->reference_layer-1-1 ; i == 0 ; i--) {
		Z_coord_end_b[i] = Z_coord_begin_b[i+1];
		Z_coord_begin_b[i] = Z_coord_end_b[i]-thickness_along_Z_b[i];
	}



	//calculate absorption coefficients
	double *mu_a;
	double *mu_b;

	double Pabs_a, Pabs_b;
	double S1_a, S1_b, S2_a, S2_b;
	double sum;
	double R1 = 0.00001;
	double R2 = 0.99999;
	double myln;
	int m;
	int j;

#if DEBUG == 1
	g_fprintf(stdout, "energy A: %lf\n",A->excitation->discrete[0].energy);
	g_fprintf(stdout, "energy B: %lf\n",B->excitation->discrete[0].energy);
	g_fprintf(stdout, "thickness 0 A:%lf\n",thickness_along_Z_a[0]);
	g_fprintf(stdout, "thickness 0 b:%lf\n",thickness_along_Z_b[0]);
#endif



	//S1
	//low energy limit
	mu_a = (double *) malloc(sizeof(double)*A->composition->n_layers);
	sum = 0.0;
	for (i = 0 ; i < A->composition->n_layers ; i++) {
		mu_a[i] = 0.0;
		for (j = 0 ; j < A->composition->layers[i].n_elements ; j++) {
			mu_a[i] += CS_Total_Kissel(A->composition->layers[i].Z[j], (float) A->excitation->discrete[0].energy)*A->composition->layers[i].weight[j];
		}
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
	}
	Pabs_a = 1.0 - exp(-1.0*sum);
	myln = -1.0*log(1.0-R1*Pabs_a);

	sum=0.0;
	for (i = 0 ; i < A->composition->n_layers ; i++) {
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
		if (sum > myln)
			break;
	}
	m = i;

	sum=0.0;
	for (i = 0; i < m ; i++)
		sum += (1.0 - (mu_a[i]*A->composition->layers[i].density)/(mu_a[m]*A->composition->layers[m].density))*thickness_along_Z_a[i];

	S1_a = sum + myln/(mu_a[m]*A->composition->layers[m].density) + Z_coord_begin_a[0]; 

	//S2
	//high energy limit
	mu_a = (double *) malloc(sizeof(double)*A->composition->n_layers);
	sum = 0.0;
	for (i = 0 ; i < A->composition->n_layers ; i++) {
		mu_a[i] = 0.0;
		for (j = 0 ; j < A->composition->layers[i].n_elements ; j++) {
			mu_a[i] += CS_Total_Kissel(A->composition->layers[i].Z[j], (float) A->excitation->discrete[A->excitation->n_discrete-1].energy)*A->composition->layers[i].weight[j];
		}
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
	}
	Pabs_a = 1.0 - exp(-1.0*sum);
	myln = -1.0*log(1.0-R2*Pabs_a);

	sum=0.0;
	for (i = 0 ; i < A->composition->n_layers ; i++) {
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
		if (sum > myln)
			break;
	}
	m = i;

	sum=0.0;
	for (i = 0; i < m ; i++)
		sum += (1.0 - (mu_a[i]*A->composition->layers[i].density)/(mu_a[m]*A->composition->layers[m].density))*thickness_along_Z_a[i];

	S2_a = sum + myln/(mu_a[m]*A->composition->layers[m].density) + Z_coord_begin_a[0]; 

	free(mu_a);


	//S1
	//low energy limit
	mu_b = (double *) malloc(sizeof(double)*B->composition->n_layers);
	sum = 0.0;
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		mu_b[i] = 0.0;
		for (j = 0 ; j < B->composition->layers[i].n_elements ; j++) {
			mu_b[i] += CS_Total_Kissel(B->composition->layers[i].Z[j], (float) B->excitation->discrete[0].energy)*B->composition->layers[i].weight[j];
		}
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
	}
	Pabs_b = 1.0 - exp(-1.0*sum);
	myln = -1.0*log(1.0-R1*Pabs_b);

#if DEBUG == 1
	g_fprintf(stdout,"S1 Pabs_b: %lf\n", Pabs_b);
	g_fprintf(stdout,"S1 myln: %lf\n", myln);
#endif

	sum=0.0;
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
		if (sum > myln)
			break;
	}
	m = i;

	sum=0.0;
	for (i = 0; i < m ; i++)
		sum += (1.0 - (mu_b[i]*B->composition->layers[i].density)/(mu_b[m]*B->composition->layers[m].density))*thickness_along_Z_b[i];

#if DEBUG == 1
	g_fprintf(stdout,"S1 my_sum: %lf\n", sum);
	g_fprintf(stdout,"S1 my_sum plus: %lf\n", sum+ myln/(mu_b[m]*B->composition->layers[m].density));
	g_fprintf(stdout,"S1 Z_coord_begin: %lf\n",Z_coord_begin_b[0]);
#endif
	S1_b = sum + myln/(mu_b[m]*B->composition->layers[m].density) + Z_coord_begin_b[0];; 

	//S2
	//high energy limit
	mu_b = (double *) malloc(sizeof(double)*B->composition->n_layers);
	sum = 0.0;
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		mu_b[i] = 0.0;
		for (j = 0 ; j < B->composition->layers[i].n_elements ; j++) {
			mu_b[i] += CS_Total_Kissel(B->composition->layers[i].Z[j], (float) B->excitation->discrete[B->excitation->n_discrete-1].energy)*B->composition->layers[i].weight[j];
		}
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
	}
	Pabs_b = 1.0 - exp(-1.0*sum);
	myln = -1.0*log(1.0-R2*Pabs_b);
#if DEBUG == 1
	g_fprintf(stdout,"S2 Pabs_b: %lf\n", Pabs_b);
	g_fprintf(stdout,"S2 myln: %lf\n", myln);
#endif

	sum=0.0;
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
		if (sum > myln)
			break;
	}
	m = i;

	sum=0.0;
	for (i = 0; i < m ; i++)
		sum += (1.0 - (mu_b[i]*B->composition->layers[i].density)/(mu_b[m]*B->composition->layers[m].density))*thickness_along_Z_b[i];

	S2_b = sum + myln/(mu_b[m]*B->composition->layers[m].density) + Z_coord_begin_b[0]; 

	free(mu_b);

#if DEBUG == 1
	fprintf(stdout, "S2_b: %lg\n", S2_b);
	fprintf(stderr, "Z_coord_end_b: %lg\n", Z_coord_end_b[B->composition->n_layers-1]);
	fprintf(stdout, "S2_a: %lg\n", S2_a);
	fprintf(stderr, "Z_coord_end_a: %lg\n", Z_coord_end_a[A->composition->n_layers-1]);
	fprintf(stdout, "S1_b: %lg\n", S1_b);
	fprintf(stderr, "Z_coord_begin_b: %lg\n", Z_coord_begin_b[0]);
	fprintf(stdout, "S1_a: %lg\n", S1_a);
	fprintf(stderr, "Z_coord_begin_a: %lg\n", Z_coord_begin_a[0]);
#endif


	//if (Z_coord_end_a[A->composition->n_layers-1] - Z_coord_end_b[B->composition->n_layers-1] < -0.0001 || Z_coord_begin_a[0] - Z_coord_begin_b[0] > 0.0001) {
	if (S2_a - S2_b < -0.0001 || S1_a - S1_b > 0.0001) {
		free(thickness_along_Z_a);
		free(thickness_along_Z_b);
		free(Z_coord_begin_a);
		free(Z_coord_end_a);
		free(Z_coord_begin_b);
		free(Z_coord_end_b);
		return 0;
	}

	free(thickness_along_Z_a);
	free(thickness_along_Z_b);
	free(Z_coord_begin_a);
	free(Z_coord_end_a);
	free(Z_coord_begin_b);
	free(Z_coord_end_b);


	//geometry
#define XMI_IF_COMPARE_GEOMETRY(a) if (fabsl(A->geometry->a - B->geometry->a)/A->geometry->a > XMI_COMPARE_THRESHOLD){\
	return 0;\
	}	
#define XMI_IF_COMPARE_GEOMETRY2(a) if (fabsl(A->geometry->a - B->geometry->a) > XMI_COMPARE_THRESHOLD){\
	return 0;\
	}	

	//should compare normalized orientations...
	//XMI_IF_COMPARE_GEOMETRY2(n_sample_orientation[0])
	//XMI_IF_COMPARE_GEOMETRY2(n_sample_orientation[1])
	//XMI_IF_COMPARE_GEOMETRY2(n_sample_orientation[2])
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

int xmi_find_solid_angle_match(char *hdf5_file, struct xmi_input *A, struct xmi_solid_angle **rv, struct xmi_main_options options) {

	hid_t file_id;
	struct xmi_solid_angles_data data;
	herr_t iterate_rv;

	//open the hdf5 file read-only!
	file_id = H5Fopen(hdf5_file, H5F_ACC_RDONLY , H5P_DEFAULT);
	if (file_id < 0 ) {
		fprintf(stderr,"Cannot open file %s for reading\n",hdf5_file);
		return 0;
	}

	//version check!
	hid_t root_group_id;
	hid_t attribute_id;

	root_group_id = H5Gopen(file_id, "/", H5P_DEFAULT);
	attribute_id = H5Aopen(root_group_id, "version", H5P_DEFAULT);
	if (attribute_id < 0) {
		//attribute does not exist
		g_fprintf(stderr, "Solid angles file %s does not contain the version tag\n", hdf5_file);
		g_fprintf(stderr, "The file will be deleted and recreated\n");
		
		H5Gclose(root_group_id);
		H5Fclose(file_id);
		if(g_unlink(hdf5_file) == -1) {
			g_fprintf(stderr,"Could not delete file %s... Fatal error\n", hdf5_file);
			return 0;
		}
		if (xmi_get_solid_angle_file(&hdf5_file, 1) == 0) {
			return 0;
		}
		*rv = NULL;
		return 1;
	}
	//attribute exists -> let's read it
	float version;
	H5Aread(attribute_id, H5T_NATIVE_FLOAT, &version);
	H5Aclose(attribute_id);
	H5Gclose(root_group_id);

	if (version < MIN_VERSION) {
		//too old -> delete file
		g_fprintf(stderr, "Solid angles file %s is not compatible with this version of XMI-MSIM\n", hdf5_file);
		g_fprintf(stderr, "The file will be deleted and recreated\n");

		H5Fclose(file_id);
		if(g_unlink(hdf5_file) == -1) {
			g_fprintf(stderr,"Could not delete file %s... Fatal error\n", hdf5_file);
			return 0;
		}
		if (xmi_get_solid_angle_file(&hdf5_file, 1) == 0) {
			return 0;
		}
		*rv = NULL;
		return 1;
	}

	data.solid_angles = rv;
	data.input = A;
	data.options = options;

	iterate_rv = H5Literate(file_id, H5_INDEX_NAME, H5_ITER_INC, NULL, xmi_read_single_solid_angle,(void *) &data);

	if (iterate_rv < 0)
		return 0;
	else if(iterate_rv == 0) {
		*rv = NULL;
	}
	

	if (H5Fclose(file_id) < 0) {
		g_fprintf(stderr, "Error closing %s... Fatal error\n", hdf5_file);
		return 0;
	}
#if DEBUG == 1
	else
		g_fprintf(stderr,"HDF5 closed succesfully\n");
#endif
	return 1;
}


void xmi_free_solid_angle(struct xmi_solid_angle *solid_angle) {

	free(solid_angle->solid_angles);
	free(solid_angle->grid_dims_r_vals);
	free(solid_angle->grid_dims_theta_vals);
	free(solid_angle->xmi_input_string);
}

int xmi_get_solid_angle_file(char **filePtr, int create_file) {
	//behavior is very much platform dependent
	//general rule
	//Linux: use g_get_user_data_dir
	//Windows: use g_get_user_data_dir BUT 
	//since it creates problems when spawned, use xmimsim.exe commandline for path
	//Mac OS X: use g_get_user_data_dir unless when packaged in App

	//create the file if it doesn't exist already!

	char *file = *filePtr;
	char *dir;
#ifdef MAC_INTEGRATION
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];
#endif



	if (file == NULL) {
#ifdef MAC_INTEGRATION
		NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask,TRUE);
		NSString *documentsDirectory = [paths objectAtIndex:0];
		const gchar *data_dir = [documentsDirectory cStringUsingEncoding:NSUTF8StringEncoding];
#else
		const gchar *data_dir = g_get_user_data_dir();
#endif


		file = (char *) malloc(sizeof(char)*(strlen(data_dir)+strlen(G_DIR_SEPARATOR_S "XMI-MSIM" G_DIR_SEPARATOR_S "xmimsim-solid-angles.h5")+1));
		strcpy(file, data_dir);
		strcat(file,G_DIR_SEPARATOR_S "XMI-MSIM" G_DIR_SEPARATOR_S "xmimsim-solid-angles.h5");
		*filePtr = file;
	}

	//check if file exists
	if (!g_file_test(file, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_REGULAR) && create_file) {
		//create underlying directory if necessary
		dir = g_path_get_dirname(file);
		if (g_mkdir_with_parents(dir,S_IRUSR | S_IWUSR | S_IXUSR) == -1) {
			fprintf(stderr,"Could not create directory: %s\n",dir);
			return 0;
		}
		g_free(dir);
		return xmi_create_empty_solid_angle_hdf5_file(file);
	}
#ifdef MAC_INTEGRATION
	[pool drain];
#endif

	return 1;
}

