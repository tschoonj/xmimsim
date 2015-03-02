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
#include "xmi_main.h"
#include <glib.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <glib/gstdio.h>
#include <gmodule.h>



struct xmi_solid_angles_data{
	struct xmi_solid_angle **solid_angles;
	struct xmi_input *input;
	struct xmi_main_options options;
};

#ifdef G_OS_WIN32
  #include "xmi_registry_win.h"
#endif

#ifndef XMIMSIM_CL
  #include <hdf5.h>
  #include "xmi_aux.h"
  #include "xmi_xml.h"
  #include <sys/stat.h>
  #include <xraylib.h>
  #ifdef G_OS_WIN32
  	#include "windows.h"
  #endif
  #ifdef MAC_INTEGRATION
	#import <Foundation/Foundation.h>
	#include "xmi_resources_mac.h"
  #endif



  static herr_t xmi_read_single_solid_angle(hid_t g_id, const char *name, const H5L_info_t *info, void *op_data);

  extern void xmi_solid_angle_calculation_f(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options);
//static void xmi_solid_angle_calculation_cl(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options);

  typedef int (*XmiSolidAngleCalculation) (xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options);
#else

  #ifdef HAVE_OPENCL_CL_H
	#include <OpenCL/cl.h>
  #elif defined(HAVE_CL_CL_H)
	#include <CL/cl.h>
  #endif
  #ifdef MAC_INTEGRATION
	#include "xmi_resources_mac.h"
  #endif


  #define RANGE_DIVIDER 8
  #define XMI_OPENCL_MAJOR 1
  #define XMI_OPENCL_MINOR 1

  extern void xmi_solid_angle_inputs_f(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, int *collimator_present, float *detector_radius, float *collimator_radius, float *collimator_height);

  extern long hits_per_single;
#endif


#ifndef XMIMSIM_CL
void xmi_solid_angle_calculation(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options xmo) {

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	XmiSolidAngleCalculation xmi_solid_angle_calculation_cl;
	GModule *module;
	gchar *module_path;
	gchar *opencl_lib;

	if (xmo.use_opencl) {
		//try to open the module
		if (!g_module_supported()) {
			fprintf(stderr,"No module support on this platform\n");
			goto fallback;
		}

#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_OPENCL_LIB,&opencl_lib) == 0)
			goto fallback;
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_OPENCL_LIB,&opencl_lib) == 0)
			goto fallback;
#else
		opencl_lib = g_strdup(XMI_OPENCL_LIB);
#endif

		module_path = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s.%s",opencl_lib,"xmimsim-cl",G_MODULE_SUFFIX);
		module = g_module_open(module_path, 0);
		g_free(module_path);
		g_free(opencl_lib);
		if (!module) {
			fprintf(stderr,"Could not open xmimsim-cl: %s\n", g_module_error());
			goto fallback;
		}
		if (!g_module_symbol(module, "xmi_solid_angle_calculation_cl", (gpointer *) &xmi_solid_angle_calculation_cl)) {
			fprintf(stderr,"Error retrieving xmi_solid_angle_calculation_cl in xmimsim-cl: %s\n", g_module_error());
			goto fallback;
		}
		if (xmi_solid_angle_calculation_cl != NULL) {
			int rv = xmi_solid_angle_calculation_cl(inputFPtr, solid_angle, strdup(input_string), xmo);
			if (rv == 0) {
				fprintf(stderr,"OpenCL calculation failed: fallback to Fortran implementation\n");
				goto fallback;
			}
		}
		else {
			fprintf(stderr,"xmi_solid_angle_calculation_cl is NULL\n");
			goto fallback;
		}
		if (!g_module_close(module)) {
			fprintf(stderr,"Warning: could not close module xmimsim-cl: %s\n",g_module_error());
		}
		return;
	
	}
fallback:
#endif
	xmi_solid_angle_calculation_f(inputFPtr, solid_angle, strdup(input_string), xmo);
}


int xmi_create_empty_solid_angle_hdf5_file(char *hdf5_file) {

	hid_t file_id;   /* file identifier */
	hid_t root_group_id;	
	hid_t attribute_id;
	hid_t dataspace_id;

	double version = g_ascii_strtod(VERSION, NULL);
	

	/* Create a new file using default properties. */
	hid_t gcpl = H5Pcreate (H5P_FILE_CREATE);
	H5Pset_link_creation_order( gcpl, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED );
	file_id = H5Fcreate(hdf5_file, H5F_ACC_TRUNC, gcpl, H5P_DEFAULT);
	H5Pclose (gcpl);

	if (file_id < 0) {
		fprintf(stderr,"Could not create solid angle HDF5 file %s\n",hdf5_file);
		return 0;
	}
	root_group_id = H5Gopen(file_id, "/", H5P_DEFAULT);

	dataspace_id = H5Screate(H5S_SCALAR);
	attribute_id = H5Acreate(root_group_id, "version", H5T_NATIVE_DOUBLE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attribute_id, H5T_NATIVE_DOUBLE, &version);
	
	H5Aclose(attribute_id);
	H5Sclose(dataspace_id);

	//write kind attribute
	dataspace_id = H5Screate(H5S_SCALAR);
	hid_t atype = H5Tcopy(H5T_C_S1);
	H5Tset_size(atype, strlen("XMI_HDF5_SOLID_ANGLES")+1);
	H5Tset_strpad(atype,H5T_STR_NULLTERM);
	attribute_id = H5Acreate2(root_group_id, "kind", atype, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attribute_id, atype, "XMI_HDF5_SOLID_ANGLES");
	
	H5Tclose(atype);
	H5Aclose(attribute_id);
	H5Sclose(dataspace_id);


	H5Gclose(root_group_id);


	/* Terminate access to the file. */
	H5Fclose(file_id); 
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
	hid_t gcpl = H5Pcreate (H5P_GROUP_CREATE);
	H5Pset_link_creation_order( gcpl, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED );
	group_id = H5Gcreate(file_id, buffer, H5P_DEFAULT, gcpl, H5P_DEFAULT);
	H5Pclose (gcpl);

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



static herr_t xmi_read_single_solid_angle(hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
	hid_t dset_id, dspace_id;
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
	
	double energy;

	//S1
	//low energy limit
	mu_a = (double *) malloc(sizeof(double)*A->composition->n_layers);
	sum = 0.0;
	if (A->excitation->n_continuous > 1 && A->excitation->n_discrete > 0) {
		energy = MIN(A->excitation->continuous[0].energy, A->excitation->discrete[0].energy);
	}
	else if (A->excitation->n_continuous > 1)
		energy = A->excitation->continuous[0].energy;
	else
		energy = A->excitation->discrete[0].energy;

	for (i = 0 ; i < A->composition->n_layers ; i++) {
		mu_a[i] = 0.0;
		for (j = 0 ; j < A->composition->layers[i].n_elements ; j++) {
			mu_a[i] += CS_Total_Kissel(A->composition->layers[i].Z[j], (float) energy)*A->composition->layers[i].weight[j];
		}
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
	}
	Pabs_a = -1.0*expm1(-1.0*sum);
	myln = -1.0*log1p(-1.0*R1*Pabs_a);

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
	free(mu_a);
	mu_a = (double *) malloc(sizeof(double)*A->composition->n_layers);
	sum = 0.0;
	if (A->excitation->n_continuous > 1 && A->excitation->n_discrete > 0) {
		energy = MAX(A->excitation->continuous[A->excitation->n_continuous-1].energy, A->excitation->discrete[A->excitation->n_discrete-1].energy);
	}
	else if (A->excitation->n_continuous > 1)
		energy = A->excitation->continuous[A->excitation->n_continuous-1].energy;
	else
		energy = A->excitation->discrete[A->excitation->n_discrete-1].energy;
	for (i = 0 ; i < A->composition->n_layers ; i++) {
		mu_a[i] = 0.0;
		for (j = 0 ; j < A->composition->layers[i].n_elements ; j++) {
			mu_a[i] += CS_Total_Kissel(A->composition->layers[i].Z[j], (float) energy)*A->composition->layers[i].weight[j];
		}
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
	}
	Pabs_a = -1.0*expm1(-1.0*sum);
	myln = -1.0*log1p(-1.0*R2*Pabs_a);

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
	if (B->excitation->n_continuous > 0 && B->excitation->n_discrete > 0) {
		energy = MIN(B->excitation->continuous[0].energy, B->excitation->discrete[0].energy);
	}
	else if (B->excitation->n_continuous > 0)
		energy = B->excitation->continuous[0].energy;
	else
		energy = B->excitation->discrete[0].energy;
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		mu_b[i] = 0.0;
		for (j = 0 ; j < B->composition->layers[i].n_elements ; j++) {
			mu_b[i] += CS_Total_Kissel(B->composition->layers[i].Z[j], (float) energy)*B->composition->layers[i].weight[j];
		}
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
	}
	Pabs_b = -1.0*expm1(-1.0*sum);
	myln = -1.0*log1p(-1.0*R1*Pabs_b);

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
	free(mu_b);
	mu_b = (double *) malloc(sizeof(double)*B->composition->n_layers);
	sum = 0.0;
	if (B->excitation->n_continuous > 1 && B->excitation->n_discrete > 0) {
		energy = MAX(B->excitation->continuous[B->excitation->n_continuous-1].energy, B->excitation->discrete[B->excitation->n_discrete-1].energy);
	}
	else if (B->excitation->n_continuous > 1)
		energy = B->excitation->continuous[B->excitation->n_continuous-1].energy;
	else
		energy = B->excitation->discrete[B->excitation->n_discrete-1].energy;
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		mu_b[i] = 0.0;
		for (j = 0 ; j < B->composition->layers[i].n_elements ; j++) {
			mu_b[i] += CS_Total_Kissel(B->composition->layers[i].Z[j], (float) energy)*B->composition->layers[i].weight[j];
		}
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
	}
	Pabs_b = -1.0*expm1(-1.0*sum);
	myln = -1.0*log1p(-1.0*R2*Pabs_b);
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

	//check for kind attribute
	attribute_id = H5Aopen(root_group_id, "kind", H5P_DEFAULT);
	if (attribute_id < 0) {
		//attribute does not exist
		g_fprintf(stderr, "Solid angles file %s does not contain the kind attribute\n", hdf5_file);
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
	hid_t atype = H5Aget_type(attribute_id);
	hid_t atype_mem = H5Tget_native_type(atype, H5T_DIR_ASCEND);
	char kind[512];
	H5Aread(attribute_id, atype_mem, kind);
	H5Tclose(atype_mem);
	H5Tclose(atype);
	H5Aclose(attribute_id);

	if (strcmp(kind, "XMI_HDF5_SOLID_ANGLES") != 0) {
		g_fprintf(stderr, "Solid angles file %s does not have the correct kind attribute\n", hdf5_file);
		g_fprintf(stderr, "Expected XMI_HDF5_SOLID_ANGLES but found %s\n", kind);
		g_fprintf(stderr, "Aborting\n");

		H5Gclose(root_group_id);
		H5Fclose(file_id);
		return 0;
	} 


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
	double version;
	H5Aread(attribute_id, H5T_NATIVE_DOUBLE, &version);
	H5Aclose(attribute_id);
	H5Gclose(root_group_id);

	if (version < XMI_SOLID_ANGLES_MIN_VERSION) {
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

	iterate_rv = H5Literate(file_id,H5_INDEX_CRT_ORDER, H5_ITER_DEC, NULL, xmi_read_single_solid_angle,(void *) &data);

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
	//this is potentially dangerous... some of this memory is allocated by fortran...
	free(solid_angle->solid_angles);
	free(solid_angle->grid_dims_r_vals);
	free(solid_angle->grid_dims_theta_vals);
	free(solid_angle->xmi_input_string);
	free(solid_angle);
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


#else
/*
 * Taken from http://tom.scogland.com/blog/2013/03/29/opencl-errors
 */




const char *clGetErrorString(cl_int err){
         switch(err){
             case 0: return "CL_SUCCESS";
             case -1: return "CL_DEVICE_NOT_FOUND";
             case -2: return "CL_DEVICE_NOT_AVAILABLE";
             case -3: return "CL_COMPILER_NOT_AVAILABLE";
             case -4: return "CL_MEM_OBJECT_ALLOCATION_FAILURE";
             case -5: return "CL_OUT_OF_RESOURCES";
             case -6: return "CL_OUT_OF_HOST_MEMORY";
             case -7: return "CL_PROFILING_INFO_NOT_AVAILABLE";
             case -8: return "CL_MEM_COPY_OVERLAP";
             case -9: return "CL_IMAGE_FORMAT_MISMATCH";
             case -10: return "CL_IMAGE_FORMAT_NOT_SUPPORTED";
             case -11: return "CL_BUILD_PROGRAM_FAILURE";
             case -12: return "CL_MAP_FAILURE";

             case -30: return "CL_INVALID_VALUE";
             case -31: return "CL_INVALID_DEVICE_TYPE";
             case -32: return "CL_INVALID_PLATFORM";
             case -33: return "CL_INVALID_DEVICE";
             case -34: return "CL_INVALID_CONTEXT";
             case -35: return "CL_INVALID_QUEUE_PROPERTIES";
             case -36: return "CL_INVALID_COMMAND_QUEUE";
             case -37: return "CL_INVALID_HOST_PTR";
             case -38: return "CL_INVALID_MEM_OBJECT";
             case -39: return "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR";
             case -40: return "CL_INVALID_IMAGE_SIZE";
             case -41: return "CL_INVALID_SAMPLER";
             case -42: return "CL_INVALID_BINARY";
             case -43: return "CL_INVALID_BUILD_OPTIONS";
             case -44: return "CL_INVALID_PROGRAM";
             case -45: return "CL_INVALID_PROGRAM_EXECUTABLE";
             case -46: return "CL_INVALID_KERNEL_NAME";
             case -47: return "CL_INVALID_KERNEL_DEFINITION";
             case -48: return "CL_INVALID_KERNEL";
             case -49: return "CL_INVALID_ARG_INDEX";
             case -50: return "CL_INVALID_ARG_VALUE";
             case -51: return "CL_INVALID_ARG_SIZE";
             case -52: return "CL_INVALID_KERNEL_ARGS";
             case -53: return "CL_INVALID_WORK_DIMENSION";
             case -54: return "CL_INVALID_WORK_GROUP_SIZE";
             case -55: return "CL_INVALID_WORK_ITEM_SIZE";
             case -56: return "CL_INVALID_GLOBAL_OFFSET";
             case -57: return "CL_INVALID_EVENT_WAIT_LIST";
             case -58: return "CL_INVALID_EVENT";
             case -59: return "CL_INVALID_OPERATION";
             case -60: return "CL_INVALID_GL_OBJECT";
             case -61: return "CL_INVALID_BUFFER_SIZE";
             case -62: return "CL_INVALID_MIP_LEVEL";
             case -63: return "CL_INVALID_GLOBAL_WORK_SIZE";
             default: return "Unknown OpenCL error";
         }
}



#define OPENCL_ERROR(name) if (status != CL_SUCCESS) { \
	fprintf(stderr,"OpenCL error %s for function %s on line %i\n",clGetErrorString(status),#name,__LINE__);\
	return 0;\
	} 

G_MODULE_EXPORT int xmi_solid_angle_calculation_cl(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options xmo) {

	cl_int status;
	char info[1000];
	size_t info_size = 1000;

	cl_uint numPlatforms = 0;
	cl_platform_id *platforms = NULL;
	status = clGetPlatformIDs(0, NULL, &numPlatforms);
	OPENCL_ERROR(clGetPlatformIDs)
	
	platforms = (cl_platform_id *) malloc(sizeof(cl_platform_id)*numPlatforms);
	status = clGetPlatformIDs(numPlatforms, platforms, NULL);
	OPENCL_ERROR(clGetPlatformIDs)



	if (numPlatforms == 0) {
		fprintf(stderr,"No OpenCL platform detected\n");
		return 0;
	}

	int i, j;

	/*for (i = 0 ; i < numPlatforms ; i++) {
		status = clGetPlatformInfo(platforms[i], CL_PLATFORM_VERSION, info_size, info, NULL);
		OPENCL_ERROR(clGetPlatformInfo)
		fprintf(stdout,"OpenCL platform version %s\n", info);
	}*/


	cl_uint numDevices = 0;
	cl_device_id *devices = NULL;
	cl_device_id device;
	status = clGetDeviceIDs(platforms[0], CL_DEVICE_TYPE_GPU, 0, NULL, &numDevices);
	OPENCL_ERROR(clGetDeviceIDs)

	if (numDevices == 0) {
		fprintf(stderr,"No OpenCL GPU devices detected\n");
		return 0;
	}

	devices = (cl_device_id *) malloc(sizeof(cl_device_id)*numDevices);
	status = clGetDeviceIDs(platforms[0], CL_DEVICE_TYPE_GPU, numDevices, devices, NULL);
	OPENCL_ERROR(clGetDeviceIDs)

	size_t max_work_group_size = 0;

	//fprintf(stdout,"Number of devices found: %i\n", numDevices);
	for (i = 0 ; i < numDevices ; i++) {
		//check implementation... should be 1.1
		status = clGetDeviceInfo(devices[i], CL_DEVICE_VERSION, info_size, info, NULL);
		OPENCL_ERROR(clGetDeviceInfo)

		//parse string;
		int cl_minor, cl_major;
		
		sscanf(info, "OpenCL %i.%i ",&cl_major, &cl_minor);
		
		if (!(cl_major >= XMI_OPENCL_MAJOR && cl_minor >= XMI_OPENCL_MINOR)) {
			continue;
		}

		status = clGetDeviceInfo(devices[i], CL_DEVICE_NAME, info_size, info, NULL);
		OPENCL_ERROR(clGetDeviceInfo)
		fprintf(stdout,"OpenCL device %i name %s\n", i, info);

		size_t max_work_group_size_temp;
		status = clGetDeviceInfo(devices[i], CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(size_t), &max_work_group_size_temp, NULL);
		OPENCL_ERROR(clGetDeviceInfo)
		fprintf(stdout,"OpenCL device %i max_work_group_size %i\n", i, max_work_group_size_temp);
		if (max_work_group_size_temp > max_work_group_size) {
			device = devices[i];
			max_work_group_size = max_work_group_size_temp;
		}
	}

	if (max_work_group_size == 0) {
		fprintf(stderr, "No suitable OpenCL GPU devices detected\n");
		return 0;
	}


	cl_context context = NULL;
	context = clCreateContext(NULL, numDevices, devices, NULL, NULL, &status);
	OPENCL_ERROR(clCreateContext)

	cl_command_queue cmdQueue;
	cmdQueue = clCreateCommandQueue(context, device, 0, &status);
	OPENCL_ERROR(clCreateCommandQueue)

	//start assembling our input
	int collimator_present;
	float detector_radius;
	float collimator_radius;
	float collimator_height;

	xmi_solid_angle_inputs_f(inputFPtr, solid_angle, &collimator_present, &detector_radius, &collimator_radius, &collimator_height);

	struct xmi_solid_angle *sa = *solid_angle;

	//fprintf(stdout,"grid_dims_r_n: %li\n", sa->grid_dims_r_n);
	//fprintf(stdout,"grid_dims_theta_n: %li\n", sa->grid_dims_theta_n);
	//fprintf(stdout,"hits_per_single: %li\n",hits_per_single);

	//other arguments needed
	//	detector%collimator_present
	//	detector%detector_radius
	//	detector%collimator_radius
	//	detector%collimator_height
	//	hits_per_single
	//

	float *grid_dims_r_vals_float = (float *) malloc(sizeof(float)*sa->grid_dims_r_n);
	float *grid_dims_theta_vals_float = (float *) malloc(sizeof(float)*sa->grid_dims_theta_n);

	for (i = 0 ; i < sa->grid_dims_r_n ; i++)
		grid_dims_r_vals_float[i] = (float) sa->grid_dims_r_vals[i];

	for (i = 0 ; i < sa->grid_dims_theta_n ; i++)
		grid_dims_theta_vals_float[i] = (float) sa->grid_dims_theta_vals[i];

	float *solid_angles_float = (float *) malloc(sizeof(float)*sa->grid_dims_r_n*sa->grid_dims_theta_n);


	//check device constant memory
	cl_ulong constant_buffer_memory;
	status = clGetDeviceInfo(device, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, sizeof(cl_ulong), &constant_buffer_memory, NULL);
	OPENCL_ERROR(clGetDeviceInfo)
	//fprintf(stdout,"Max constant buffer size: %lu\n", constant_buffer_memory);
	if (constant_buffer_memory < (sizeof(float)*sa->grid_dims_r_n+sizeof(float)*sa->grid_dims_theta_n)) {
		fprintf(stdout,"OpenCL constant buffer size too small\n");
		return 0;
	}

//#define DEBUG

	cl_event writeEventA = 0, writeEventB = 0;
	cl_event eventlist[2] = {writeEventA, writeEventB};
#ifndef DEBUG
	cl_mem grid_dims_r_vals_cl = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float)*sa->grid_dims_r_n, NULL, &status);
	OPENCL_ERROR(clCreateBuffer)
	cl_mem grid_dims_theta_vals_cl = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float)*sa->grid_dims_theta_n, NULL, &status);
	OPENCL_ERROR(clCreateBuffer)
	cl_mem solid_angles_cl = clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(float)*sa->grid_dims_r_n*sa->grid_dims_theta_n, NULL, &status);
	OPENCL_ERROR(clCreateBuffer)
	status = clEnqueueWriteBuffer(cmdQueue, grid_dims_r_vals_cl, CL_TRUE, 0, sizeof(float)*sa->grid_dims_r_n, grid_dims_r_vals_float, 0, NULL, NULL);
	OPENCL_ERROR(clEnqueueWriteBuffer)
	status = clEnqueueWriteBuffer(cmdQueue, grid_dims_theta_vals_cl, CL_TRUE, 0, sizeof(float)*sa->grid_dims_theta_n, grid_dims_theta_vals_float, 0, NULL, NULL);
	OPENCL_ERROR(clEnqueueWriteBuffer)
#else
	float *input1 = (float *) malloc(sizeof(float));
	float *input2 = (float *) malloc(sizeof(float));
	input1[0] = sa->grid_dims_r_vals[199];
	input2[0] = sa->grid_dims_theta_vals[199];

	cl_mem grid_dims_r_vals_cl = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float), NULL, &status);
	OPENCL_ERROR(clCreateBuffer)
	cl_mem grid_dims_theta_vals_cl = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float), NULL, &status);
	OPENCL_ERROR(clCreateBuffer)
	cl_mem solid_angles_cl = clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(float)*sa->grid_dims_r_n*sa->grid_dims_theta_n, NULL, &status);
	OPENCL_ERROR(clCreateBuffer)
	status = clEnqueueWriteBuffer(cmdQueue, grid_dims_r_vals_cl, CL_FALSE, 0, sizeof(float), input1, 0, NULL, NULL);
	OPENCL_ERROR(clEnqueueWriteBuffer)
	status = clEnqueueWriteBuffer(cmdQueue, grid_dims_theta_vals_cl, CL_FALSE, 0, sizeof(float), input2, 0, NULL, NULL);
	OPENCL_ERROR(clEnqueueWriteBuffer)
#endif

	//compile kernel
	//read the kernel code first from file
	//
	//
	

	gchar *opencl_code;

#ifdef G_OS_WIN32
	if (xmi_registry_win_query(XMI_REGISTRY_WIN_OPENCL_CODE,&opencl_code) == 0)
		return 0;
#elif defined(MAC_INTEGRATION)
	if (xmi_resources_mac_query(XMI_RESOURCES_MAC_OPENCL_CODE,&opencl_code) == 0)
		return 0;
#else
	opencl_code = g_strdup(XMI_OPENCL_CODE);
#endif

	gchar *kernel_code = NULL;
	gchar *kernel_file;
	gchar *source_code = NULL;
	gsize file_length;
	
	//kernel_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "xmi_kernels.cl",opencl_code);;
	kernel_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "openclfeatures.h",opencl_code);
	if(g_file_get_contents(kernel_file, &source_code, &file_length, NULL) == FALSE) {
		g_fprintf(stderr,"Could not open %s\n",kernel_file);
		return 0;
	}
	kernel_code = g_realloc(kernel_code, sizeof(char) * (file_length+2));
	strcpy(kernel_code, source_code);
	strcat(kernel_code, "\n");
	g_free(source_code);
	g_free(kernel_file);

	kernel_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "compilerfeatures.h",opencl_code);
	if(g_file_get_contents(kernel_file, &source_code, &file_length, NULL) == FALSE) {
		g_fprintf(stderr,"Could not open %s\n",kernel_file);
		return 0;
	}
	kernel_code = g_realloc(kernel_code, sizeof(char) * (strlen(kernel_code)+file_length+2));
	strcat(kernel_code, source_code);
	strcat(kernel_code, "\n");
	g_free(source_code);
	g_free(kernel_file);

	kernel_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "sse.h",opencl_code);
	if(g_file_get_contents(kernel_file, &source_code, &file_length, NULL) == FALSE) {
		g_fprintf(stderr,"Could not open %s\n",kernel_file);
		return 0;
	}
	kernel_code = g_realloc(kernel_code, sizeof(char) * (strlen(kernel_code)+file_length+2));
	strcat(kernel_code, source_code);
	strcat(kernel_code, "\n");
	g_free(source_code);
	g_free(kernel_file);

	kernel_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "array.h",opencl_code);
	if(g_file_get_contents(kernel_file, &source_code, &file_length, NULL) == FALSE) {
		g_fprintf(stderr,"Could not open %s\n",kernel_file);
		return 0;
	}
	kernel_code = g_realloc(kernel_code, sizeof(char) * (strlen(kernel_code)+file_length+2));
	strcat(kernel_code, source_code);
	strcat(kernel_code, "\n");
	g_free(source_code);
	g_free(kernel_file);

	kernel_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "threefry.h",opencl_code);
	if(g_file_get_contents(kernel_file, &source_code, &file_length, NULL) == FALSE) {
		g_fprintf(stderr,"Could not open %s\n",kernel_file);
		return 0;
	}
	kernel_code = g_realloc(kernel_code, sizeof(char) * (strlen(kernel_code)+file_length+2));
	strcat(kernel_code, source_code);
	strcat(kernel_code, "\n");
	g_free(source_code);
	g_free(kernel_file);

	kernel_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "xmi_kernels.cl",opencl_code);
	if(g_file_get_contents(kernel_file, &source_code, &file_length, NULL) == FALSE) {
		g_fprintf(stderr,"Could not open %s\n",kernel_file);
		return 0;
	}
	kernel_code = g_realloc(kernel_code, sizeof(char) * (strlen(kernel_code)+file_length+2));
	strcat(kernel_code, source_code);
	strcat(kernel_code, "\n");
	g_free(source_code);
	g_free(kernel_file);

	g_free(opencl_code);
	
	cl_program myprog = clCreateProgramWithSource(context, 1, (const char **) &kernel_code, NULL, &status);
	OPENCL_ERROR(clCreateProgramWithSource)
	g_free(kernel_code);

	gchar *build_options = g_strdup_printf("-DRANGE_DIVIDER=%i", RANGE_DIVIDER);
	status = clBuildProgram(myprog, 0, NULL, build_options , NULL, NULL);
	g_free(build_options);
	OPENCL_ERROR(clBuildProgram)

	if (status == CL_BUILD_PROGRAM_FAILURE) {
		fprintf(stderr,"build failure\n");
		// Determine the size of the log
		size_t log_size;
		status = clGetProgramBuildInfo(myprog, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);
		OPENCL_ERROR(clGetProgramBuildInfo)
	
		// Allocate memory for the log
		char *my_log = (char *) malloc(log_size+1);
		
	        // Get the log
		status = clGetProgramBuildInfo(myprog, device, CL_PROGRAM_BUILD_LOG, log_size, my_log, NULL);
		OPENCL_ERROR(clGetProgramBuildInfo)
		
		// Print the log
		g_fprintf(stderr, "%s\n", my_log);
		return 0;;
	}
	//else {
	//	fprintf(stdout,"status: %i\n",status);
	//}

	cl_kernel mykernel = clCreateKernel(myprog, "xmi_solid_angle_calculation", &status);
	//cl_kernel mykernel = clCreateKernel(myprog, "xmi_kernel_test", &status);
	OPENCL_ERROR(clCreateKernel)

	//fprintf(stdout,"max kernel global work size: %zi %zi %zi",globalmax[0], globalmax[1], globalmax[2]);

	//exit(0);

	status = clSetKernelArg(mykernel, 0, sizeof(cl_mem), (void *) &grid_dims_r_vals_cl);
	OPENCL_ERROR(clSetKernelArg)
	status = clSetKernelArg(mykernel, 1, sizeof(cl_mem), (void *) &grid_dims_theta_vals_cl);
	OPENCL_ERROR(clSetKernelArg)
	status = clSetKernelArg(mykernel, 2, sizeof(cl_mem), (void *) &solid_angles_cl);
	OPENCL_ERROR(clSetKernelArg)
	status = clSetKernelArg(mykernel, 3, sizeof(cl_int), (void *) &collimator_present);
	OPENCL_ERROR(clSetKernelArg)
	status = clSetKernelArg(mykernel, 4, sizeof(cl_float), (void *) &detector_radius);
	OPENCL_ERROR(clSetKernelArg)
	status = clSetKernelArg(mykernel, 5, sizeof(cl_float), (void *) &collimator_radius);
	OPENCL_ERROR(clSetKernelArg)
	status = clSetKernelArg(mykernel, 6, sizeof(cl_float), (void *) &collimator_height);
	OPENCL_ERROR(clSetKernelArg)
	status = clSetKernelArg(mykernel, 7, sizeof(cl_int), (void *) &hits_per_single);
	OPENCL_ERROR(clSetKernelArg)

	//fprintf(stdout,"after setting the kernel args\n");

	size_t globalws[2] = {sa->grid_dims_r_n/RANGE_DIVIDER, sa->grid_dims_theta_n/RANGE_DIVIDER};
	//size_t localws[2] = {16, 16};
	//size_t globalws[2] = {1,1};
	
	
	cl_event *kernelEvent;
	size_t offset[2];
	for (i = 0 ; i < RANGE_DIVIDER ; i++) {
		for (j = 0 ; j < RANGE_DIVIDER ; j++) {
			kernelEvent = (cl_event*) malloc(sizeof(kernelEvent));
			offset[0] = i*sa->grid_dims_r_n/RANGE_DIVIDER;
			offset[1] = j*sa->grid_dims_theta_n/RANGE_DIVIDER;
			status = clEnqueueNDRangeKernel(cmdQueue, mykernel, 2, offset, globalws, NULL, 0, NULL, kernelEvent);
			OPENCL_ERROR(clEnqueueNDRangeKernel)
			status = clWaitForEvents(1, kernelEvent);
			OPENCL_ERROR(clWaitForEvents)
			status = clReleaseEvent(kernelEvent[0]);
			OPENCL_ERROR(clReleaseEvent)
			free(kernelEvent);
			if (xmo.verbose)
				fprintf(stdout,"Solid angle calculation at %3i %%\n",(int) floor(100.0*(float)(RANGE_DIVIDER*i+j+1)/(float)(RANGE_DIVIDER*RANGE_DIVIDER)));
		}
	}

	//clReleaseEvent(writeEventA);
	//clReleaseEvent(writeEventB);

	//fprintf(stdout,"after launching the kernel\n");
	//clFinish(cmdQueue);

	cl_event readEvent;
	status = clEnqueueReadBuffer(cmdQueue, solid_angles_cl, CL_FALSE, 0, (sizeof(float)*sa->grid_dims_r_n*sa->grid_dims_theta_n), (void *) solid_angles_float, 0, NULL, &readEvent);
	OPENCL_ERROR(clEnqueueReadBuffer)

	//clReleaseEvent(kernelEvent);
	clWaitForEvents(1, &readEvent);
	clReleaseEvent(readEvent);

	
	for (i = 0 ; i < sa->grid_dims_r_n * sa->grid_dims_theta_n ; i++)
		sa->solid_angles[i] = (double) solid_angles_float[i];
	sa->xmi_input_string = input_string;


	//cleanup
	clReleaseKernel(mykernel);
	clReleaseProgram(myprog);
	clReleaseCommandQueue(cmdQueue);
	clReleaseMemObject(grid_dims_r_vals_cl);
	clReleaseMemObject(grid_dims_theta_vals_cl);
	clReleaseMemObject(solid_angles_cl);
	clReleaseContext(context);

	free(solid_angles_float);
	free(devices);
	free(platforms);
	
	if (xmo.verbose)
		fprintf(stdout,"Solid angle calculation finished\n");
	return 1;
}


#endif



