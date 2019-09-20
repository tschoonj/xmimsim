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

#include "config.h"
#include "xmi_solid_angle.h"
#include "xmi_data_structs.h"
#include "xmi_main.h"
#include <glib.h>
#include <math.h>
#include <string.h>
#include <glib/gstdio.h>
#include <gmodule.h>


#ifdef G_OS_WIN32
  #include "xmi_registry_win.h"
  #include "windows.h"
#endif

#include <hdf5.h>
#include "xmi_aux.h"
#include "xmi_xml.h"
#include <xraylib.h>
#ifdef MAC_INTEGRATION
  #include "xmi_resources_mac.h"
#endif

typedef struct {
	xmi_solid_angle **solid_angles;
	xmi_input *input;
	xmi_main_options *options;
} xmi_solid_angles_data;

static herr_t xmi_read_single_solid_angle(hid_t g_id, const char *name, const H5L_info_t *info, void *op_data);

extern void xmi_solid_angle_calculation_f(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *options);

typedef int (*XmiSolidAngleCalculation) (xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *options);

void xmi_solid_angle_calculation(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *xmo) {

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	XmiSolidAngleCalculation xmi_solid_angle_calculation_cl;
	GModule *module;
	gchar *module_path;
	gchar *opencl_lib;

	if (xmo->use_gpu) {
		//try to open the module
		if (!g_module_supported()) {
			fprintf(stderr,"No module support on this platform\n");
			goto fallback;
		}

		const gchar *opencl_lib_env = g_getenv("XMIMSIM_CL_LIB");
		if (opencl_lib_env != NULL) {
			opencl_lib = g_strdup(opencl_lib_env);
		}
		else {
#ifdef G_OS_WIN32
			if (xmi_registry_win_query(XMI_REGISTRY_WIN_OPENCL_LIB, &opencl_lib) == 0)
				goto fallback;
#elif defined(MAC_INTEGRATION)
			if (xmi_resources_mac_query(XMI_RESOURCES_MAC_OPENCL_LIB, &opencl_lib) == 0)
				goto fallback;
#else
			opencl_lib = g_strdup(XMI_OPENCL_LIB);
#endif
		}
		module_path = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s.%s", opencl_lib, "xmimsim-cl", G_MODULE_SUFFIX);
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
			int rv = xmi_solid_angle_calculation_cl(inputFPtr, solid_angle, g_strdup(input_string), xmo);
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
	g_setenv("XMIMSIM_CL_FALLBACK", "", TRUE);
#endif
	xmi_solid_angle_calculation_f(inputFPtr, solid_angle, g_strdup(input_string), xmo);
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


int xmi_update_solid_angle_hdf5_file(char *hdf5_file, xmi_solid_angle *solid_angle) {
	hid_t file_id;
	char *buffer;
	hid_t group_id;
	hid_t dspace_id;
	hid_t dset_id;
	hsize_t dims[2];
	hsize_t xmi_input_strlen;
	gchar *timestring;

	file_id = H5Fopen(hdf5_file, H5F_ACC_RDWR , H5P_DEFAULT);
	if (file_id < 0 ) {
		fprintf(stderr,"Cannot open file %s for read/write\n",hdf5_file);
		return 0;
	}

	//create group name based on user and timestamp
#if GLIB_CHECK_VERSION(2, 62, 0)
	GDateTime *date_time = g_date_time_new_now_local();
	timestring = g_date_time_format_iso8601(date_time);
	g_date_time_unref(date_time);
#else
	GTimeVal time;
	g_get_current_time(&time);
        timestring = g_time_val_to_iso8601(&time);
#endif

	buffer = g_strdup_printf("%s %s",g_get_user_name(),timestring);

	g_free(timestring);

	//create group
	hid_t gcpl = H5Pcreate (H5P_GROUP_CREATE);
	H5Pset_link_creation_order( gcpl, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED );
	group_id = H5Gcreate(file_id, buffer, H5P_DEFAULT, gcpl, H5P_DEFAULT);
	H5Pclose (gcpl);
	g_free(buffer);

	//add solid_angles dataset
	dims[1] = solid_angle->grid_dims_r_n;
	dims[0] = solid_angle->grid_dims_theta_n;

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

	H5Fflush(file_id, H5F_SCOPE_GLOBAL);
	H5Fclose(file_id);

	return 1;
}

struct multiple_solid_angles {
	xmi_solid_angle *solid_angles;
	int n_solid_angles;
};

static herr_t xmi_read_single_solid_angle(hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {
	hid_t dset_id, dspace_id;
	hsize_t dims[2], dims_string[1];
	hid_t group_id;
	char *xmi_input_string;
	xmi_solid_angles_data *data = op_data;
	xmi_input *temp_input;
	xmi_solid_angle *solid_angles;

	if (data->options->extra_verbose) {
		fprintf(stdout, "Checking solid angle grid group with name %s\n", name);
	}

	//open group
	group_id = H5Gopen(g_id,name, H5P_DEFAULT);
	if (group_id < 0) {
		fprintf(stderr, "Error opening group %s\n", name);
		return -1;
	}

	//open xmi_input_string
	dset_id = H5Dopen(group_id, "xmi_input_string", H5P_DEFAULT);
	dspace_id = H5Dget_space(dset_id);
	H5Sget_simple_extent_dims(dspace_id, dims_string, NULL);
	xmi_input_string = g_malloc(sizeof(char) * dims_string[0]);
	H5Dread(dset_id, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,xmi_input_string );
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	if ((temp_input = xmi_input_read_from_xml_string(xmi_input_string, NULL)) == NULL)
		return -1;

	if (xmi_check_solid_angle_match(temp_input, data->input) == 1) {
		//match
		//read in this group completely
		xmi_input_free(temp_input);
		*(data->solid_angles) = g_malloc(sizeof(xmi_solid_angle));
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
		solid_angles->solid_angles = g_malloc(sizeof(double) * dims[0] * dims[1]);
		solid_angles->grid_dims_r_n = dims[1];
		solid_angles->grid_dims_theta_n = dims[0];

		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, solid_angles->solid_angles);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dset_id = H5Dopen(group_id, "grid_dims_r_vals", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		solid_angles->grid_dims_r_vals = g_malloc(sizeof(double) * dims[1]);
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, solid_angles->grid_dims_r_vals);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dset_id = H5Dopen(group_id, "grid_dims_theta_vals", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		solid_angles->grid_dims_theta_vals = g_malloc(sizeof(double) * dims[0]);
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, solid_angles->grid_dims_theta_vals);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);
		H5Gclose(group_id);

		if (data->options->extra_verbose)
			fprintf(stdout, "Match in solid angle grid\n");
	}
	else {
		//no match -> continue looking...
		H5Gclose(group_id);
		xmi_input_free(temp_input);
		g_free(xmi_input_string);
		if (data->options->extra_verbose)
			fprintf(stdout, "No match in solid angle grid\n");

		return 0;
	}

	return 1;
}

//A -> from HDF5 file (existing - old)
//B -> from XMSI file (new)
int xmi_check_solid_angle_match(xmi_input *A, xmi_input *B) {
	int i;
	double *thickness_along_Z_a, *thickness_along_Z_b;
	double *Z_coord_begin_a, *Z_coord_begin_b;
	double *Z_coord_end_a, *Z_coord_end_b;

	//composition
	//new approach: compare extremes of the layer system
	xmi_normalize_vector_double(A->geometry->n_sample_orientation, 3);
	xmi_normalize_vector_double(B->geometry->n_sample_orientation, 3);
	thickness_along_Z_a = (double *) g_malloc(sizeof(double) * A->composition->n_layers);
	Z_coord_begin_a = (double *) g_malloc(sizeof(double) * A->composition->n_layers);
	Z_coord_end_a = (double *) g_malloc(sizeof(double) * A->composition->n_layers);
	thickness_along_Z_b = (double *) g_malloc(sizeof(double) * B->composition->n_layers);
	Z_coord_begin_b = (double *) g_malloc(sizeof(double) * B->composition->n_layers);
	Z_coord_end_b = (double *) g_malloc(sizeof(double) * B->composition->n_layers);


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
	double energy;

	//S1
	//low energy limit
	mu_a = g_malloc(sizeof(double) * A->composition->n_layers);
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
			mu_a[i] += CS_Total_Kissel(A->composition->layers[i].Z[j], energy, NULL)*A->composition->layers[i].weight[j];
		}
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
	}
	Pabs_a = -1.0 * expm1(-1.0 * sum);
	myln = -1.0 * log1p(-1.0 * R1 * Pabs_a);

	sum=0.0;
	for (i = 0 ; i < A->composition->n_layers ; i++) {
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
		if (sum > myln)
			break;
	}
	m = i;

	sum = 0.0;
	for (i = 0; i < m ; i++)
		sum += (1.0 - (mu_a[i]*A->composition->layers[i].density)/(mu_a[m]*A->composition->layers[m].density))*thickness_along_Z_a[i];

	S1_a = sum + myln/(mu_a[m]*A->composition->layers[m].density) + Z_coord_begin_a[0];

	//S2
	//high energy limit
	g_free(mu_a);
	mu_a = g_malloc(sizeof(double) * A->composition->n_layers);
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
			mu_a[i] += CS_Total_Kissel(A->composition->layers[i].Z[j], energy, NULL)*A->composition->layers[i].weight[j];
		}
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
	}
	Pabs_a = -1.0 * expm1(-1.0 * sum);
	myln = -1.0 * log1p(-1.0 * R2 * Pabs_a);

	sum = 0.0;
	for (i = 0 ; i < A->composition->n_layers ; i++) {
		sum += mu_a[i]*A->composition->layers[i].density*thickness_along_Z_a[i];
		if (sum > myln)
			break;
	}
	m = i;

	sum = 0.0;
	for (i = 0; i < m ; i++)
		sum += (1.0 - (mu_a[i]*A->composition->layers[i].density)/(mu_a[m]*A->composition->layers[m].density))*thickness_along_Z_a[i];

	S2_a = sum + myln/(mu_a[m]*A->composition->layers[m].density) + Z_coord_begin_a[0];

	g_free(mu_a);


	//S1
	//low energy limit
	mu_b = g_malloc(sizeof(double) * B->composition->n_layers);
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
			mu_b[i] += CS_Total_Kissel(B->composition->layers[i].Z[j], energy, NULL)*B->composition->layers[i].weight[j];
		}
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
	}
	Pabs_b = -1.0 * expm1(-1.0 * sum);
	myln = -1.0 * log1p(-1.0 * R1 * Pabs_b);

	sum = 0.0;
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
		if (sum > myln)
			break;
	}
	m = i;

	sum = 0.0;
	for (i = 0; i < m ; i++)
		sum += (1.0 - (mu_b[i]*B->composition->layers[i].density)/(mu_b[m]*B->composition->layers[m].density))*thickness_along_Z_b[i];

	S1_b = sum + myln/(mu_b[m]*B->composition->layers[m].density) + Z_coord_begin_b[0];;

	//S2
	//high energy limit
	g_free(mu_b);
	mu_b = g_malloc(sizeof(double) * B->composition->n_layers);
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
			mu_b[i] += CS_Total_Kissel(B->composition->layers[i].Z[j], energy, NULL)*B->composition->layers[i].weight[j];
		}
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
	}
	Pabs_b = -1.0 * expm1(-1.0 * sum);
	myln = -1.0 * log1p(-1.0 * R2 * Pabs_b);

	sum = 0.0;
	for (i = 0 ; i < B->composition->n_layers ; i++) {
		sum += mu_b[i]*B->composition->layers[i].density*thickness_along_Z_b[i];
		if (sum > myln)
			break;
	}
	m = i;

	sum = 0.0;
	for (i = 0; i < m ; i++)
		sum += (1.0 - (mu_b[i]*B->composition->layers[i].density)/(mu_b[m]*B->composition->layers[m].density))*thickness_along_Z_b[i];

	S2_b = sum + myln/(mu_b[m]*B->composition->layers[m].density) + Z_coord_begin_b[0];

	g_free(mu_b);

	g_free(thickness_along_Z_a);
	g_free(thickness_along_Z_b);
	g_free(Z_coord_begin_a);
	g_free(Z_coord_end_a);
	g_free(Z_coord_begin_b);
	g_free(Z_coord_end_b);

	if (S2_a - S2_b < -0.0001 || S1_a - S1_b > 0.0001) {
		return 0;
	}

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

int xmi_find_solid_angle_match(char *hdf5_file, xmi_input *A, xmi_solid_angle **rv, xmi_main_options *options) {

	hid_t file_id;
	xmi_solid_angles_data data;
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

	if (g_strcmp0(kind, "XMI_HDF5_SOLID_ANGLES") != 0) {
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

	return 1;
}

void xmi_free_solid_angle(xmi_solid_angle *solid_angle) {
	if (!solid_angle)
		return;
	g_free(solid_angle->solid_angles);
	g_free(solid_angle->grid_dims_r_vals);
	g_free(solid_angle->grid_dims_theta_vals);
	g_free(solid_angle->xmi_input_string);
	g_free(solid_angle);
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

	if (file == NULL) {
#ifdef MAC_INTEGRATION
		const gchar *data_dir = xmi_resources_mac_get_user_data_dir();
#else
		const gchar *data_dir = g_get_user_data_dir();
#endif
		file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM" G_DIR_SEPARATOR_S "xmimsim-solid-angles.h5", data_dir);
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

	return 1;
}

