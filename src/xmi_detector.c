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
#include "xmi_data_structs.h"
#include "xmi_detector.h"
#include <hdf5.h>
#include <glib.h>
#include <gmodule.h>
#include <math.h>
#include "xmi_aux.h"
#include "xmi_xml.h"
#include "xmi_data.h"
#include "xmi_main.h"
#include <xraylib.h>
#include <glib/gstdio.h>
#include <string.h>

#ifdef MAC_INTEGRATION
	#include "xmi_resources_mac.h"
#endif


void xmi_escape_ratios_calculation_fortran(xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr, xmi_escape_ratios **escape_ratios, char *input_string, xmi_main_options *options, xmi_escape_ratios_options ero);

int xmi_create_empty_escape_ratios_hdf5_file(char *hdf5_file) {

	hid_t file_id;   /* file identifier */
	hid_t root_group_id;
	hid_t attribute_id;
	hid_t dataspace_id;
	herr_t status;

	double version = g_ascii_strtod(VERSION, NULL);


	/* Create a new file using default properties. */
	hid_t gcpl = H5Pcreate (H5P_FILE_CREATE);
	H5Pset_link_creation_order( gcpl, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED );
	file_id = H5Fcreate(hdf5_file, H5F_ACC_TRUNC, gcpl, H5P_DEFAULT);
	H5Pclose (gcpl);

	if (file_id < 0) {
		fprintf(stderr,"Could not create escape ratios HDF5 file %s\n",hdf5_file);
		return 0;
	}
	root_group_id = H5Gopen(file_id, "/", H5P_DEFAULT);

	dataspace_id = H5Screate(H5S_SCALAR);
	attribute_id = H5Acreate(root_group_id, "version", H5T_NATIVE_DOUBLE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attribute_id, H5T_NATIVE_DOUBLE, &version);

	H5Aclose(attribute_id);
	H5Sclose(dataspace_id);
	//
	//write kind attribute
	dataspace_id = H5Screate(H5S_SCALAR);
	hid_t atype = H5Tcopy(H5T_C_S1);
	H5Tset_size(atype, strlen("XMI_HDF5_ESCAPE_RATIOS")+1);
	H5Tset_strpad(atype,H5T_STR_NULLTERM);
	attribute_id = H5Acreate2(root_group_id, "kind", atype, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attribute_id, atype, "XMI_HDF5_ESCAPE_RATIOS");

	H5Tclose(atype);
	H5Aclose(attribute_id);
	H5Sclose(dataspace_id);


	H5Gclose(root_group_id);


	/* Terminate access to the file. */
	status = H5Fclose(file_id);
	return 1;
}

void xmi_escape_ratios_calculation(xmi_input *inputPtr, xmi_escape_ratios **escape_ratios, char *input_string, char *hdf5_file, xmi_main_options *options, xmi_escape_ratios_options ero) {

	xmi_input *esc_ratio_inputPtr;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;


	//copy input structure
	xmi_input_copy(inputPtr, &esc_ratio_inputPtr);

	xmi_composition_free(esc_ratio_inputPtr->composition);

	xmi_copy_abs_or_crystal2composition(esc_ratio_inputPtr->detector->crystal_layers, esc_ratio_inputPtr->detector->n_crystal_layers, &(esc_ratio_inputPtr->composition));
	esc_ratio_inputPtr->composition->reference_layer = 1;

	//modify geometry
	esc_ratio_inputPtr->geometry->d_sample_source = 1.0;
	esc_ratio_inputPtr->geometry->d_source_slit = 1.0;
	esc_ratio_inputPtr->geometry->slit_size_x = 0.0001;
	esc_ratio_inputPtr->geometry->slit_size_y = 0.0001;
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


	xmi_main_options escape_main_options = {.omp_num_threads = options->omp_num_threads, .verbose = options->verbose, .use_cascade_auger = 0, .use_cascade_radiative = 0, .use_M_lines = 0, .extra_verbose = options->extra_verbose};

	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr, &escape_main_options) == 0) {
		fprintf(stderr,"Could not initialize from hdf5 data file\n");
		exit(1);
	}

	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);

	//do the actual calculation...
	xmi_escape_ratios_calculation_fortran(inputFPtr, hdf5FPtr, escape_ratios, g_strdup(input_string), &escape_main_options, ero);

}

int xmi_check_escape_ratios_match(xmi_input *A, xmi_input *B) {
	//match if detector crystal compositions match
	int i,j;

	//composition
	if (A->detector->n_crystal_layers != B->detector->n_crystal_layers) {
		return 0;
	}

	for (i = 0 ; i < A->detector->n_crystal_layers ; i++) {
		if (fabsl(A->detector->crystal_layers[i].thickness- B->detector->crystal_layers[i].thickness)/A->detector->crystal_layers[i].thickness > XMI_COMPARE_THRESHOLD) {
			return 0;
		}
		if (fabsl(A->detector->crystal_layers[i].density- B->detector->crystal_layers[i].density)/A->detector->crystal_layers[i].density > XMI_COMPARE_THRESHOLD) {
			return 0;
		}
		if (A->detector->crystal_layers[i].n_elements != B->detector->crystal_layers[i].n_elements)
			return 0;
		for (j = 0 ; j < A->detector->crystal_layers[i].n_elements ; j++) {
			if (A->detector->crystal_layers[i].Z[j] != B->detector->crystal_layers[i].Z[j])
				return 0;
			if (fabsl(A->detector->crystal_layers[i].weight[j]-B->detector->crystal_layers[i].weight[j]) > XMI_COMPARE_THRESHOLD)
				return 0;

		}

	}
	return 1;
}


int xmi_update_escape_ratios_hdf5_file(char *hdf5_file, xmi_escape_ratios *escape_ratios) {
	hid_t file_id;
	gchar *buffer;
	hid_t group_id;
	hid_t dspace_id;
	hid_t dset_id;
	hsize_t dims1[1];
	hsize_t dims2[2];
	hsize_t dims3[3];
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
	g_free(buffer);
	H5Pclose(gcpl);


	//write Z
	dims1[0]=escape_ratios->n_elements;
	dspace_id = H5Screate_simple(1, dims1, dims1);
	dset_id = H5Dcreate(group_id,"elements",H5T_NATIVE_INT, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL,H5S_ALL, H5P_DEFAULT, escape_ratios->Z);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);


	//write fluo_escape_ratios
	dims3[2] = escape_ratios->n_elements;
	dims3[1] = abs(L3P3_LINE);
	dims3[0] = escape_ratios->n_fluo_input_energies;
	dspace_id = H5Screate_simple(3, dims3, dims3);
	dset_id = H5Dcreate(group_id,"fluo escape ratios",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL,H5S_ALL, H5P_DEFAULT, escape_ratios->fluo_escape_ratios);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	//write fluo_escape_input_energies
	dspace_id = H5Screate_simple(1, &dims3[0], &dims3[0]);
	dset_id = H5Dcreate(group_id,"fluo escape input energies",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL,H5S_ALL, H5P_DEFAULT, escape_ratios->fluo_escape_input_energies);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	//write compton escape ratios
	dims2[0] = escape_ratios->n_compton_output_energies;
	dims2[1] = escape_ratios->n_compton_input_energies;
	dspace_id = H5Screate_simple(2, dims2, dims2);
	dset_id = H5Dcreate(group_id,"compton escape ratios",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL,H5S_ALL, H5P_DEFAULT, escape_ratios->compton_escape_ratios);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	//write compton input energies
	dspace_id = H5Screate_simple(1, &dims2[1], &dims2[1]);
	dset_id = H5Dcreate(group_id,"compton escape input energies",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL,H5S_ALL, H5P_DEFAULT, escape_ratios->compton_escape_input_energies);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	//write compton output energies
	dspace_id = H5Screate_simple(1, &dims2[0], &dims2[0]);
	dset_id = H5Dcreate(group_id,"compton escape output energies",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL,H5S_ALL, H5P_DEFAULT, escape_ratios->compton_escape_output_energies);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	//write xmi_input_string
	xmi_input_strlen = strlen(escape_ratios->xmi_input_string)+1;
	dspace_id = H5Screate_simple(1, &xmi_input_strlen, &xmi_input_strlen);
	dset_id = H5Dcreate(group_id, "xmi_input_string",H5T_NATIVE_CHAR, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL,H5P_DEFAULT, escape_ratios->xmi_input_string);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);


	H5Gclose(group_id);

	H5Fflush(file_id, H5F_SCOPE_GLOBAL);
#if DEBUG == 2
	fprintf(stdout,"open objects: %i\n",(int) H5Fget_obj_count(file_id, H5F_OBJ_ALL));
#endif

	H5Fclose(file_id);

	return 1;
}

typedef struct {
	xmi_escape_ratios **escape_ratios;
	xmi_input *input;
	xmi_main_options *options;
} xmi_escape_ratios_data;

static herr_t xmi_read_single_escape_ratios( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {

	hid_t dset_id, dspace_id;
	hsize_t dims1[1],dims2[2], dims3[3],dims_string[1];
	hid_t group_id;
	char *xmi_input_string;
	xmi_escape_ratios_data *data = (xmi_escape_ratios_data *) op_data;
	xmi_input *temp_input;
	xmi_escape_ratios *escape_ratios;


	if (data->options->extra_verbose) {
		fprintf(stdout,"Checking escape ratios group with name %s\n", name);
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
	xmi_input_string = (char *) g_malloc(sizeof(char)*dims_string[0]);
	H5Dread(dset_id, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,xmi_input_string );
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	GError *error = NULL;

	if ((temp_input = xmi_input_read_from_xml_string(xmi_input_string, &error)) == NULL) {
		if (error) {
			fprintf(stderr, "Error calling xmi_input_read_from_xml_string: %s\n", error->message);
			g_error_free(error);
		}
		return -1;
	}

	if (xmi_check_escape_ratios_match(temp_input, data->input) == 1) {
		//match
		//read in this group completely
		xmi_input_free(temp_input);
		*(data->escape_ratios) = (xmi_escape_ratios *) g_malloc(sizeof(xmi_escape_ratios));
		escape_ratios = *(data->escape_ratios);
		escape_ratios->xmi_input_string  = xmi_input_string;
		//read elements
		dset_id = H5Dopen(group_id, "elements", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		if (H5Sget_simple_extent_ndims(dspace_id) != 1) {
			fprintf(stderr,"Number of dimensions of elements must be equal to 1\n");
			return -1;
		}
		H5Sget_simple_extent_dims(dspace_id, dims1, NULL);
		escape_ratios->n_elements = dims1[0];
		escape_ratios->Z = (int *) g_malloc(sizeof(int)*dims1[0]);
		H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,escape_ratios->Z );
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//read fluo escape ratios
		dset_id = H5Dopen(group_id, "fluo escape ratios", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		if (H5Sget_simple_extent_ndims(dspace_id) != 3) {
			fprintf(stderr,"Number of dimensions of fluo escape ratios must be equal to 3\n");
			return -1;
		}
		H5Sget_simple_extent_dims(dspace_id, dims3, NULL);
		escape_ratios->n_fluo_input_energies = dims3[0];
		escape_ratios->fluo_escape_input_energies = (double *) g_malloc(sizeof(double)*dims3[0]);
		escape_ratios->fluo_escape_ratios = (double *) g_malloc(sizeof(double)*dims3[0]*dims3[1]*dims3[2]);
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,escape_ratios->fluo_escape_ratios);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//read fluo escape input energies
		dset_id = H5Dopen(group_id, "fluo escape input energies", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		if (H5Sget_simple_extent_ndims(dspace_id) != 1) {
			fprintf(stderr,"Number of dimensions of fluo escape input energies must be equal to 1\n");
			return -1;
		}
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,escape_ratios->fluo_escape_input_energies);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//read compton escape ratios
		dset_id = H5Dopen(group_id, "compton escape ratios", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		if (H5Sget_simple_extent_ndims(dspace_id) != 2) {
			fprintf(stderr,"Number of dimensions of compton escape ratios must be equal to 2\n");
			return -1;
		}
		H5Sget_simple_extent_dims(dspace_id, dims2, NULL);
		escape_ratios->n_compton_input_energies = dims2[1];
		escape_ratios->n_compton_output_energies = dims2[0];
		escape_ratios->compton_escape_input_energies = (double *) g_malloc(sizeof(double)*dims2[1]);
		escape_ratios->compton_escape_output_energies = (double *) g_malloc(sizeof(double)*dims2[0]);
		escape_ratios->compton_escape_ratios = (double *) g_malloc(sizeof(double)*dims2[0]*dims2[1]);
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,escape_ratios->compton_escape_ratios);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//read compton escape input energies
		dset_id = H5Dopen(group_id, "compton escape input energies", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		if (H5Sget_simple_extent_ndims(dspace_id) != 1) {
			fprintf(stderr,"Number of dimensions of compton escape input energies must be equal to 1\n");
			return -1;
		}
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,escape_ratios->compton_escape_input_energies);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//read compton escape output energies
		dset_id = H5Dopen(group_id, "compton escape output energies", H5P_DEFAULT);
		dspace_id = H5Dget_space(dset_id);
		if (H5Sget_simple_extent_ndims(dspace_id) != 1) {
			fprintf(stderr,"Number of dimensions of compton escape output energies must be equal to 1\n");
			return -1;
		}
		H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,escape_ratios->compton_escape_output_energies);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		H5Gclose(group_id);
		if (data->options->extra_verbose)
			fprintf(stdout,"Match in escape ratios\n");
	}
	else {
		//no match -> continue looking...
		H5Gclose(group_id);
		xmi_input_free(temp_input);
		g_free(xmi_input_string);
		if (data->options->extra_verbose)
			fprintf(stdout,"No match in escape ratios\n");
		return 0;
	}

	return 1;
}

int xmi_find_escape_ratios_match(char *hdf5_file, xmi_input *A, xmi_escape_ratios **rv, xmi_main_options *options) {

	//open the hdf5 file and iterate through all the groups
	//in every group read ONLY the xmi_input_string and use it to compare...
	//if comparison is successful, then read in the rest and close the file
	hid_t file_id;
	xmi_escape_ratios_data data;
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
		g_fprintf(stderr, "Escape ratios file %s does not contain the kind tag\n", hdf5_file);
		g_fprintf(stderr, "The file will be deleted and recreated\n");

		H5Gclose(root_group_id);
		H5Fclose(file_id);
		if(g_unlink(hdf5_file) == -1) {
			g_fprintf(stderr,"Could not delete file %s... Fatal error\n", hdf5_file);
			return 0;
		}
		if (xmi_get_escape_ratios_file(&hdf5_file, 1) == 0) {
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

	if (g_strcmp0(kind, "XMI_HDF5_ESCAPE_RATIOS") != 0) {
		g_fprintf(stderr, "Escape ratios file %s does not have the correct kind attribute\n", hdf5_file);
		g_fprintf(stderr, "Expected XMI_HDF5_ESCAPE_RATIOS but found %s\n", kind);
		g_fprintf(stderr, "Aborting\n");

		H5Gclose(root_group_id);
		H5Fclose(file_id);
		return 0;
	}


	attribute_id = H5Aopen(root_group_id, "version", H5P_DEFAULT);
	if (attribute_id < 0) {
		//attribute does not exist
		g_fprintf(stderr, "Escape ratios file %s does not contain the version tag\n", hdf5_file);
		g_fprintf(stderr, "The file will be deleted and recreated\n");

		H5Gclose(root_group_id);
		H5Fclose(file_id);
		if(g_unlink(hdf5_file) == -1) {
			g_fprintf(stderr,"Could not delete file %s... Fatal error\n", hdf5_file);
			return 0;
		}
		if (xmi_get_escape_ratios_file(&hdf5_file, 1) == 0) {
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

	if (version < XMI_ESCAPE_RATIOS_MIN_VERSION) {
		//too old -> delete file
		g_fprintf(stderr, "Escape ratios file %s is not compatible with this version of XMI-MSIM\n", hdf5_file);
		g_fprintf(stderr, "The file will be deleted and recreated\n");

		H5Fclose(file_id);
		if(g_unlink(hdf5_file) == -1) {
			g_fprintf(stderr,"Could not delete file %s... Fatal error\n", hdf5_file);
			return 0;
		}
		if (xmi_get_escape_ratios_file(&hdf5_file, 1) == 0) {
			return 0;
		}
		*rv = NULL;
		return 1;
	}
	data.escape_ratios = rv;
	data.input = A;
	data.options = options;

	iterate_rv = H5Literate(file_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, NULL, xmi_read_single_escape_ratios,(void *) &data);

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

void xmi_free_escape_ratios(xmi_escape_ratios *escape_ratios) {
	if (escape_ratios->fluo_escape_input_energies != escape_ratios->compton_escape_input_energies) {
		//allocated in C
		g_free(escape_ratios->Z);
		g_free(escape_ratios->fluo_escape_ratios);
		g_free(escape_ratios->fluo_escape_input_energies);
		g_free(escape_ratios->compton_escape_input_energies);
		g_free(escape_ratios->compton_escape_ratios);
		g_free(escape_ratios->compton_escape_output_energies);
		g_free(escape_ratios->xmi_input_string);
		g_free(escape_ratios);
	}
	else {
		//allocated in Fortran
		//do not free compton_escape_input_energies here!!!
		g_free(escape_ratios->Z);
		g_free(escape_ratios->fluo_escape_ratios);
		g_free(escape_ratios->fluo_escape_input_energies);
		g_free(escape_ratios->compton_escape_ratios);
		g_free(escape_ratios->compton_escape_output_energies);
		g_free(escape_ratios->xmi_input_string);
		g_free(escape_ratios);
	}
}

int xmi_get_escape_ratios_file(char **filePtr, int create_file) {
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
		file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM" G_DIR_SEPARATOR_S "xmimsim-escape-ratios.h5", data_dir);
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
		return xmi_create_empty_escape_ratios_hdf5_file(file);
	}

	return 1;
}

int xmi_check_detector_convolute_plugin(char *dlm) {
	XmiDetectorConvoluteAll xmi_detector_convolute_all_custom;
	GModule *module;
	if (!g_module_supported()) {
		fprintf(stderr,"No module support on this platform: cannot use custom detector convolution routine\n");
		return -1;
	}
	module = g_module_open(dlm, 0);
	if (!module) {
		fprintf(stderr,"Could not open %s: %s\n", dlm, g_module_error());
		return 0;
	}
	if (!g_module_symbol(module, "xmi_detector_convolute_all_custom", (gpointer *) &xmi_detector_convolute_all_custom)) {
		fprintf(stderr,"Error retrieving xmi_detector_convolute_all_custom in %s: %s\n", dlm, g_module_error());
		return 0;
	}
	if (!g_module_close(module)) {
		fprintf(stderr,"Warning: could not close module %s: %s\n",dlm, g_module_error());
		return 0;
	}
	return 1;
}

xmi_escape_ratios_options xmi_get_default_escape_ratios_options(void) {
	xmi_escape_ratios_options rv = {1990, 1999, 500000, 1.0, 0.1, 0.1, 0.1};
	return rv;
}
