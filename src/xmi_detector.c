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
#include "xmi_data_structs.h"
#include "xmi_detector.h"
#include <stdlib.h>
#include <hdf5.h>
#include <glib.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "xmi_aux.h"
#include "xmi_xml.h"
#include <xraylib.h>
#include <sys/stat.h>
#ifdef MAC_INTEGRATION
	#import <Foundation/Foundation.h>
#endif

void xmi_escape_ratios_calculation_fortran(xmi_inputFPtr inputFPtr, xmi_hdf5FPtr hdf5FPtr, struct xmi_escape_ratios **escape_ratios, char *input_string, struct xmi_main_options options);

int xmi_create_empty_escape_ratios_hdf5_file(char *hdf5_file) {

	hid_t file_id;   /* file identifier */
	hid_t root_group_id;	
	hid_t attribute_id;
	hid_t dataspace_id;
	herr_t status;

	float version = (float) g_ascii_strtod(VERSION, NULL);
	

	/* Create a new file using default properties. */
	file_id = H5Fcreate(hdf5_file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) {
		fprintf(stderr,"Could not create escape ratios HDF5 file %s\n",hdf5_file);
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

void xmi_escape_ratios_calculation(struct xmi_input *inputPtr, struct xmi_escape_ratios **escape_ratios, char *input_string, char *hdf5_file, struct xmi_main_options options) {

	struct xmi_input *esc_ratio_inputPtr;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;


	//copy input structure
	xmi_copy_input(inputPtr, &esc_ratio_inputPtr);

	xmi_free_composition(esc_ratio_inputPtr->composition);

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
	
	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr) == 0) {
		fprintf(stderr,"Could not initialize from hdf5 data file\n");
		exit(1);
	}	

	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);

	//do the actual calculation...
	xmi_escape_ratios_calculation_fortran(inputFPtr, hdf5FPtr, escape_ratios, input_string, options);

}

int xmi_check_escape_ratios_match(struct xmi_input *A, struct xmi_input *B) {
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


int xmi_update_escape_ratios_hdf5_file(char *hdf5_file, struct xmi_escape_ratios *escape_ratios) {
	hid_t file_id;
	char buffer[1024];
	hid_t group_id;
	hid_t dspace_id;
	hid_t dset_id;
	hsize_t dims1[1];
	hsize_t dims2[2];
	hsize_t dims3[3];
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

struct xmi_escape_ratios_data{
	struct xmi_escape_ratios **escape_ratios;
	struct xmi_input *input;
};

static herr_t xmi_read_single_escape_ratios( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data) {

	hid_t dset_id, dapl_id, dspace_id;
	hsize_t dims1[1],dims2[2], dims3[3],dims_string[1]; 
	hid_t group_id;
	char *xmi_input_string;
	struct xmi_escape_ratios_data *data = (struct xmi_escape_ratios_data *) op_data;
	struct xmi_input *temp_input;
	struct xmi_escape_ratios *escape_ratios;
	

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

	if (xmi_check_escape_ratios_match(temp_input, data->input) == 1) {
		//match
		//read in this group completely
		xmi_free_input(temp_input);
		*(data->escape_ratios) = (struct xmi_escape_ratios *) malloc(sizeof(struct xmi_escape_ratios));
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
		escape_ratios->Z = (int *) malloc(sizeof(int)*dims1[0]);
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
		escape_ratios->fluo_escape_input_energies = (double *) malloc(sizeof(double)*dims3[0]);
		escape_ratios->fluo_escape_ratios = (double *) malloc(sizeof(double)*dims3[0]*dims3[1]*dims3[2]);
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
		escape_ratios->compton_escape_input_energies = (double *) malloc(sizeof(double)*dims2[1]);
		escape_ratios->compton_escape_output_energies = (double *) malloc(sizeof(double)*dims2[0]);
		escape_ratios->compton_escape_ratios = (double *) malloc(sizeof(double)*dims2[0]*dims2[1]);
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
	}
	else {
		//no match -> continue looking...
		H5Gclose(group_id);
		xmi_free_input(temp_input);
		free(xmi_input_string);
		return 0;
	}

	return 1;
}

int xmi_find_escape_ratios_match(char *hdf5_file, struct xmi_input *A, struct xmi_escape_ratios **rv) {

	//let's take this one on a bit more elegant than xmi_find_solid_angle_match...
	//open the hdf5 file and iterate through all the groups
	//in every group read ONLY the xmi_input_string and use it to compare...
	//if comparison is successful, then read in the rest and close the file
	hid_t file_id;
	struct xmi_escape_ratios_data data;
	herr_t iterate_rv;

	//open the hdf5 file read-only!
	file_id = H5Fopen(hdf5_file, H5F_ACC_RDONLY , H5P_DEFAULT);
	if (file_id < 0 ) {
		fprintf(stderr,"Cannot open file %s for reading\n",hdf5_file);
		return 0;
	}

	data.escape_ratios = rv;
	data.input = A;

	iterate_rv = H5Literate(file_id, H5_INDEX_NAME, H5_ITER_INC, NULL, xmi_read_single_escape_ratios,(void *) &data);

	if (iterate_rv < 0)
		return 0;
	else if(iterate_rv == 0) {
		*rv = NULL;
	}
	

	H5Fclose(file_id);
	return 1;
}

void xmi_free_escape_ratios(struct xmi_escape_ratios *escape_ratios) {
	free(escape_ratios->Z);
	free(escape_ratios->fluo_escape_ratios);
	free(escape_ratios->fluo_escape_input_energies);
	free(escape_ratios->compton_escape_ratios);
	free(escape_ratios->compton_escape_input_energies);
	free(escape_ratios->compton_escape_output_energies);
	free(escape_ratios->xmi_input_string);
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
		file = (char *) malloc(sizeof(char)*(strlen(data_dir)+strlen(G_DIR_SEPARATOR_S "XMI-MSIM" G_DIR_SEPARATOR_S "xmimsim-escape-ratios.h5")+1));
		strcpy(file, data_dir);
		strcat(file,G_DIR_SEPARATOR_S "XMI-MSIM" G_DIR_SEPARATOR_S "xmimsim-escape-ratios.h5");
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
#ifdef MAC_INTEGRATION
	[pool drain];
#endif

	return 1;
}


