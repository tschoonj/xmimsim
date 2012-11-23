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
#include <hdf5.h>
#include <glib.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "xmi_aux.h"
#include "xmi_xml.h"
#include <sys/stat.h>
#ifdef MAC_INTEGRATION
	#import <Foundation/Foundation.h>
#endif

#ifdef HAVE_OPENCL_CL_H
#include <OpenCL/cl.h>
#elif defined(HAVE_CL_CL_H)
#endif

struct xmi_solid_angles_data{
	struct xmi_solid_angle **solid_angles;
	struct xmi_input *input;
};

static herr_t xmi_read_single_solid_angle( hid_t g_id, const char *name, const H5L_info_t *info, void *op_data);



extern void xmi_solid_angle_calculation_f(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options);
static void xmi_solid_angle_calculation_cl(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options);
extern void xmi_solid_angle_inputs_f(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, int *collimator_present, float *detector_radius, float *collimator_radius, float *collimator_height);

extern long hits_per_single;

void xmi_solid_angle_calculation(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options xmo) {

#ifdef HAVE_OPENCL_CL_H
	if (xmo.use_opencl)  
		xmi_solid_angle_calculation_cl(inputFPtr, solid_angle, input_string, xmo);
	else {
		xmi_solid_angle_calculation_f(inputFPtr, solid_angle, input_string, xmo);
	}
#else
	xmi_solid_angle_calculation_f(inputFPtr, solid_angle, input_string, xmo);
#endif
}


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

	hid_t file_id;
	struct xmi_solid_angles_data data;
	herr_t iterate_rv;

	//open the hdf5 file read-only!
	file_id = H5Fopen(hdf5_file, H5F_ACC_RDONLY , H5P_DEFAULT);
	if (file_id < 0 ) {
		fprintf(stderr,"Cannot open file %s for reading\n",hdf5_file);
		return 0;
	}

	data.solid_angles = rv;
	data.input = A;

	iterate_rv = H5Literate(file_id, H5_INDEX_NAME, H5_ITER_INC, NULL, xmi_read_single_solid_angle,(void *) &data);

	if (iterate_rv < 0)
		return 0;
	else if(iterate_rv == 0) {
		*rv = NULL;
	}
	

	H5Fclose(file_id);
	return 1;
}


void xmi_free_solid_angle(struct xmi_solid_angle *solid_angle) {
	//this is potentially dangerous... some of this memory is allocated by fortran...
	free(solid_angle->solid_angles);
	free(solid_angle->grid_dims_r_vals);
	free(solid_angle->grid_dims_theta_vals);
	free(solid_angle->xmi_input_string);
}

int xmi_get_solid_angle_file(char **filePtr) {
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
	if (!g_file_test(file, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_REGULAR)) {
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


#ifdef HAVE_OPENCL_CL_H

#define OPENCL_ERROR(name) if (status != CL_SUCCESS) { \
	fprintf(stderr,"OpenCL error for function %s on line %i\n",#name,__LINE__);\
	exit(1);\
	} 

static void xmi_solid_angle_calculation_cl(xmi_inputFPtr inputFPtr, struct xmi_solid_angle **solid_angle, char *input_string, struct xmi_main_options xmo) {

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

	fprintf(stdout,"Number of platforms found: %i\n",numPlatforms);
	
	status = clGetPlatformInfo(platforms[0], CL_PLATFORM_VERSION, info_size, info, NULL);
	OPENCL_ERROR(clGetPlatformInfo)
	fprintf(stdout,"OpenCL platform version %s\n", info);

	cl_uint numDevices = 0;
	cl_device_id *devices = NULL;
	status = clGetDeviceIDs(platforms[0], CL_DEVICE_TYPE_GPU, 0, NULL, &numDevices);
	OPENCL_ERROR(clGetDeviceIDs)

	devices = (cl_device_id *) malloc(sizeof(cl_device_id)*numDevices);
	status = clGetDeviceIDs(platforms[0], CL_DEVICE_TYPE_GPU, numDevices, devices, NULL);
	OPENCL_ERROR(clGetDeviceIDs)

	fprintf(stdout,"Number of devices found: %i\n", numDevices);

	cl_context context = NULL;
	context = clCreateContext(NULL, numDevices, devices, NULL, NULL, &status);
	OPENCL_ERROR(clCreateContext)

	cl_command_queue cmdQueue;
	cmdQueue = clCreateCommandQueue(context, devices[0], 0, &status);
	OPENCL_ERROR(clCreateCommandQueue)

	//start assembling our input
	int collimator_present;
	float detector_radius;
	float collimator_radius;
	float collimator_height;

	xmi_solid_angle_inputs_f(inputFPtr, solid_angle, &collimator_present, &detector_radius, &collimator_radius, &collimator_height);

	struct xmi_solid_angle *sa = *solid_angle;

	fprintf(stdout,"grid_dims_r_n: %li\n", sa->grid_dims_r_n);
	fprintf(stdout,"grid_dims_theta_n: %li\n", sa->grid_dims_theta_n);
	fprintf(stdout,"hits_per_single: %li\n",hits_per_single);

	//other arguments needed
	//	detector%collimator_present
	//	detector%detector_radius
	//	detector%collimator_radius
	//	detector%collimator_height
	//	hits_per_single
	//

	float *grid_dims_r_vals_float = (float *) malloc(sizeof(float)*sa->grid_dims_r_n);
	float *grid_dims_theta_vals_float = (float *) malloc(sizeof(float)*sa->grid_dims_theta_n);
	int i;

	for (i = 0 ; i < sa->grid_dims_r_n ; i++)
		grid_dims_r_vals_float[i] = (float) sa->grid_dims_r_vals[i];

	for (i = 0 ; i < sa->grid_dims_theta_n ; i++)
		grid_dims_theta_vals_float[i] = (float) sa->grid_dims_theta_vals[i];

	float *solid_angles_float = (float *) malloc(sizeof(float)*sa->grid_dims_r_n*sa->grid_dims_theta_n);


	//check device constant memory
	cl_ulong constant_buffer_memory;
	status = clGetDeviceInfo(devices[0], CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, sizeof(cl_ulong), &constant_buffer_memory, NULL);
	OPENCL_ERROR(clGetDeviceInfo)
	fprintf(stdout,"Max constant buffer size: %lu\n", constant_buffer_memory);
	if (constant_buffer_memory < (sizeof(float)*sa->grid_dims_r_n+sizeof(float)*sa->grid_dims_theta_n)) {
		fprintf(stdout,"constant buffer size too small\n");
	}

//#define DEBUG

	cl_event writeEventA, writeEventB;
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
	status = clEnqueueWriteBuffer(cmdQueue, grid_dims_r_vals_cl, CL_TRUE, 0, sizeof(float), input1, 0, NULL, NULL);
	OPENCL_ERROR(clEnqueueWriteBuffer)
	status = clEnqueueWriteBuffer(cmdQueue, grid_dims_theta_vals_cl, CL_TRUE, 0, sizeof(float), input2, 0, NULL, NULL);
	OPENCL_ERROR(clEnqueueWriteBuffer)
#endif

	//compile kernel
	//read the kernel code first from file
	//
	//
	

	gchar *kernel_code;
	if(g_file_get_contents(XMI_OPENCL_DIR "/xmi_kernels.cl", &kernel_code, NULL, NULL) == FALSE) {
		fprintf(stderr,"Could not open %s\n",XMI_OPENCL_DIR "/xmi_kernels.cl");
	}
	
	cl_program myprog = clCreateProgramWithSource(context, 1, (const char **) &kernel_code, NULL, &status);
	OPENCL_ERROR(clCreateProgramWithSource)

	status = clBuildProgram(myprog, 0, NULL, "-I" XMI_OPENCL_DIR, NULL, NULL);
	//OPENCL_ERROR(clBuildProgram)

	if (status == CL_BUILD_PROGRAM_FAILURE) {
		fprintf(stderr,"build failure\n");
		// Determine the size of the log
		size_t log_size;
		clGetProgramBuildInfo(myprog, devices[0], CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);
	
		// Allocate memory for the log
		char *log = (char *) malloc(log_size);
		
	        // Get the log
		clGetProgramBuildInfo(myprog, devices[0], CL_PROGRAM_BUILD_LOG, log_size, log, NULL);
		
		// Print the log
		printf("%s\n", log);
		exit(0);
	}
	else {
		fprintf(stdout,"status: %i\n",status);
	}

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

	fprintf(stdout,"after setting the kernel args\n");

	size_t globalws[2] = {sa->grid_dims_r_n, sa->grid_dims_theta_n};
	size_t localws[2] = {16, 16};
	//size_t globalws[2] = {1,1};
	cl_event kernelEvent;
	status = clEnqueueNDRangeKernel(cmdQueue, mykernel, 2, 0, globalws, NULL, 0, NULL, &kernelEvent);
	OPENCL_ERROR(clEnqueueNDRangeKernel)

	//clReleaseEvent(writeEventA);
	//clReleaseEvent(writeEventB);

	fprintf(stdout,"after launching the kernel\n");
	//clFinish(cmdQueue);

	cl_event readEvent;
	status = clEnqueueReadBuffer(cmdQueue, solid_angles_cl, CL_FALSE, 0, (sizeof(float)*sa->grid_dims_r_n*sa->grid_dims_theta_n), (void *) solid_angles_float, 1, &kernelEvent, &readEvent);
	OPENCL_ERROR(clEnqueueReadBuffer)

	clReleaseEvent(kernelEvent);
	clWaitForEvents(1, &readEvent);
	clReleaseEvent(readEvent);

	
	for (i = 0 ; i < sa->grid_dims_r_n * sa->grid_dims_theta_n ; i++)
		sa->solid_angles[i] = (double) solid_angles_float[i];
	sa->xmi_input_string = input_string;

}


#endif



