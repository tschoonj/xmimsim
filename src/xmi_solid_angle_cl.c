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
#endif

// Silence Apple's warning about the deprecation of OpenCL.
#define CL_SILENCE_DEPRECATION

// Silence warnings about using deprecated OpenCL 1.2 functions.
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS

#ifdef HAVE_OPENCL_CL_H
  #include <OpenCL/cl.h>
#elif defined(HAVE_CL_CL_H)
  #include <CL/cl.h>
#endif

#include "xmi_resources.h"

#define XMI_OPENCL_MAJOR 1
#define XMI_OPENCL_MINOR 1

extern void xmi_solid_angle_inputs_f(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, int *collimator_present, float *detector_radius, float *collimator_radius, float *collimator_height);

extern long hits_per_single;

/*
 * Taken from http://tom.scogland.com/blog/2013/03/29/opencl-errors
 */
static const char *clGetErrorString(cl_int err){
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

G_MODULE_EXPORT int xmi_solid_angle_calculation_cl(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *xmo);

G_MODULE_EXPORT int xmi_solid_angle_calculation_cl(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *xmo) {

	cl_int status;
	char info[1000];
	size_t info_size = 1000;

	cl_uint numPlatforms = 0;
	cl_platform_id *platforms = NULL;
	status = clGetPlatformIDs(0, NULL, &numPlatforms);
	OPENCL_ERROR(clGetPlatformIDs)

	platforms = (cl_platform_id *) g_malloc(sizeof(cl_platform_id)*numPlatforms);
	status = clGetPlatformIDs(numPlatforms, platforms, NULL);
	OPENCL_ERROR(clGetPlatformIDs)



	if (numPlatforms == 0) {
		fprintf(stderr,"No OpenCL platform detected\n");
		return 0;
	}

	int i, j;
	cl_device_id device;
	size_t max_work_group_size = 0;

	for (i = 0 ; i < numPlatforms ; i++) {
		status = clGetPlatformInfo(platforms[i], CL_PLATFORM_NAME, info_size, info, NULL);
		OPENCL_ERROR(clGetPlatformInfo)
		if (xmo->verbose)
			fprintf(stdout,"Found OpenCL platform %i name %s\n", i, info);

		cl_uint numDevices = 0;
		cl_device_id *devices = NULL;
		status = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU, 0, NULL, &numDevices);

		if (status == CL_DEVICE_NOT_FOUND || numDevices == 0) {
			continue;
		}
		else if (status != CL_SUCCESS) {
			OPENCL_ERROR(clGetDeviceIDs)
		}

		devices = (cl_device_id *) g_malloc(sizeof(cl_device_id)*numDevices);
		status = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU, numDevices, devices, NULL);
		OPENCL_ERROR(clGetDeviceIDs)


		for (j = 0 ; j < numDevices ; j++) {
			//check implementation... should be 1.1
			status = clGetDeviceInfo(devices[j], CL_DEVICE_VERSION, info_size, info, NULL);
			OPENCL_ERROR(clGetDeviceInfo)

			//parse string;
			int cl_minor, cl_major;

			sscanf(info, "OpenCL %i.%i ",&cl_major, &cl_minor);

			if (cl_major < XMI_OPENCL_MAJOR || (cl_major == XMI_OPENCL_MAJOR && cl_minor < XMI_OPENCL_MINOR))
				continue;

			status = clGetDeviceInfo(devices[j], CL_DEVICE_NAME, info_size, info, NULL);
			OPENCL_ERROR(clGetDeviceInfo)
			if (xmo->verbose)
				fprintf(stdout,"Found OpenCL device %i name %s\n", j, info);

			size_t max_work_group_size_temp;
			status = clGetDeviceInfo(devices[j], CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(size_t), &max_work_group_size_temp, NULL);
			OPENCL_ERROR(clGetDeviceInfo)
			if (max_work_group_size_temp > max_work_group_size) {
				device = devices[j];
				max_work_group_size = max_work_group_size_temp;
			}
		}
		g_free(devices);
	}
	g_free(platforms);



	if (max_work_group_size == 0) {
		fprintf(stderr, "No suitable OpenCL GPU devices detected\n");
		return 0;
	}


	cl_context context = NULL;
	context = clCreateContext(NULL, 1, &device, NULL, NULL, &status);
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

	xmi_solid_angle *sa = *solid_angle;

	//fprintf(stdout,"grid_dims_r_n: %li\n", sa->grid_dims_r_n);
	//fprintf(stdout,"grid_dims_theta_n: %li\n", sa->grid_dims_theta_n);
	//fprintf(stdout,"hits_per_single: %li\n",hits_per_single);

	//other arguments needed
	//	detector%collimator_present
	//	detector%detector_radius
	//	q
	//	detector%collimator_radius
	//	detector%collimator_height
	//	hits_per_single
	//

	float *grid_dims_r_vals_float = (float *) g_malloc(sizeof(float)*sa->grid_dims_r_n);
	float *grid_dims_theta_vals_float = (float *) g_malloc(sizeof(float)*sa->grid_dims_theta_n);

	for (i = 0 ; i < sa->grid_dims_r_n ; i++)
		grid_dims_r_vals_float[i] = (float) sa->grid_dims_r_vals[i];

	for (i = 0 ; i < sa->grid_dims_theta_n ; i++)
		grid_dims_theta_vals_float[i] = (float) sa->grid_dims_theta_vals[i];

	float *solid_angles_float = (float *) g_malloc(sizeof(float) * sa->grid_dims_r_n * sa->grid_dims_theta_n);


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
	float *input1 = (float *) g_malloc(sizeof(float));
	float *input2 = (float *) g_malloc(sizeof(float));
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

	GResource *xmi_resource = xmi_get_resource();
	const gchar * const filenames[] = {
		"openclfeatures.h",
		"compilerfeatures.h",
		"sse.h",
		"array.h",
		"threefry.h",
		"xmi_kernels.cl",
	};

	GString *kernel_code = g_string_sized_new(4096);

	for (i = 0 ; i < G_N_ELEMENTS(filenames) ; i++) {
		gchar *kernel_file = g_strdup_printf("/com/github/tschoonj/xmimsim/gpu/%s", filenames[i]);
		GError *error = NULL;
		GBytes *source_code = g_resource_lookup_data(xmi_resource, kernel_file, G_RESOURCE_LOOKUP_FLAGS_NONE, &error);
		if (!source_code) {
			g_fprintf(stderr,"Could not open resource %s -> %s\n", kernel_file, error->message);
			g_error_free(error);
			return 0;
		}
		g_string_append(kernel_code, g_bytes_get_data(source_code, NULL));
		g_free(kernel_file);
		g_bytes_unref(source_code);
	}

	cl_program myprog = clCreateProgramWithSource(context, 1, (const char **) &kernel_code->str, NULL, &status);
	OPENCL_ERROR(clCreateProgramWithSource)
	g_string_free(kernel_code, TRUE);

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
		char *my_log = (char *) g_malloc(log_size+1);

	        // Get the log
		status = clGetProgramBuildInfo(myprog, device, CL_PROGRAM_BUILD_LOG, log_size, my_log, NULL);
		OPENCL_ERROR(clGetProgramBuildInfo)

		// Print the log
		g_fprintf(stderr, "%s\n", my_log);
		g_free(my_log);
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
			kernelEvent = (cl_event*) g_malloc(sizeof(kernelEvent));
			offset[0] = i*sa->grid_dims_r_n/RANGE_DIVIDER;
			offset[1] = j*sa->grid_dims_theta_n/RANGE_DIVIDER;
			status = clEnqueueNDRangeKernel(cmdQueue, mykernel, 2, offset, globalws, NULL, 0, NULL, kernelEvent);
			OPENCL_ERROR(clEnqueueNDRangeKernel)
			status = clWaitForEvents(1, kernelEvent);
			OPENCL_ERROR(clWaitForEvents)
			status = clReleaseEvent(kernelEvent[0]);
			OPENCL_ERROR(clReleaseEvent)
			g_free(kernelEvent);
			if (xmo->verbose)
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

	g_free(solid_angles_float);
	g_free(grid_dims_r_vals_float);
	g_free(grid_dims_theta_vals_float);

	if (xmo->verbose)
		fprintf(stdout,"Solid angle calculation finished\n");
	return 1;
}
