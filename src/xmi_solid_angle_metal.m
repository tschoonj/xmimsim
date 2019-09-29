/*
Copyright (C) 2010-2019 Tom Schoonjans and Laszlo Vincze

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

#import <Metal/Metal.h>

#ifdef MAC_INTEGRATION
  #include "xmi_resources_mac.h"
#endif

#include "xmi_resources.h"

#define RANGE_DIVIDER 8

extern void xmi_solid_angle_inputs_f(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, int *collimator_present, float *detector_radius, float *collimator_radius, float *collimator_height);

extern long hits_per_single;

G_MODULE_EXPORT int xmi_solid_angle_calculation_metal(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *xmo);

G_MODULE_EXPORT int xmi_solid_angle_calculation_metal(xmi_inputFPtr inputFPtr, xmi_solid_angle **solid_angle, char *input_string, xmi_main_options *xmo) {

	@autoreleasepool {

		id<MTLDevice> device;
		id<MTLCommandQueue> queue;
		id<MTLLibrary> library;
		id<MTLFunction> function;

		NSError *error = nil;
		NSArray<id<MTLDevice>> *devices;
		unsigned int i, j;

		// get default device
		device = MTLCreateSystemDefaultDevice();
		if (!device) {
			fprintf(stderr, "Could not create Metal system default device\n");
			return 0;
		}

		if (xmo->verbose)
			fprintf(stdout,"Found Metal device %s\n", device.name.UTF8String);

		// create command queue
		queue = [device newCommandQueue];
		if (!queue) {
			fprintf(stderr, "Could not create Metal command queue\n");
			return 0;
		}

		// get Metal library
		gchar *metal_kernel = NULL;
		const gchar *metal_kernel_env = g_getenv("XMIMSIM_METAL_KERNEL");
		if (metal_kernel_env != NULL) {
			metal_kernel = g_strdup(metal_kernel_env);
		}
#if defined(MAC_INTEGRATION)
		else if (xmi_resources_mac_query(XMI_RESOURCES_MAC_METAL_KERNEL, &metal_kernel) == 0)
			return 0;
		}
#endif
		else {
			metal_kernel = g_strdup(XMI_METAL_KERNEL);
		}

		if (xmo->verbose)
			fprintf(stdout,"Using Metal library from %s\n", metal_kernel);
	
		library = [device newLibraryWithFile: [[NSString alloc] initWithUTF8String: metal_kernel] error: &error];
		if (!library) {
			fprintf(stderr, "Could not open Metal library %s: %s\n", metal_kernel, error.localizedDescription.UTF8String);
			return 0;
		}

		function = [library newFunctionWithName: @"xmi_solid_angle_calculation"];
		if (!function) {
			fprintf(stderr, "Could not create Metal kernel %s\n", "xmi_solid_angle_calculation");
			return 0;
		}

		//start assembling our input
		int collimator_present;
		float detector_radius;
		float collimator_radius;
		float collimator_height;

		xmi_solid_angle_inputs_f(inputFPtr, solid_angle, &collimator_present, &detector_radius, &collimator_radius, &collimator_height);

		xmi_solid_angle *sa = *solid_angle;

		float *grid_dims_r_vals_float = (float *) g_malloc(sizeof(float) * sa->grid_dims_r_n);
		float *grid_dims_theta_vals_float = (float *) g_malloc(sizeof(float) * sa->grid_dims_theta_n);

		for (i = 0 ; i < sa->grid_dims_r_n ; i++)
			grid_dims_r_vals_float[i] = (float) sa->grid_dims_r_vals[i];

		for (i = 0 ; i < sa->grid_dims_theta_n ; i++)
			grid_dims_theta_vals_float[i] = (float) sa->grid_dims_theta_vals[i];

		id<MTLBuffer> grid_dims_r_vals_metal = [device newBufferWithBytes: grid_dims_r_vals_float length: sizeof(float) * sa->grid_dims_r_n options: MTLResourceStorageModePrivate];
		if (!grid_dims_r_vals_metal) {
			fprintf(stderr, "Could not create Metal buffer: %s\n", "grid_dims_r_vals_metal");
			return 0;
		}

		id<MTLBuffer> grid_dims_theta_vals_metal = [device newBufferWithBytes: grid_dims_theta_vals_float length: sizeof(float) * sa->grid_dims_theta_n options: MTLResourceStorageModePrivate];
		if (!grid_dims_theta_vals_metal) {
			fprintf(stderr, "Could not create Metal buffer: %s\n", "grid_dims_theta_vals_metal");
			return 0;
		}

		id<MTLBuffer> solid_angles_metal = [device newBufferWithLength: sizeof(float) * sa->grid_dims_r_n * sa->grid_dims_theta_n options: MTLResourceStorageModeShared];
		if (!solid_angles_metal) {
			fprintf(stderr, "Could not create Metal buffer: %s\n", "solid_angles_metal");
			return 0;
		}

		for (i = 0 ; i < RANGE_DIVIDER ; i++) {
			for (j = 0 ; j < RANGE_DIVIDER ; j++) {
				@autoreleasepool {
					id<MTLCommandBuffer> buffer;
					id<MTLComputeCommandEncoder> encoder;
					id<MTLComputePipelineState> pipeline;
					
					unsigned int tid_offset0 = i * sa->grid_dims_r_n / RANGE_DIVIDER;
					unsigned int tid_offset1 = j * sa->grid_dims_theta_n / RANGE_DIVIDER;

					buffer = [queue commandBuffer];
					encoder = [buffer computeCommandEncoder];
					pipeline = [device newComputePipelineStateWithFunction: function error: &error];
					[encoder setComputePipelineState:pipeline];

					[encoder setBuffer: grid_dims_r_vals_metal offset: 0 atIndex: 0];
					[encoder setBuffer: grid_dims_theta_vals_metal offset: 0 atIndex: 1];
					[encoder setBuffer: solid_angles_metal offset: 0 atIndex: 2];
					[encoder setBytes: &collimator_present length: sizeof(int) atIndex: 3];
					[encoder setBytes: &detector_radius length: sizeof(float) atIndex: 4];
					[encoder setBytes: &collimator_radius length: sizeof(float) atIndex: 5];
					[encoder setBytes: &collimator_height length: sizeof(float) atIndex: 6];
					[encoder setBytes: &hits_per_single length: sizeof(int) atIndex: 7];
					[encoder setBytes: &tid_offset0 length: sizeof(unsigned int) atIndex: 8];
					[encoder setBytes: &tid_offset1 length: sizeof(unsigned int) atIndex: 9];

					NSUInteger w = [pipeline threadExecutionWidth];
					NSUInteger h = [pipeline maxTotalThreadsPerThreadgroup] / w;
					MTLSize threadsPerThreadgroup = MTLSizeMake(w, h, 1);

					MTLSize grid = MTLSizeMake(sa->grid_dims_r_n/RANGE_DIVIDER, sa->grid_dims_theta_n/RANGE_DIVIDER, 1);
					[encoder dispatchThreads:grid threadsPerThreadgroup:threadsPerThreadgroup];
					[encoder endEncoding];
					[buffer commit];
					[buffer waitUntilCompleted];

					if (xmo->verbose)
						fprintf(stdout,"Solid angle calculation at %3i %%\n",(int) floor(100.0*(float)(RANGE_DIVIDER*i+j+1)/(float)(RANGE_DIVIDER*RANGE_DIVIDER)));
				}
			}
		}

		// copy metal buffer into solid_angles struct
		float *solid_angles_float = [solid_angles_metal contents];
		for (i = 0 ; i < sa->grid_dims_r_n * sa->grid_dims_theta_n ; i++) {
			sa->solid_angles[i] = (double) solid_angles_float[i];
		}
		sa->xmi_input_string = input_string;

		g_free(grid_dims_r_vals_float);
		g_free(grid_dims_theta_vals_float);

	}
	if (xmo->verbose)
		fprintf(stdout,"Solid angle calculation finished\n");
	return 1;
}
