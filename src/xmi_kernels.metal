/*
Copyright (C) 2019 Tom Schoonjans and Laszlo Vincze

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

#include <Random123/threefry.h>
#include <metal_stdlib>

using namespace metal;

/*
 * counthits generates n x,y points and returns hits[tid] with
 * the count of number of those points within the unit circle on
 * each thread.
 */
kernel void counthits(const device unsigned  &n [[ buffer(0) ]],
                            device uint *hitsp [[ buffer(1) ]],
                            device uint *triesp [[ buffer(2) ]],
			           uint     tid [[thread_position_in_grid]]) {
    unsigned hits = 0, tries = 0;
    threefry4x32_key_t k = {{tid, 0xdecafbad, 0xfacebead, 0x12345678}};
    threefry4x32_ctr_t c = {{0, 0xf00dcafe, 0xdeadbeef, 0xbeeff00d}};
    const float uint_max_fl = (float) UINT_MAX;
    while (tries < n) {
	union {
	    threefry4x32_ctr_t c;
	    uint4 i;
	} u;
	c.v[0]++;
	u.c = threefry4x32(c, k);
	float x1 = ((float) u.i.x) / uint_max_fl, y1 = ((float) u.i.y) / uint_max_fl;
	float x2 = ((float) u.i.z) / uint_max_fl, y2 = ((float) u.i.w) / uint_max_fl;
	if ((x1*x1 + y1*y1) < (1.0)) {
	    hits++;
	}
	tries++;
	if ((x2*x2 + y2*y2) < (1.0)) {
	    hits++;
	}
	tries++;
    }
    hitsp[tid] = hits;
    triesp[tid] = tries;
}

typedef struct {
	float3 point;
	float3 normv;
} xmi_plane;

typedef struct {
	float3 point;
	float3 dirv;
} xmi_line;

float3 xmi_intersection_plane_line(xmi_plane plane, xmi_line line) {
	float3 point;
	float ItimesN, d;

	ItimesN = dot(line.dirv, plane.normv);

	d = dot(plane.point - line.point, plane.normv)/ItimesN;
	point = d*line.dirv + line.point;

	return point;
}

kernel void xmi_solid_angle_calculation(device float *grid_dims_r_vals [[ buffer(0) ]],
					device float *grid_dims_theta_vals [[ buffer(1) ]],
					device float *solid_angles [[ buffer(2) ]],
					const device int &collimator_present [[ buffer(3) ]],
					const device float &detector_radius [[ buffer(4) ]],
					const device float &collimator_radius [[ buffer(5) ]],
					const device float &collimator_height [[ buffer(6) ]],
					const device int &hits_per_single [[ buffer(7) ]],
					const device unsigned int &tid_offset0 [[ buffer(8) ]],
					const device unsigned int &tid_offset1 [[ buffer(9) ]],
					uint2 tid [[ thread_position_in_grid ]],
					uint2 gid [[ threads_per_grid ]]) {

	const uint tid0 = tid_offset0 + tid.x;
	const uint tid1 = tid_offset1 + tid.y;
	const uint tid_max0 = gid.x * RANGE_DIVIDER;
	//const uint tid_max1 = gid.y * RANGE_DIVIDER;
	threefry4x32_key_t k = {{tid0, 0xdecafbad, 0xfacebead, tid1}};
	threefry4x32_ctr_t c = {{0, 0xf00dcafe, 0xdeadbeef, 0xbeeff00d}};

	union {
            threefry4x32_ctr_t c;
            uint4 i;
        } u;

	float rn1, rn2;
	const float uint_max_fl = (float) UINT_MAX;

	float r1 = grid_dims_r_vals[tid0];
	float theta1 = grid_dims_theta_vals[tid1];

	const float3 detector_normal(0.0, 0.0, 1.0);
	float3 cone_base_normal;
	float full_cone_solid_angle;
	float full_cone_base_radius;
	float full_cone_apex;
	float3 rotation_matrix_r1;
	float3 rotation_matrix_r2;
	float3 rotation_matrix_r3;
	float theta_rng, phi_rng;
	int detector_hits;
	float3 dirv_from_cone;
	float3 dirv_from_detector;
	xmi_plane detector_plane, collimator_plane;
	xmi_line photon_line;
	float3 intersection_point;
	int i;
	float r;
	float theta;
	int outside_collimator;
	float alpha1, beta;
	float cos_full_cone_apex;

	float result;

#ifdef DEBUG

	printf("tid0: %i\n", (int) tid0);
	printf("tid1: %i\n", (int) tid1);
	printf("tid_max0: %i\n", (int) tid_max0);
	printf("tid_max1: %i\n", (int) tid_max1);
	printf("r1: %f\n", r1);
	printf("theta1: %f\n", theta1);
	printf("collimator_present: %i\n", collimator_present);
	printf("detector_radius: %f\n", detector_radius);
	printf("collimator_radius: %f\n", collimator_radius);
	printf("collimator_height: %f\n", collimator_height);
	printf("hits_per_single %i\n", hits_per_single);

#endif



	if (!collimator_present) {
		r = r1;
		theta = theta1;
		full_cone_base_radius = detector_radius;
		outside_collimator = 0;
	}
	else if (fabs(collimator_radius-detector_radius) < 0.000001) {
		if (r1*cos(theta1) <= detector_radius) {
			r = r1;
			theta = theta1;
			full_cone_base_radius = detector_radius;
		}
		else {
                        r = sqrt(r1*r1 - 2.0*r1*sin(theta1)*collimator_height + collimator_height*collimator_height);
                        theta = acos(r1*cos(theta1)/r);
                        full_cone_base_radius = collimator_radius;
		}
		if (r1*sin(theta1) > collimator_height) {
			outside_collimator = 1;
		}
		else {
			outside_collimator = 0;
		}
	}
	else {
		//conical collimator
                if (r1*cos(theta1) <= detector_radius &&
                r1*sin(theta1) <=
                (collimator_height)*(r1*cos(theta1)-
                detector_radius)/(collimator_radius
                -detector_radius)) {
                        r = r1;
                        theta = theta1;
                        full_cone_base_radius = detector_radius;
		}
                else if (r1*sin(theta1) <= collimator_height) {
			solid_angles[tid1*tid_max0 + tid0] = 0.0;
			return;
		}
                else { 
                        r = sqrt(r1*r1 - 
                        2.0*r1*sin(theta1)*collimator_height +
                        collimator_height*collimator_height);
                        theta = acos(r1*cos(theta1)/r);
                        full_cone_base_radius = collimator_radius;
		}
                if (r1*sin(theta1) > collimator_height)
                        outside_collimator = 1;
               	else 
                        outside_collimator = 0;
                
	}

        //define proper cone
        //calculate angle between actual detector plane and base of new cone
        cone_base_normal.x = 0.0;
        cone_base_normal.y = cos(theta);
        cone_base_normal.z = sin(theta);
	
	beta = atan(full_cone_base_radius/r);
        alpha1 = atan(full_cone_base_radius*sin(theta)/
        (r - full_cone_base_radius*cos(theta)));

	if (alpha1 <= 0.0)
		alpha1=alpha1+M_PI_F;
	
        full_cone_apex = max(beta, alpha1);

        cos_full_cone_apex = cos(full_cone_apex);

        full_cone_solid_angle = 2*M_PI_F*(1.0-cos_full_cone_apex);

	rotation_matrix_r1.x = 1.0;
	rotation_matrix_r1.y = 0.0;
	rotation_matrix_r1.z = 0.0;
	rotation_matrix_r2.x = 0.0;
	rotation_matrix_r2.y = -1.0*sin(theta);
	rotation_matrix_r2.z = -1.0*cos(theta);
	rotation_matrix_r3.x = 0.0;
	rotation_matrix_r3.y = cos(theta);
	rotation_matrix_r3.z = -1.0*sin(theta);


	detector_hits = 0;

	detector_plane.point.x = 0.0;
	detector_plane.point.y = 0.0;
	detector_plane.point.z = 0.0;
	detector_plane.normv.x = detector_normal.x;
	detector_plane.normv.y = detector_normal.y;
	detector_plane.normv.z = detector_normal.z;

	if (outside_collimator) {
		collimator_plane.point.x = 0.0;
		collimator_plane.point.y = 0.0;
		collimator_plane.point.z = collimator_height;
		collimator_plane.normv.x = detector_normal.x;
		collimator_plane.normv.y = detector_normal.y;
		collimator_plane.normv.z = detector_normal.z;
	}
	
	photon_line.point.x = 0.0;
	photon_line.point.y = r1*cos(theta1);
	photon_line.point.z = r1*sin(theta1);

	for (i = 0 ; i < hits_per_single ; i++) {
		if (i % 2 == 0) {
			//if even get 4 new random numbers
			c.v[0]++;
			u.c = threefry4x32(c, k);
			rn1 = ((float) u.i.x) / uint_max_fl;
			rn2 = ((float) u.i.y) / uint_max_fl;
		}		       
		else {
			rn1 = ((float) u.i.z) / uint_max_fl;
			rn2 = ((float) u.i.w) / uint_max_fl;
		}
		//rn1=0.5;
		//rn2=0.5;

		theta_rng = acos(1.0 -rn1*(1.0-cos_full_cone_apex));
		phi_rng = rn2*2.0*M_PI_F;

		dirv_from_cone.x = sin(theta_rng) * cos(phi_rng);
		dirv_from_cone.y = sin(theta_rng) * sin(phi_rng);
		dirv_from_cone.z = cos(theta_rng);

		dirv_from_detector.x = dot(rotation_matrix_r1,dirv_from_cone);
		dirv_from_detector.y = dot(rotation_matrix_r2,dirv_from_cone);
		dirv_from_detector.z = dot(rotation_matrix_r3,dirv_from_cone);

		photon_line.dirv.x = dirv_from_detector.x;
		photon_line.dirv.y = dirv_from_detector.y;
		photon_line.dirv.z = dirv_from_detector.z;

		if (dot(detector_normal, dirv_from_detector) >= 0.0) 
			continue;

		if (outside_collimator) {
			intersection_point = xmi_intersection_plane_line(collimator_plane, photon_line);
			intersection_point.z = 0.0;
			if (length(intersection_point) > collimator_radius) {
				continue;
			}
		}
		
		intersection_point = xmi_intersection_plane_line(detector_plane, photon_line);
		if (length(intersection_point) <= detector_radius)
			detector_hits++;

	}
	result = full_cone_solid_angle*((float) detector_hits)/((float) hits_per_single);	
#ifdef DEBUG
	printf("result: %f\n",result);
	printf("beta: %f\n",beta);
	printf("alpha1: %f\n",alpha1);
	printf("detector_hits: %i\n",detector_hits);
	printf("intersection_point %v3f\n", intersection_point);
#endif


	solid_angles[tid1*tid_max0 + tid0] = result;
}
