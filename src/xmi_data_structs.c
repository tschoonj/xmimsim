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
#include "xmi_aux.h"
#include "xmi_lines.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>


void xmi_free_layer (struct xmi_layer *layer) {
	free(layer->Z);
	free(layer->weight);
}





void xmi_free_input(struct xmi_input *input) {
	//general
	free(input->general->outputfile);
	free(input->general->comments);
	free(input->general);

	//composition
	xmi_free_composition(input->composition);

	//geometry
	xmi_free_geometry(input->geometry);

	//excitation
	xmi_free_excitation(input->excitation);

	//absorbers
	xmi_free_absorbers(input->absorbers);

	//detector
	xmi_free_detector(input->detector);

	//input
	free(input);


}


void xmi_copy_input(struct xmi_input *A, struct xmi_input **B) {
	int i;

	//allocate space for B
	*B = (struct xmi_input *) malloc(sizeof(struct xmi_input));

	//general
	//(*B)->general = (struct xmi_general *) malloc(sizeof(struct xmi_general));
	(*B)->general = (struct xmi_general *) xmi_memdup((A)->general, sizeof(struct xmi_general));
	(*B)->general->outputfile = strdup(A->general->outputfile);
	(*B)->general->comments= strdup(A->general->comments);

	//composition
	xmi_copy_composition(A->composition, &((*B)->composition));
	
	//geometry
	xmi_copy_geometry(A->geometry, &((*B)->geometry));

	//excitation
	xmi_copy_excitation(A->excitation, &((*B)->excitation));

	//absorbers
	xmi_copy_absorbers(A->absorbers, &((*B)->absorbers));

	//detector
	xmi_copy_detector(A->detector, &((*B)->detector));

	
	return;
}


int xmi_compare_input(struct xmi_input *A, struct xmi_input *B) {
	int rv;
	int i,j;
	double *temparr1;
	double *temparr2;
	
	//Yes, I know every textbook on programming says not to use the goto construct but I'm going to do it anyway :-)
	//Don't try this at home though!
	
	rv = 0;

	//general
	if (A->general->version != B->general->version) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

	if (strcmp(A->general->outputfile,B->general->outputfile) != 0) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

	if (A->general->n_photons_interval != B->general->n_photons_interval) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

	if (A->general->n_photons_line != B->general->n_photons_line) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

	if (A->general->n_interactions_trajectory != B->general->n_interactions_trajectory) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

	if (strcmp(A->general->comments,B->general->comments) != 0) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}


	after_general:


	//composition
	if (A->composition->n_layers != B->composition->n_layers) {
		rv |= XMI_CONFLICT_COMPOSITION;
	}
	else if (A->composition->reference_layer != B->composition->reference_layer) {
		rv |= XMI_CONFLICT_COMPOSITION;
	}
	else {
		for (i = 0 ; i < A->composition->n_layers ; i++) {
			if (A->composition->layers[i].n_elements != B->composition->layers[i].n_elements) {
				rv |= XMI_CONFLICT_COMPOSITION;
				break;
			}
			else {
				for (j = 0 ; j < A->composition->layers[i].n_elements ; j++) {
					if (A->composition->layers[i].Z[j] != B->composition->layers[i].Z[j]) {
						rv |= XMI_CONFLICT_COMPOSITION;
						goto after_composition;	
					}	
					else if (fabsl(A->composition->layers[i].weight[j]- B->composition->layers[i].weight[j])/A->composition->layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_COMPOSITION;
						goto after_composition;	
					}	
				} 
				if (fabsl(A->composition->layers[i].density - B->composition->layers[i].density)/A->composition->layers[i].density > XMI_COMPARE_THRESHOLD) {
					rv |= XMI_CONFLICT_COMPOSITION;
					break;
				}
				if (fabsl(A->composition->layers[i].thickness- B->composition->layers[i].thickness)/A->composition->layers[i].thickness > XMI_COMPARE_THRESHOLD) {
					rv |= XMI_CONFLICT_COMPOSITION;
					break;
				}
			}
		}
	}

	after_composition:

	//geometry
#define XMI_IF_COMPARE_GEOMETRY(a) if (fabsl(A->geometry->a - B->geometry->a)/fabs(A->geometry->a) > XMI_COMPARE_THRESHOLD){\
	rv |= XMI_CONFLICT_GEOMETRY;\
	goto after_geometry;\
	}	
#define XMI_IF_COMPARE_GEOMETRY2(a) if (fabsl(A->geometry->a - B->geometry->a) > XMI_COMPARE_THRESHOLD){\
	rv |= XMI_CONFLICT_GEOMETRY;\
	goto after_geometry;\
	}	

#define XMI_IF_COMPARE_GEOMETRY3(a,b) if (fabsl(a - b) > XMI_COMPARE_THRESHOLD){\
	rv |= XMI_CONFLICT_GEOMETRY;\
	goto after_geometry;\
	}	

	XMI_IF_COMPARE_GEOMETRY(d_sample_source)
	//should compare normalized orientations...
	temparr1 = (double *) xmi_memdup(A->geometry->n_sample_orientation,sizeof(double)*3);
	temparr2 = (double *) xmi_memdup(B->geometry->n_sample_orientation,sizeof(double)*3);
	xmi_normalize_vector_double(temparr1, 3);
	xmi_normalize_vector_double(temparr2, 3);

	XMI_IF_COMPARE_GEOMETRY3(temparr1[0],temparr2[0])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[1],temparr2[1])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[2],temparr2[2])
	free(temparr1);
	free(temparr2);
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[0])
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[1])
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[2])
	//should compare normalized orientations...
	temparr1 = (double *) xmi_memdup(A->geometry->n_detector_orientation,sizeof(double)*3);
	temparr2 = (double *) xmi_memdup(B->geometry->n_detector_orientation,sizeof(double)*3);
	xmi_normalize_vector_double(temparr1, 3);
	xmi_normalize_vector_double(temparr2, 3);
	XMI_IF_COMPARE_GEOMETRY3(temparr1[0],temparr2[0])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[1],temparr2[1])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[2],temparr2[2])
	free(temparr1);
	free(temparr2);
	XMI_IF_COMPARE_GEOMETRY(area_detector)
	XMI_IF_COMPARE_GEOMETRY2(collimator_height)
	XMI_IF_COMPARE_GEOMETRY2(collimator_diameter)
	XMI_IF_COMPARE_GEOMETRY2(d_source_slit)
	XMI_IF_COMPARE_GEOMETRY2(slit_size_x)
	XMI_IF_COMPARE_GEOMETRY2(slit_size_y)

	after_geometry:

#define XMI_IF_COMPARE_EXCITATION_DISCRETE(a) if (fabsl(A->excitation->discrete[i].a-B->excitation->discrete[i].a)/A->excitation->discrete[i].a > XMI_COMPARE_THRESHOLD) {\
					rv |= XMI_CONFLICT_EXCITATION;\
					break;\
				}

	//excitation
	if (A->excitation->n_discrete > 0 || B->excitation->n_discrete > 0) {
		if (A->excitation->n_discrete != B->excitation->n_discrete) {
			rv |= XMI_CONFLICT_EXCITATION;
		}
		else {
			for (i = 0 ; i < A->excitation->n_discrete ; i++) {
				XMI_IF_COMPARE_EXCITATION_DISCRETE(energy)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(horizontal_intensity)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(vertical_intensity)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(sigma_x)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(sigma_xp)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(sigma_y)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(sigma_yp)
				if (A->excitation->discrete[i].distribution_type != B->excitation->discrete[i].distribution_type) {
					rv |= XMI_CONFLICT_EXCITATION;
					break;
				}
				else if (A->excitation->discrete[i].distribution_type != XMI_DISCRETE_MONOCHROMATIC) {
					XMI_IF_COMPARE_EXCITATION_DISCRETE(scale_parameter)
				}
			}
		}
	}	

#define XMI_IF_COMPARE_EXCITATION_CONTINUOUS(a) if (fabsl(A->excitation->continuous[i].a-B->excitation->continuous[i].a)/A->excitation->continuous[i].a > XMI_COMPARE_THRESHOLD) {\
					rv |= XMI_CONFLICT_EXCITATION;\
					break;\
				}
	if (A->excitation->n_continuous > 0 || B->excitation->n_continuous > 0) {
		if (A->excitation->n_continuous != B->excitation->n_continuous) {
			rv |= XMI_CONFLICT_EXCITATION;
		}
		else {
			for (i = 0 ; i < A->excitation->n_continuous ; i++) {
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(energy)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(horizontal_intensity)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(vertical_intensity)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(sigma_x)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(sigma_xp)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(sigma_y)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(sigma_yp)
			}
		}
	}	

	//absorbers
	if (A->absorbers->n_exc_layers > 0 || B->absorbers->n_exc_layers > 0) {
		if (A->absorbers->n_exc_layers != B->absorbers->n_exc_layers) {
			rv |= XMI_CONFLICT_ABSORBERS;
		}
		else {
			for (i = 0 ; i < A->absorbers->n_exc_layers ; i++) {
				if (A->absorbers->exc_layers[i].n_elements != B->absorbers->exc_layers[i].n_elements) {
					rv |= XMI_CONFLICT_ABSORBERS;
					break;
				}
				else {
					for (j = 0 ; j < A->absorbers->exc_layers[i].n_elements ; j++) {
						if (A->absorbers->exc_layers[i].Z[j] != B->absorbers->exc_layers[i].Z[j]) {
							rv |= XMI_CONFLICT_ABSORBERS;
							goto after_absorbers;	
						}	
						else if (fabsl(A->absorbers->exc_layers[i].weight[j]- B->absorbers->exc_layers[i].weight[j])/A->absorbers->exc_layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
							rv |= XMI_CONFLICT_ABSORBERS;
							goto after_absorbers;	
						}	
					} 
					if (fabsl(A->absorbers->exc_layers[i].density - B->absorbers->exc_layers[i].density)/A->absorbers->exc_layers[i].density > XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_ABSORBERS;
						break;
					}
					if (fabsl(A->absorbers->exc_layers[i].thickness- B->absorbers->exc_layers[i].thickness)/A->absorbers->exc_layers[i].thickness > XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_ABSORBERS;
						break;
					}
				}
			}
		}
	}
	if (A->absorbers->n_det_layers > 0 || B->absorbers->n_det_layers > 0) {
		if (A->absorbers->n_det_layers != B->absorbers->n_det_layers) {
			rv |= XMI_CONFLICT_ABSORBERS;
		}
		else {
			for (i = 0 ; i < A->absorbers->n_det_layers ; i++) {
				if (A->absorbers->det_layers[i].n_elements != B->absorbers->det_layers[i].n_elements) {
					rv |= XMI_CONFLICT_ABSORBERS;
					break;
				}
				else {
					for (j = 0 ; j < A->absorbers->det_layers[i].n_elements ; j++) {
						if (A->absorbers->det_layers[i].Z[j] != B->absorbers->det_layers[i].Z[j]) {
							rv |= XMI_CONFLICT_ABSORBERS;
							goto after_absorbers;	
						}	
						else if (fabsl(A->absorbers->det_layers[i].weight[j]- B->absorbers->det_layers[i].weight[j])/A->absorbers->det_layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
							rv |= XMI_CONFLICT_ABSORBERS;
							goto after_absorbers;	
						}	
					} 
					if (fabsl(A->absorbers->det_layers[i].density - B->absorbers->det_layers[i].density)/A->absorbers->det_layers[i].density > XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_ABSORBERS;
						break;
					}
					if (fabsl(A->absorbers->det_layers[i].thickness- B->absorbers->det_layers[i].thickness)/A->absorbers->det_layers[i].thickness > XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_ABSORBERS;
						break;
					}
				}
			}
		}
	}

	after_absorbers:

	//detector
	if (A->detector->detector_type != B->detector->detector_type) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}

#define XMI_IF_COMPARE_DETECTOR(a) if (fabsl(A->detector->a - B->detector->a) > XMI_COMPARE_THRESHOLD){\
	rv |= XMI_CONFLICT_DETECTOR;\
	goto after_detector;\
	}

	XMI_IF_COMPARE_DETECTOR(live_time)
	XMI_IF_COMPARE_DETECTOR(pulse_width)
	XMI_IF_COMPARE_DETECTOR(gain)
	XMI_IF_COMPARE_DETECTOR(zero)
	XMI_IF_COMPARE_DETECTOR(fano)
	XMI_IF_COMPARE_DETECTOR(noise)
	XMI_IF_COMPARE_DETECTOR(max_convolution_energy)

	if (A->detector->n_crystal_layers != B->detector->n_crystal_layers) {
		rv |= XMI_CONFLICT_DETECTOR;
	}
	else {
		for (i = 0 ; i < A->detector->n_crystal_layers ; i++) {
			if (A->detector->crystal_layers[i].n_elements != B->detector->crystal_layers[i].n_elements) {
				rv |= XMI_CONFLICT_DETECTOR;
				break;
			}
			else {
				for (j = 0 ; j < A->detector->crystal_layers[i].n_elements ; j++) {
					if (A->detector->crystal_layers[i].Z[j] != B->detector->crystal_layers[i].Z[j]) {
						rv |= XMI_CONFLICT_DETECTOR;
						goto after_detector;	
					}	
					else if (fabsl(A->detector->crystal_layers[i].weight[j]- B->detector->crystal_layers[i].weight[j])/A->detector->crystal_layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_DETECTOR;
						goto after_detector;	
					}	
				} 
				if (fabsl(A->detector->crystal_layers[i].density - B->detector->crystal_layers[i].density)/A->detector->crystal_layers[i].density > XMI_COMPARE_THRESHOLD) {
					rv |= XMI_CONFLICT_DETECTOR;
					break;
				}
				if (fabsl(A->detector->crystal_layers[i].thickness- B->detector->crystal_layers[i].thickness)/A->detector->crystal_layers[i].thickness > XMI_COMPARE_THRESHOLD) {
					rv |= XMI_CONFLICT_DETECTOR;
					break;
				}
			}
		}
	}



	after_detector:

	return rv;


}

void xmi_free_composition(struct xmi_composition *composition) {
	int i;

	for (i = 0 ; i < composition->n_layers ; i++) 
		xmi_free_layer(composition->layers+i);
	 
	free(composition->layers);

	free(composition);
}

void xmi_copy_composition(struct xmi_composition *A, struct xmi_composition **B) {
	int i;

	//allocate space for B
	*B = (struct xmi_composition *) malloc(sizeof(struct xmi_composition));
	(*B)->n_layers = A->n_layers;
	(*B)->reference_layer = A->reference_layer;
	(*B)->layers = (struct xmi_layer *) xmi_memdup((A)->layers,((A)->n_layers)*sizeof(struct xmi_layer));
	for (i = 0 ; i < (A)->n_layers ; i++) {
		(*B)->layers[i].Z = (int *) xmi_memdup((A)->layers[i].Z,((A)->layers[i].n_elements)*sizeof(int));
		(*B)->layers[i].weight = (double *) xmi_memdup((A)->layers[i].weight,((A)->layers[i].n_elements)*sizeof(double));
	}


}

void xmi_copy_layer(struct xmi_layer *A, struct xmi_layer **B) {
	//allocate space for B
	*B = (struct xmi_layer *) malloc(sizeof(struct xmi_layer));
	(*B)->n_elements = A->n_elements;
	(*B)->density = A->density;
	(*B)->thickness = A->thickness;
	(*B)->Z = (int *) xmi_memdup(A->Z, A->n_elements*sizeof(int));
	(*B)->weight = (double*) xmi_memdup(A->weight, A->n_elements*sizeof(double));
}


void xmi_copy_layer2(struct xmi_layer *A, struct xmi_layer *B) {
	B->n_elements = A->n_elements;
	B->density = A->density;
	B->thickness = A->thickness;
	B->Z = (int *) xmi_memdup(A->Z, A->n_elements*sizeof(int));
	B->weight = (double*) xmi_memdup(A->weight, A->n_elements*sizeof(double));
}

struct xmi_input *xmi_init_empty_input(void) {

	struct xmi_input *rv;

	rv = (struct xmi_input *) malloc(sizeof(struct xmi_input));

	//general
	rv->general = (struct xmi_general *) malloc(sizeof(struct xmi_general));
	rv->general->version = 1.0;
	rv->general->outputfile = strdup("");
	rv->general->comments= strdup("");
	rv->general->n_photons_interval = 10000;
	rv->general->n_photons_line = 100000;
	rv->general->n_interactions_trajectory = 4;

	//layer
	rv->composition = (struct xmi_composition *) malloc(sizeof(struct xmi_composition));
	rv->composition->n_layers = 0;
	rv->composition->layers = NULL;
	rv->composition->reference_layer = 1;

	//geometry
	rv->geometry = (struct xmi_geometry *) malloc(sizeof(struct xmi_geometry));
	rv->geometry->d_sample_source=100.0;
	rv->geometry->n_sample_orientation[0] = 0.0;
	rv->geometry->n_sample_orientation[1] = -1.0*sqrt(2.0)/2.0;
	rv->geometry->n_sample_orientation[2] = sqrt(2.0)/2.0;
	rv->geometry->p_detector_window[0] = 0.0;
	rv->geometry->p_detector_window[1] = 1.0;
	rv->geometry->p_detector_window[2] = 100.0;
	rv->geometry->n_detector_orientation[0] = 0.0;
	rv->geometry->n_detector_orientation[1] = -1.0;
	rv->geometry->n_detector_orientation[2] = 0.0;
	rv->geometry->area_detector = 0.3;
	//default is NO collimator
	rv->geometry->collimator_height = 0.0;
	rv->geometry->collimator_diameter= 0.0;
	rv->geometry->d_source_slit = 100.0;
	rv->geometry->slit_size_x = 0.001;
	rv->geometry->slit_size_y = 0.001;

	//excitation
	rv->excitation = (struct xmi_excitation *) malloc(sizeof(struct xmi_excitation));
	rv->excitation->n_discrete = 1;
	rv->excitation->n_continuous = 0;
	rv->excitation->continuous = NULL;
	rv->excitation->discrete = (struct xmi_energy_discrete *) malloc(sizeof(struct xmi_energy_discrete));
	rv->excitation->discrete[0].energy = 28.0;
	rv->excitation->discrete[0].horizontal_intensity= 1E12;
	rv->excitation->discrete[0].vertical_intensity= 1E9;
	rv->excitation->discrete[0].sigma_x= 0.0;
	rv->excitation->discrete[0].sigma_xp= 0.0;
	rv->excitation->discrete[0].sigma_y= 0.0;
	rv->excitation->discrete[0].sigma_yp= 0.0;
	rv->excitation->discrete[0].scale_parameter = 0.0;
	rv->excitation->discrete[0].distribution_type = XMI_DISCRETE_MONOCHROMATIC;

	//absorbers
	rv->absorbers = (struct xmi_absorbers *) malloc(sizeof(struct xmi_absorbers));
	rv->absorbers->n_exc_layers = 0;
	rv->absorbers->exc_layers = NULL;
	rv->absorbers->n_det_layers = 1;
	rv->absorbers->det_layers = malloc(sizeof(struct xmi_layer));
	rv->absorbers->det_layers[0].n_elements = 1;
	rv->absorbers->det_layers[0].Z = (int *) malloc(sizeof(int));
	rv->absorbers->det_layers[0].weight = (double *) malloc(sizeof(double));
	rv->absorbers->det_layers[0].Z[0] = 4;
	rv->absorbers->det_layers[0].weight[0] = 1.0;
	rv->absorbers->det_layers[0].density = 1.85;
	rv->absorbers->det_layers[0].thickness = 0.002;

	//detector
	rv->detector = (struct xmi_detector *) malloc(sizeof(struct xmi_detector));
	rv->detector->detector_type = XMI_DETECTOR_SILI;
	rv->detector->live_time = 1;
	rv->detector->pulse_width= 10E-6;
	rv->detector->gain = 20.0/1000.0;
	rv->detector->zero = 0.0;
	rv->detector->fano = 0.12;
	rv->detector->noise = 0.1;
	rv->detector->max_convolution_energy = 40.0;
	rv->detector->n_crystal_layers = 1;
	rv->detector->crystal_layers = malloc(sizeof(struct xmi_layer));
	rv->detector->crystal_layers[0].n_elements = 1;
	rv->detector->crystal_layers[0].Z = (int *) malloc(sizeof(int));
	rv->detector->crystal_layers[0].weight = (double *) malloc(sizeof(double));
	rv->detector->crystal_layers[0].Z[0] = 14;
	rv->detector->crystal_layers[0].weight[0] = 1.0;
	rv->detector->crystal_layers[0].density = 2.33;
	rv->detector->crystal_layers[0].thickness = 0.5;



	return rv;

}


void xmi_free_exc_absorbers(struct xmi_absorbers *A) {
	int i;

	if (A->n_exc_layers > 0) {
		for (i = 0 ; i < A->n_exc_layers ; i++) 
			xmi_free_layer(A->exc_layers+i);
		free(A->exc_layers);
	}
	A->n_exc_layers = 0;
	A->exc_layers = NULL;
}

void xmi_free_det_absorbers(struct xmi_absorbers *A) {
	int i;

	if (A->n_det_layers > 0) {
		for (i = 0 ; i < A->n_det_layers ; i++) 
			xmi_free_layer(A->det_layers+i);
		free(A->det_layers);
	}
	A->n_det_layers = 0;
	A->det_layers = NULL;
}

void xmi_free_absorbers(struct xmi_absorbers *A) {
	xmi_free_exc_absorbers(A);
	xmi_free_det_absorbers(A);

	free(A);
}

void xmi_copy_exc_absorbers(struct xmi_absorbers *A, struct xmi_absorbers *B) {
	int i;

	B->n_exc_layers = A->n_exc_layers;
	B->exc_layers = (struct xmi_layer *) xmi_memdup(A->exc_layers,(A->n_exc_layers)*sizeof(struct xmi_layer));
	for (i = 0 ; i < A->n_exc_layers ; i++) {
		B->exc_layers[i].Z = (int *) xmi_memdup(A->exc_layers[i].Z,(A->exc_layers[i].n_elements)*sizeof(int));
		B->exc_layers[i].weight = (double *) xmi_memdup(A->exc_layers[i].weight,(A->exc_layers[i].n_elements)*sizeof(double));
	}
}

void xmi_copy_det_absorbers(struct xmi_absorbers *A, struct xmi_absorbers *B) {
	int i;

	B->n_det_layers = A->n_det_layers;
	B->det_layers = (struct xmi_layer *) xmi_memdup(A->det_layers,(A->n_det_layers)*sizeof(struct xmi_layer));
	for (i = 0 ; i < A->n_det_layers ; i++) {
		B->det_layers[i].Z = (int *) xmi_memdup(A->det_layers[i].Z,(A->det_layers[i].n_elements)*sizeof(int));
		B->det_layers[i].weight = (double *) xmi_memdup(A->det_layers[i].weight,(A->det_layers[i].n_elements)*sizeof(double));
	}
}

void xmi_copy_absorbers(struct xmi_absorbers *A, struct xmi_absorbers **B) {
	//allocate space for B
	*B = (struct xmi_absorbers *) malloc(sizeof(struct xmi_absorbers));

	xmi_copy_exc_absorbers(A, *B);
	xmi_copy_det_absorbers(A, *B);
}

void xmi_copy_abs_or_crystal2composition(struct xmi_layer *layers, int n_layers, struct xmi_composition **composition) {
	int i;

	*composition = (struct xmi_composition *) malloc(sizeof(struct xmi_composition));
	(*composition)->n_layers = n_layers;
	if (n_layers > 0) {
		(*composition)->layers = (struct xmi_layer *) malloc(sizeof(struct xmi_layer)*n_layers); 
		for (i = 0 ; i < n_layers ; i++) 
			xmi_copy_layer2(layers+i,(*composition)->layers+i);
	}
	else 
		(*composition)->layers = NULL;

}

void xmi_copy_composition2abs_or_crystal(struct xmi_composition *composition, struct xmi_layer **layers, int *n_layers) {
	int i;
	
	*n_layers = composition->n_layers;

	if (*n_layers > 0) {
		*layers	= (struct xmi_layer *) malloc(sizeof(struct xmi_layer)**n_layers);
		for (i = 0 ; i < *n_layers ; i++) {
			xmi_copy_layer2(composition->layers+i, (*layers)+i);
		}
	}
	else
		*layers = NULL;

	return;
}

int xmi_validate_input(struct xmi_input *a) {
	int i,j;
	int rv = 0;
	double sum;



	//validate general
	if (a->general->n_photons_interval <= 0) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

	if (a->general->n_photons_line <= 0) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

	if (a->general->n_interactions_trajectory <= 0) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

	if (strlen(a->general->outputfile) == 0) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}

after_general:

	//composition
	if (a->composition->n_layers < 1) {
		rv |= XMI_CONFLICT_COMPOSITION;
		goto after_composition;
	}

	if (a->composition->reference_layer < 1 || a->composition->reference_layer > a->composition->n_layers) {
		rv |= XMI_CONFLICT_COMPOSITION;
		goto after_composition;
	}

	for (i = 0 ; i < a->composition->n_layers ; i++) {
		sum = 0.0;
		for (j = 0 ; j < a->composition->layers[i].n_elements ; j++) {
			if (a->composition->layers[i].Z[j] < 1 || a->composition->layers[i].Z[j] > 94) {
				rv |= XMI_CONFLICT_COMPOSITION;
				goto after_composition;
			}
			else if (a->composition->layers[i].weight[j] < 0.0 || a->composition->layers[i].weight[j] > 1.0) {
				rv |= XMI_CONFLICT_COMPOSITION;
				goto after_composition;
			}
			sum += a->composition->layers[i].weight[j];
		}
		if (sum <= 0.0) {
			rv |= XMI_CONFLICT_COMPOSITION;
			goto after_composition;
		}
		if (a->composition->layers[i].density <= 0.0) {
			rv |= XMI_CONFLICT_COMPOSITION;
			goto after_composition;
		}
		if (a->composition->layers[i].thickness <= 0.0) {
			rv |= XMI_CONFLICT_COMPOSITION;
			goto after_composition;
		}
	}

after_composition:

	//geometry
	if (a->geometry->d_sample_source <= 0.0) {
		rv |= XMI_CONFLICT_GEOMETRY;
		goto after_geometry;
	}
	if (a->geometry->area_detector <= 0.0) {
		rv |= XMI_CONFLICT_GEOMETRY;
		goto after_geometry;
	}
	if (a->geometry->collimator_height < 0.0) {
		rv |= XMI_CONFLICT_GEOMETRY;
		goto after_geometry;
	}
	if (a->geometry->collimator_diameter < 0.0) {
		rv |= XMI_CONFLICT_GEOMETRY;
		goto after_geometry;
	}

after_geometry:

	if (a->excitation->n_discrete == 0 && a->excitation->n_continuous < 2) {
		rv |= XMI_CONFLICT_EXCITATION;
		goto after_excitation;
	}
	else if (a->excitation->n_continuous == 1) {
		rv |= XMI_CONFLICT_EXCITATION;
		goto after_excitation;
	}
	for (i = 0 ; i < a->excitation->n_discrete ; i++) {
		if (a->excitation->discrete[i].energy <= 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->discrete[i].horizontal_intensity < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->discrete[i].vertical_intensity < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->discrete[i].vertical_intensity+a->excitation->discrete[i].horizontal_intensity <= 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->discrete[i].sigma_x < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->discrete[i].sigma_y < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->discrete[i].distribution_type < XMI_DISCRETE_MONOCHROMATIC || a->excitation->discrete[i].distribution_type > XMI_DISCRETE_LORENTZIAN) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->discrete[i].distribution_type != XMI_DISCRETE_MONOCHROMATIC && a->excitation->discrete[i].scale_parameter <= 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
	}
	for (i = 0 ; i < a->excitation->n_continuous ; i++) {
		if (a->excitation->continuous[i].energy < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->continuous[i].horizontal_intensity < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->continuous[i].vertical_intensity < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->continuous[i].vertical_intensity+a->excitation->continuous[i].horizontal_intensity < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->continuous[i].sigma_x < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->continuous[i].sigma_y < 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (i < a->excitation->n_continuous-1 && a->excitation->continuous[i].horizontal_intensity + a->excitation->continuous[i].vertical_intensity + a->excitation->continuous[i+1].horizontal_intensity + a->excitation->continuous[i+1].vertical_intensity == 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
	}

after_excitation:


	//absorbers
	for (i = 0 ; i < a->absorbers->n_exc_layers ; i++) {
		sum = 0.0;
		for (j = 0 ; j < a->absorbers->exc_layers[i].n_elements ; j++) {
			if (a->absorbers->exc_layers[i].Z[j] < 1 || a->absorbers->exc_layers[i].Z[j] > 94) {
				rv |= XMI_CONFLICT_ABSORBERS;
				goto after_absorbers;
			}
			else if (a->absorbers->exc_layers[i].weight[j] < 0.0 || a->absorbers->exc_layers[i].weight[j] > 1.0) {
				rv |= XMI_CONFLICT_ABSORBERS;
				goto after_absorbers;
			}
			sum += a->absorbers->exc_layers[i].weight[j];
		}
		if (sum <= 0.0) {
			rv |= XMI_CONFLICT_ABSORBERS;
			goto after_absorbers;
		}
		if (a->absorbers->exc_layers[i].density <= 0.0) {
			rv |= XMI_CONFLICT_ABSORBERS;
			goto after_absorbers;
		}
		if (a->absorbers->exc_layers[i].thickness <= 0.0) {
			rv |= XMI_CONFLICT_ABSORBERS;
			goto after_absorbers;
		}
	}

	for (i = 0 ; i < a->absorbers->n_det_layers ; i++) {
		sum = 0.0;
		for (j = 0 ; j < a->absorbers->det_layers[i].n_elements ; j++) {
			if (a->absorbers->det_layers[i].Z[j] < 1 || a->absorbers->det_layers[i].Z[j] > 94) {
				rv |= XMI_CONFLICT_ABSORBERS;
				goto after_absorbers;
			}
			else if (a->absorbers->det_layers[i].weight[j] < 0.0 || a->absorbers->det_layers[i].weight[j] > 1.0) {
				rv |= XMI_CONFLICT_ABSORBERS;
				goto after_absorbers;
			}
			sum += a->absorbers->det_layers[i].weight[j];
		}
		if (sum <= 0.0) {
			rv |= XMI_CONFLICT_ABSORBERS;
			goto after_absorbers;
		}
		if (a->absorbers->det_layers[i].density <= 0.0) {
			rv |= XMI_CONFLICT_ABSORBERS;
			goto after_absorbers;
		}
		if (a->absorbers->det_layers[i].thickness <= 0.0) {
			rv |= XMI_CONFLICT_ABSORBERS;
			goto after_absorbers;
		}
	}

after_absorbers:

	//crystal
	if (a->detector->live_time <= 0.0) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}
	if (a->detector->pulse_width <= 0.0) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}
	if (a->detector->gain <= 0.0) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}
	if (a->detector->fano <= 0.0) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}
	if (a->detector->noise <= 0.0) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}
	if (a->detector->max_convolution_energy <= 0.0) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}

	if (a->detector->n_crystal_layers < 1) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}

	for (i = 0 ; i < a->detector->n_crystal_layers ; i++) {
		sum = 0.0;
		for (j = 0 ; j < a->detector->crystal_layers[i].n_elements ; j++) {
			if (a->detector->crystal_layers[i].Z[j] < 1 || a->detector->crystal_layers[i].Z[j] > 94) {
				rv |= XMI_CONFLICT_DETECTOR;
				goto after_detector;
			}
			else if (a->detector->crystal_layers[i].weight[j] < 0.0 || a->detector->crystal_layers[i].weight[j] > 1.0) {
				rv |= XMI_CONFLICT_DETECTOR;
				goto after_detector;
			}
			sum += a->detector->crystal_layers[i].weight[j];
		}
		if (sum <= 0.0) {
			rv |= XMI_CONFLICT_DETECTOR;
			goto after_detector;
		}
		if (a->detector->crystal_layers[i].density <= 0.0) {
			rv |= XMI_CONFLICT_DETECTOR;
			goto after_detector;
		}
		if (a->detector->crystal_layers[i].thickness <= 0.0) {
			rv |= XMI_CONFLICT_DETECTOR;
			goto after_detector;
		}
	}

after_detector:

	return rv;
}

void xmi_print_layer(FILE *fPtr ,struct xmi_layer *layer, int n_layers) {

	int i,j;


	for (i = 0 ; i < n_layers ; i++) {
		fprintf(fPtr,"Layer %i\n", i);
		for (j = 0 ; j < layer[i].n_elements ; j++) {
			fprintf(fPtr, "Z: %i -> weight: %g\n",layer[i].Z[j],layer[i].weight[j]);
		}
		fprintf(fPtr, "density: %g\n",layer[i].density);
		fprintf(fPtr, "thickness: %g\n",layer[i].thickness);
	}
	return;
}

void xmi_print_input(FILE *fPtr, struct xmi_input *input) {
	int i, j;

	//general
	fprintf(fPtr, "general\n");
	fprintf(fPtr, "outputfile: %s\n",input->general->outputfile);
	fprintf(fPtr, "comments: %s\n",input->general->comments);
	fprintf(fPtr, "n_photons_interval: %li\n", input->general->n_photons_interval);
	fprintf(fPtr, "n_photons_line: %li\n", input->general->n_photons_line);
	fprintf(fPtr, "n_interactions_trajectory: %i\n", input->general->n_interactions_trajectory);
	fprintf(fPtr, "\n");

	//composition
	fprintf(fPtr, "composition\n");
	xmi_print_layer(fPtr, input->composition->layers, input->composition->n_layers);
	fprintf(fPtr, "reference_layer: %i\n",input->composition->reference_layer);
	fprintf(fPtr, "\n");

	//geometry
	fprintf(fPtr, "geometry\n");
	fprintf(fPtr, "d_sample_source: %g\n", input->geometry->d_sample_source);
	fprintf(fPtr, "n_sample_orientation: %g  %g  %g\n",input->geometry->n_sample_orientation[0],input->geometry->n_sample_orientation[1],input->geometry->n_sample_orientation[2]);
	fprintf(fPtr, "p_detector_window: %g  %g  %g\n",input->geometry->p_detector_window[0],input->geometry->p_detector_window[1],input->geometry->p_detector_window[2]);
	fprintf(fPtr, "n_detector_orientation: %g  %g  %g\n",input->geometry->n_detector_orientation[0],input->geometry->n_detector_orientation[1],input->geometry->n_detector_orientation[2]);
	fprintf(fPtr, "area_detector: %g\n",input->geometry->area_detector);
	fprintf(fPtr, "collimator_height: %g\n",input->geometry->collimator_height);
	fprintf(fPtr, "collimator_diameter: %g\n",input->geometry->collimator_diameter);
	fprintf(fPtr, "d_source_slit: %g\n",input->geometry->d_source_slit);
	fprintf(fPtr, "slit_size_x: %g\n",input->geometry->slit_size_x);
	fprintf(fPtr, "slit_size_y: %g\n",input->geometry->slit_size_y);
	fprintf(fPtr, "\n");

	//excitation
	fprintf(fPtr, "excitation\n");
	fprintf(fPtr, "discrete\n");
	for (i = 0 ; i < input->excitation->n_discrete ; i++) {
		fprintf(fPtr, "Energy %i: %g\n",i,input->excitation->discrete[i].energy);
		fprintf(fPtr, "Horizontal intensity: %g\n",input->excitation->discrete[i].horizontal_intensity);
		fprintf(fPtr, "Vertical intensity: %g\n",input->excitation->discrete[i].vertical_intensity);
		fprintf(fPtr, "sigma_x: %g\n",input->excitation->discrete[i].sigma_x);
		fprintf(fPtr, "sigma_xp: %g\n",input->excitation->discrete[i].sigma_xp);
		fprintf(fPtr, "sigma_y: %g\n",input->excitation->discrete[i].sigma_y);
		fprintf(fPtr, "sigma_yp: %g\n",input->excitation->discrete[i].sigma_yp);
		fprintf(fPtr, "distribution_type: %i\n",input->excitation->discrete[i].distribution_type);
		if (input->excitation->discrete[i].distribution_type != XMI_DISCRETE_MONOCHROMATIC)
			fprintf(fPtr, "scale_parameter: %g\n",input->excitation->discrete[i].scale_parameter);
	}

	fprintf(fPtr, "continuous\n");
	for (i = 0 ; i < input->excitation->n_continuous ; i++) {
		fprintf(fPtr, "Energy %i: %g\n",i,input->excitation->continuous[i].energy);
		fprintf(fPtr, "Horizontal intensity: %g\n",input->excitation->continuous[i].horizontal_intensity);
		fprintf(fPtr, "Vertical intensity: %g\n",input->excitation->continuous[i].vertical_intensity);
		fprintf(fPtr, "sigma_x: %g\n",input->excitation->continuous[i].sigma_x);
		fprintf(fPtr, "sigma_xp: %g\n",input->excitation->continuous[i].sigma_xp);
		fprintf(fPtr, "sigma_y: %g\n",input->excitation->continuous[i].sigma_y);
		fprintf(fPtr, "sigma_yp: %g\n",input->excitation->continuous[i].sigma_yp);
	}
	fprintf(fPtr, "\n");

	//absorbers
	fprintf(fPtr, "Beam absorbers\n");
	xmi_print_layer(fPtr, input->absorbers->exc_layers, input->absorbers->n_exc_layers);
	fprintf(fPtr, "Detector absorbers\n");
	xmi_print_layer(fPtr, input->absorbers->det_layers, input->absorbers->n_det_layers);
	fprintf(fPtr, "\n");

	//detector
	fprintf(fPtr, "Detector\n");
	fprintf(fPtr, "detectortype: %i\n",input->detector->detector_type);
	fprintf(fPtr, "gain: %g\n", input->detector->gain);
	fprintf(fPtr, "live_time: %g\n", input->detector->live_time);
	fprintf(fPtr, "pulse_width: %g\n", input->detector->pulse_width);
	fprintf(fPtr, "zero: %g\n", input->detector->zero);
	fprintf(fPtr, "fano: %g\n", input->detector->fano);
	fprintf(fPtr, "noise: %g\n", input->detector->noise);
	fprintf(fPtr, "max_convolution_energy: %g\n", input->detector->max_convolution_energy);
	fprintf(fPtr, "detector crystal\n");
	xmi_print_layer(fPtr, input->detector->crystal_layers, input->detector->n_crystal_layers);
	fprintf(fPtr, "\n");


	return;
} 

#define ARRAY2D_FORTRAN(array,i,j,Ni,Nj) (array[(Nj)*(i)+(j)])
#define ARRAY3D_FORTRAN(array,i,j,k,Ni,Nj,Nk) (array[(Nj)*(Nk)*(i-1)+(Nk)*(j-1)+(k-1)])
struct xmi_output* xmi_output_raw2struct(struct xmi_input *input, double *brute_history, double *var_red_history,double **channels_conv, double *channels_unconv, int nchannels, char *inputfile, int use_zero_interactions ) {

	struct xmi_output* output = malloc(sizeof(struct xmi_output));
	int i,j,k;

	//first the easy ones
	output->input = input;
	xmi_copy_input(input, &output->input);
	output->inputfile = strdup(inputfile);
	output->nchannels = nchannels;
	output->use_zero_interactions = use_zero_interactions;
	output->channels_conv = malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
	output->channels_unconv = malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
	output->ninteractions = input->general->n_interactions_trajectory;

	for (i = (use_zero_interactions ? 0 : 1) ; i <= input->general->n_interactions_trajectory ; i++) {
		output->channels_unconv[i] = malloc(sizeof(double)*nchannels);
		output->channels_conv[i] = malloc(sizeof(double)*nchannels);
		for (j = 0 ; j < nchannels ; j++) {
			output->channels_unconv[i][j] = ARRAY2D_FORTRAN(channels_unconv,i,j,input->general->n_interactions_trajectory+1,nchannels);
			output->channels_conv[i][j] = channels_conv[i][j];
		}
	}

	int *uniqZ = NULL;
	int nuniqZ = 1;
	int found;
	uniqZ = realloc(uniqZ, sizeof(int));
	uniqZ[0] = input->composition->layers[0].Z[0];
	for (i = 0 ; i < input->composition->n_layers ; i++) {
		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			found = 0;
			for (k = 0 ; k < nuniqZ ; k++) {
				if (uniqZ[k] == input->composition->layers[i].Z[j]) {
					found = 1;
					break;
				}
			}
			if (found == 0) {
				//enlarge uniqZ
				uniqZ = (int *) realloc(uniqZ, sizeof(int)*++nuniqZ);
				uniqZ[nuniqZ-1] = input->composition->layers[i].Z[j]; 
			}
		}
	}
	qsort(uniqZ, nuniqZ, sizeof(int),xmi_cmp_int);
	output->nbrute_force_history = 0;
	output->nvar_red_history = 0;
	output->brute_force_history = NULL;
	output->var_red_history = NULL;
	
	for (i = 0 ; i < nuniqZ ; i++) {
		//start by checking total number of counts for this element
		double counts_sum = 0.0;
		for (j = 1 ; j <= 383 ; j++) {
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				counts_sum += ARRAY3D_FORTRAN(brute_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory);
			}
		}	
		if (counts_sum == 0.0)
			continue;

		//so there are counts somewhere: open element
		output->brute_force_history = realloc(output->brute_force_history, sizeof(struct xmi_fluorescence_line_counts)*++output->nbrute_force_history);
		output->brute_force_history[output->nbrute_force_history-1].atomic_number = uniqZ[i];
		output->brute_force_history[output->nbrute_force_history-1].total_counts = counts_sum;
		output->brute_force_history[output->nbrute_force_history-1].n_lines = 0;
		output->brute_force_history[output->nbrute_force_history-1].lines = NULL;
		for (j = 1 ; j <= 383 ; j++) {
			counts_sum = 0.0;
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				counts_sum += ARRAY3D_FORTRAN(brute_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory);
			}	
			if (counts_sum == 0.0)
				continue;
			output->brute_force_history[output->nbrute_force_history-1].lines = realloc(output->brute_force_history[output->nbrute_force_history-1].lines, sizeof(struct xmi_fluorescence_line)*++output->brute_force_history[output->nbrute_force_history-1].n_lines);
			strcpy(output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].line_type, xmi_lines[j]);
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].energy = LineEnergy(uniqZ[i], -1*j);
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].total_counts = counts_sum;
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].n_interactions = 0;
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions = NULL;

			//interactions loop
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				if (ARRAY3D_FORTRAN(brute_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory) <= 0.0)
					continue;
				output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions = realloc(output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions, ++output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].n_interactions*sizeof(struct xmi_counts));
				output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions[output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].n_interactions-1].counts = ARRAY3D_FORTRAN(brute_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory);
				output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions[output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].n_interactions-1].interaction_number = k;
			}
		}
	}

	if (var_red_history == NULL)
		return output;

	for (i = 0 ; i < nuniqZ ; i++) {
		//start by checking total number of counts for this element
		double counts_sum = 0.0;
		for (j = 1 ; j <= 383 ; j++) {
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				counts_sum += ARRAY3D_FORTRAN(var_red_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory);
			}
		}	
		if (counts_sum == 0.0)
			continue;

		//so there are counts somewhere: open element
		output->var_red_history = realloc(output->var_red_history, sizeof(struct xmi_fluorescence_line_counts)*++output->nvar_red_history);
		output->var_red_history[output->nvar_red_history-1].atomic_number = uniqZ[i];
		output->var_red_history[output->nvar_red_history-1].total_counts = counts_sum;
		output->var_red_history[output->nvar_red_history-1].n_lines = 0;
		output->var_red_history[output->nvar_red_history-1].lines = NULL;
		for (j = 1 ; j <= 383 ; j++) {
			counts_sum = 0.0;
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				counts_sum += ARRAY3D_FORTRAN(var_red_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory);
			}	
			if (counts_sum == 0.0)
				continue;
			output->var_red_history[output->nvar_red_history-1].lines = realloc(output->var_red_history[output->nvar_red_history-1].lines, sizeof(struct xmi_fluorescence_line)*++output->var_red_history[output->nvar_red_history-1].n_lines);
			strcpy(output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].line_type, xmi_lines[j]);
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].energy = LineEnergy(uniqZ[i], -1*j);
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].total_counts = counts_sum;
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].n_interactions = 0;
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions = NULL;

			//interactions loop
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				if (ARRAY3D_FORTRAN(var_red_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory) <= 0.0)
					continue;
				output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions = realloc(output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions, ++output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].n_interactions*sizeof(struct xmi_counts));
				output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions[output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].n_interactions-1].counts = ARRAY3D_FORTRAN(var_red_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory);
				output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions[output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].n_interactions-1].interaction_number = k;
			}
		}
	}


	return output;
}

void xmi_free_fluorescence_line_counts(struct xmi_fluorescence_line_counts *history, int nhistory) {
	int i,j,k;

	if (history == NULL)
		return;

	for (i = 0 ; i < nhistory ; i++) {
		for (j = 0 ; j < history[i].n_lines ; j++) {
			free(history[i].lines[j].interactions);
		}
		free(history[i].lines);
	}
	free(history);
}

void xmi_free_output(struct xmi_output *output) {
	free(output->inputfile);
	int i;

	for (i = (output->use_zero_interactions ? 0 : 1) ; i <= output->input->general->n_interactions_trajectory ; i++) {
		free(output->channels_conv[i]);
		free(output->channels_unconv[i]);
	}
	free(output->channels_conv);
	free(output->channels_unconv);
	xmi_free_fluorescence_line_counts(output->brute_force_history, output->nbrute_force_history);
	xmi_free_fluorescence_line_counts(output->var_red_history, output->nvar_red_history);

	xmi_free_input(output->input);

	return;
}

void xmi_free_archive(struct xmi_archive *archive) {
	free(archive->xpath1);
	if (archive->xpath2 != NULL)
		free(archive->xpath2);
	int i,j;
	for (i = 0 ; i <= archive->nsteps1 ; i++) {
		for (j = 0 ; j <= archive->nsteps2 ; j++) {
			xmi_free_output(archive->output[i][j]);
		}
		free(archive->input[i]);
		free(archive->output[i]);
		free(archive->inputfiles[i]);
		free(archive->outputfiles[i]);
	}
	free(archive->input);
	free(archive->output);
	free(archive->inputfiles);
	free(archive->outputfiles);

	return;
}

struct xmi_archive* xmi_archive_raw2struct(struct xmi_output ***output, double start_value1, double end_value1, int nsteps1, char *xpath1, double start_value2, double end_value2, int nsteps2, char *xpath2) {
	struct xmi_archive *archive = malloc(sizeof(struct xmi_archive));	
	archive->start_value1 = start_value1;
	archive->end_value1 = end_value1;
	archive->nsteps1 = nsteps1;
	archive->xpath1 = strdup(xpath1);
	archive->start_value2 = start_value2;
	archive->end_value2= end_value2;
	archive->nsteps2 = nsteps2;
	if (xpath2)
		archive->xpath2 = strdup(xpath2);
	else
		archive->xpath2 = NULL;
	archive->output = malloc(sizeof(struct xmi_output **)*(nsteps1+1));
	archive->input = malloc(sizeof(struct xmi_input **)*(nsteps1+1));
	archive->inputfiles = malloc(sizeof(char **)*(nsteps1+1));
	archive->outputfiles = malloc(sizeof(char **)*(nsteps1+1));
	int i;
	for (i = 0 ; i <= nsteps1 ; i++) {
		archive->output[i] = malloc(sizeof(struct xmi_output *)*(nsteps2+1));
		archive->input[i] = malloc(sizeof(struct xmi_input *)*(nsteps2+1));
		archive->inputfiles[i] = malloc(sizeof(char *)*(nsteps2+1));
		archive->outputfiles[i] = malloc(sizeof(char *)*(nsteps2+1));
	}

	int j;
	for (i = 0 ; i <= nsteps1 ; i++) {
		for (j = 0 ; j <= nsteps2 ; j++) {
			xmi_copy_output(output[i][j], &archive->output[i][j]);
			archive->input[i][j] = archive->output[i][j]->input;
			archive->inputfiles[i][j] = archive->output[i][j]->inputfile;
			archive->outputfiles[i][j] = archive->input[i][j]->general->outputfile;
		}
	}

	return archive;
}

void xmi_copy_output(struct xmi_output *A, struct xmi_output **B) {
	struct xmi_output *C = malloc(sizeof(struct xmi_output));
	C->inputfile = strdup(A->inputfile);
	xmi_copy_input(A->input, &C->input);
	C->nbrute_force_history = A->nbrute_force_history;
	C->nvar_red_history = A->nvar_red_history;
	C->nchannels = A->nchannels;
	C->ninteractions = A->ninteractions;
	C->use_zero_interactions = A->use_zero_interactions;
	int i, j;
	C->channels_conv = malloc(sizeof(double *) * (C->ninteractions+1));
	C->channels_unconv = malloc(sizeof(double *) * (C->ninteractions+1));
	for (i = 0 ; i <= C->ninteractions ; i++) {
		 C->channels_conv[i] = xmi_memdup(A->channels_conv[i], sizeof(double)*A->nchannels);
		 C->channels_unconv[i] = xmi_memdup(A->channels_unconv[i], sizeof(double)*A->nchannels);
	}

	C->brute_force_history = xmi_memdup(A->brute_force_history, sizeof(struct xmi_fluorescence_line_counts ) * C->nbrute_force_history);

	for (i = 0 ; i < C->nbrute_force_history ; i++) {
		C->brute_force_history[i].lines = xmi_memdup(A->brute_force_history[i].lines, sizeof(struct xmi_fluorescence_line)*C->brute_force_history[i].n_lines);
		for (j = 0 ; j < C->brute_force_history[i].n_lines ; j++)
			C->brute_force_history[i].lines[j].interactions = xmi_memdup(C->brute_force_history[i].lines[j].interactions, sizeof(struct xmi_counts)*C->brute_force_history[i].lines[j].n_interactions);
	}

	C->var_red_history = xmi_memdup(A->var_red_history, sizeof(struct xmi_fluorescence_line_counts ) * C->nvar_red_history);

	for (i = 0 ; i < C->nvar_red_history ; i++) {
		C->var_red_history[i].lines = xmi_memdup(A->var_red_history[i].lines, sizeof(struct xmi_fluorescence_line)*C->var_red_history[i].n_lines);
		for (j = 0 ; j < C->var_red_history[i].n_lines ; j++)
			C->var_red_history[i].lines[j].interactions = xmi_memdup(C->var_red_history[i].lines[j].interactions, sizeof(struct xmi_counts)*C->var_red_history[i].lines[j].n_interactions);
	}



	*B = C;
	return;
}

void xmi_copy_geometry(struct xmi_geometry *A, struct xmi_geometry **B) {
	//allocate space for B
	*B = (struct xmi_geometry *) xmi_memdup(A,sizeof(struct xmi_geometry));

	return;
} 

void xmi_copy_excitation(struct xmi_excitation *A, struct xmi_excitation **B) {
	*B = (struct xmi_excitation *) malloc(sizeof(struct xmi_excitation));
	(*B)->n_discrete = A->n_discrete;
	(*B)->n_continuous = A->n_continuous;

	if ((*B)->n_discrete > 0) {
		(*B)->discrete = (struct xmi_energy_discrete *) xmi_memdup(A->discrete,A->n_discrete*sizeof(struct xmi_energy_discrete));
	}
	else 
		(*B)->discrete = NULL;
	if ((*B)->n_continuous > 0) {
		(*B)->continuous = (struct xmi_energy_continuous *) xmi_memdup(A->continuous,A->n_continuous*sizeof(struct xmi_energy_continuous));

	}
	else 
		(*B)->continuous = NULL;

	return;
}

void xmi_copy_detector(struct xmi_detector *A, struct xmi_detector **B) {
	int i;

	*B= (struct xmi_detector *) xmi_memdup(A,sizeof(struct xmi_detector));
	(*B)->crystal_layers = (struct xmi_layer *) xmi_memdup(A->crystal_layers,(A->n_crystal_layers)*sizeof(struct xmi_layer));
	for (i = 0 ; i < A->n_crystal_layers ; i++) {
		(*B)->crystal_layers[i].Z = (int *) xmi_memdup(A->crystal_layers[i].Z,(A->crystal_layers[i].n_elements)*sizeof(int));
		(*B)->crystal_layers[i].weight = (double *) xmi_memdup(A->crystal_layers[i].weight,(A->crystal_layers[i].n_elements)*sizeof(double));
	}

	return;
}

void xmi_free_detector(struct xmi_detector *A) {
	int i;

	if (A->n_crystal_layers > 0) {
		for (i = 0 ; i < A->n_crystal_layers ; i++) 
			xmi_free_layer(A->crystal_layers+i);
		free(A->crystal_layers);
	}

	free(A);

	return;
}

void xmi_free_geometry(struct xmi_geometry *A) {
	free(A);

	return;
}

void xmi_free_excitation(struct xmi_excitation *A) {
	if (A->n_discrete > 0)
		free(A->discrete);

	if (A->n_continuous > 0)
		free(A->continuous);

	free(A);

	return;
}
