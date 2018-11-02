/*
Copyright (C) 2010-2018 Tom Schoonjans and Laszlo Vincze

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
#include <math.h>
#include <glib.h>
#include <string.h>
#include <stdlib.h>

void xmi_layer_free (xmi_layer *layer) {
	if (layer == NULL)
		return;
	g_free(layer->Z);
	g_free(layer->weight);
}

/**
 * xmi_layer_new: (constructor)
 * @n_elements: the number of elements the layer will contain.
 * @Z: (array length=n_elements) (transfer none): the atomic numbers of the elements
 * @weight: (array length=n_elements) (transfer none): the weights of the elements
 * @density: the density of the layer, expressed in g/cm3
 * @thickness: the thickness of the layer, expressed in cm
 * 
 * Allocates space for an #xmi_layer struct and populates it with the provided values
 *
 * Returns: (transfer full): the newly allocated #xmi_layer structure.
 */
xmi_layer* xmi_layer_new(int n_elements, int *Z, double *weight, double density, double thickness) {
	xmi_layer *rv = g_malloc0(sizeof(xmi_layer));
	rv->n_elements = n_elements;
	rv->Z = g_memdup(Z, sizeof(int) * n_elements);
	rv->weight = g_memdup(weight, sizeof(double) * n_elements);
	rv->density = density;
	rv->thickness = thickness;

	return rv;

}

void xmi_input_free(xmi_input *input) {
	if (input == NULL)
		return;
	//general
	xmi_general_free(input->general);

	//composition
	xmi_composition_free(input->composition);

	//geometry
	xmi_geometry_free(input->geometry);

	//excitation
	xmi_excitation_free(input->excitation);

	//absorbers
	xmi_absorbers_free(input->absorbers);

	//detector
	xmi_detector_free(input->detector);

	//input
	g_free(input);
}

/**
 * xmi_general_new: (constructor)
 * @outputfile: the name of the file that the simulation results will be written into (the XMSO-file...).
 * @n_photons_interval: the number of photons that will be simulated per interval of the continuous excitation spectrum
 * @n_photons_line: the number of photons that will be simulated per line in the discrete excitation spectrum
 * @n_interactions_trajectory: the maximum number of interactions that will be experienced by each simulated photon. A typical value is 4.
 * @comments: (nullable): text that may be added to the input-file that may help the reader to understand the purpose of the simulation.
 *
 * Returns: a freshly allocated xmi_general struct containing the provided argument values.
 */
xmi_general* xmi_general_new(const char *outputfile, long n_photons_interval, long n_photons_line, int n_interactions_trajectory, const char *comments) {
	xmi_general *rv = g_malloc0(sizeof(xmi_general));
	rv->version = g_ascii_strtod(VERSION, NULL);
	rv->outputfile = outputfile == NULL || strlen(outputfile) == 0 ? g_strdup("") : g_strdup(outputfile);
	rv->n_photons_interval = n_photons_interval;
	rv->n_photons_line = n_photons_line;
	rv->n_interactions_trajectory = n_interactions_trajectory;
	rv->comments = comments == NULL || strlen(comments) == 0 ? g_strdup("") : g_strdup(comments);
	return rv;
}

/**
 * xmi_general_copy:
 * @A: the original  #xmi_general struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_general_copy struct
 */
void xmi_general_copy(xmi_general *A, xmi_general **B) {
	if (A == NULL) {
		*B = NULL;
		return;
	}

	*B = g_memdup(A, sizeof(xmi_general));
	(*B)->outputfile = g_strdup(A->outputfile);
	(*B)->comments = g_strdup(A->comments);
}

/**
 * xmi_general_free:
 * @A: a #xmi_general struct
 *
 * Frees the resources allocated by xmi_general_new().
 */
void xmi_general_free(xmi_general *A) {
	if (A == NULL)
		return;

	g_free(A->outputfile);
	g_free(A->comments);
}

/**
 * xmi_input_copy:
 * @A: the original  #xmi_input struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_input_copy struct
 */
void xmi_input_copy(xmi_input *A, xmi_input **B) {
	//allocate space for B
	*B = (xmi_input *) g_malloc(sizeof(xmi_input));

	//general
	xmi_general_copy(A->general, &((*B)->general));

	//composition
	xmi_composition_copy(A->composition, &((*B)->composition));

	//geometry
	xmi_geometry_copy(A->geometry, &((*B)->geometry));

	//excitation
	xmi_excitation_copy(A->excitation, &((*B)->excitation));

	//absorbers
	xmi_absorbers_copy(A->absorbers, &((*B)->absorbers));

	//detector
	xmi_detector_copy(A->detector, &((*B)->detector));
}

#ifndef QUICKLOOK

int xmi_energy_discrete_equal(xmi_energy_discrete *a, xmi_energy_discrete *b) {
	if (fabs(a->energy - b->energy) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->horizontal_intensity - b->horizontal_intensity) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->vertical_intensity - b->vertical_intensity) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->sigma_x - b->sigma_x) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->sigma_xp - b->sigma_xp) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->sigma_y - b->sigma_y) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->sigma_yp - b->sigma_yp) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (a->distribution_type != b->distribution_type)
		return 0;
	if (a->distribution_type != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC && fabs(a->scale_parameter - b->scale_parameter) > XMI_COMPARE_THRESHOLD)
		return 0;

	return 1;
}

int xmi_energy_continuous_equal(xmi_energy_continuous *a, xmi_energy_continuous *b) {
	if (fabs(a->energy - b->energy) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->horizontal_intensity - b->horizontal_intensity) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->vertical_intensity - b->vertical_intensity) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->sigma_x - b->sigma_x) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->sigma_xp - b->sigma_xp) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->sigma_y - b->sigma_y) > XMI_COMPARE_THRESHOLD)
		return 0;
	if (fabs(a->sigma_yp - b->sigma_yp) > XMI_COMPARE_THRESHOLD)
		return 0;

	return 1;
}

int xmi_input_compare(xmi_input *A, xmi_input *B) {
	int rv;
	int i,j;
	double *temparr1;
	double *temparr2;

	//Yes, I know every textbook on programming says not to use the goto construct but I'm going to do it anyway :-)
	//Don't try this at home though!

	rv = 0;

	//general
	//let's ignore the version check for now shall we...
	/*if (A->general->version != B->general->version) {
		rv |= XMI_CONFLICT_GENERAL;
		goto after_general;
	}*/

	if (g_strcmp0(A->general->outputfile,B->general->outputfile) != 0) {
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

	if (g_strcmp0(A->general->comments,B->general->comments) != 0) {
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
					else if (fabs(A->composition->layers[i].weight[j]- B->composition->layers[i].weight[j])/A->composition->layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_COMPOSITION;
						goto after_composition;
					}
				}
				if (fabs(A->composition->layers[i].density - B->composition->layers[i].density)/A->composition->layers[i].density > XMI_COMPARE_THRESHOLD) {
					rv |= XMI_CONFLICT_COMPOSITION;
					break;
				}
				if (fabs(A->composition->layers[i].thickness- B->composition->layers[i].thickness)/A->composition->layers[i].thickness > XMI_COMPARE_THRESHOLD) {
					rv |= XMI_CONFLICT_COMPOSITION;
					break;
				}
			}
		}
	}

	after_composition:

	//geometry
#define XMI_IF_COMPARE_GEOMETRY(a) if (fabs(A->geometry->a - B->geometry->a)/fabs(A->geometry->a) > XMI_COMPARE_THRESHOLD){\
	rv |= XMI_CONFLICT_GEOMETRY;\
	goto after_geometry;\
	}
#define XMI_IF_COMPARE_GEOMETRY2(a) if (fabs(A->geometry->a - B->geometry->a) > XMI_COMPARE_THRESHOLD){\
	rv |= XMI_CONFLICT_GEOMETRY;\
	goto after_geometry;\
	}

#define XMI_IF_COMPARE_GEOMETRY3(a,b) if (fabs(a - b) > XMI_COMPARE_THRESHOLD){\
	rv |= XMI_CONFLICT_GEOMETRY;\
	goto after_geometry;\
	}

	XMI_IF_COMPARE_GEOMETRY(d_sample_source)
	temparr1 = (double *) g_memdup(A->geometry->n_sample_orientation,sizeof(double)*3);
	temparr2 = (double *) g_memdup(B->geometry->n_sample_orientation,sizeof(double)*3);
	xmi_normalize_vector_double(temparr1, 3);
	xmi_normalize_vector_double(temparr2, 3);

	XMI_IF_COMPARE_GEOMETRY3(temparr1[0],temparr2[0])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[1],temparr2[1])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[2],temparr2[2])
	g_free(temparr1);
	g_free(temparr2);

	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[0])
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[1])
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[2])

	temparr1 = (double *) g_memdup(A->geometry->n_detector_orientation,sizeof(double)*3);
	temparr2 = (double *) g_memdup(B->geometry->n_detector_orientation,sizeof(double)*3);
	xmi_normalize_vector_double(temparr1, 3);
	xmi_normalize_vector_double(temparr2, 3);

	XMI_IF_COMPARE_GEOMETRY3(temparr1[0],temparr2[0])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[1],temparr2[1])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[2],temparr2[2])
	g_free(temparr1);
	g_free(temparr2);

	XMI_IF_COMPARE_GEOMETRY(area_detector)
	XMI_IF_COMPARE_GEOMETRY2(collimator_height)
	XMI_IF_COMPARE_GEOMETRY2(collimator_diameter)
	XMI_IF_COMPARE_GEOMETRY2(d_source_slit)
	XMI_IF_COMPARE_GEOMETRY2(slit_size_x)
	XMI_IF_COMPARE_GEOMETRY2(slit_size_y)

	after_geometry:

#define XMI_IF_COMPARE_EXCITATION_DISCRETE(a) if (fabs(A->excitation->discrete[i].a-B->excitation->discrete[i].a)/A->excitation->discrete[i].a > XMI_COMPARE_THRESHOLD) {\
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
				else if (A->excitation->discrete[i].distribution_type != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC) {
					XMI_IF_COMPARE_EXCITATION_DISCRETE(scale_parameter)
				}
			}
		}
	}

#define XMI_IF_COMPARE_EXCITATION_CONTINUOUS(a) if (fabs(A->excitation->continuous[i].a-B->excitation->continuous[i].a)/A->excitation->continuous[i].a > XMI_COMPARE_THRESHOLD) {\
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
						else if (fabs(A->absorbers->exc_layers[i].weight[j]- B->absorbers->exc_layers[i].weight[j])/A->absorbers->exc_layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
							rv |= XMI_CONFLICT_ABSORBERS;
							goto after_absorbers;
						}
					}
					if (fabs(A->absorbers->exc_layers[i].density - B->absorbers->exc_layers[i].density)/A->absorbers->exc_layers[i].density > XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_ABSORBERS;
						break;
					}
					if (fabs(A->absorbers->exc_layers[i].thickness- B->absorbers->exc_layers[i].thickness)/A->absorbers->exc_layers[i].thickness > XMI_COMPARE_THRESHOLD) {
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
						else if (fabs(A->absorbers->det_layers[i].weight[j]- B->absorbers->det_layers[i].weight[j])/A->absorbers->det_layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
							rv |= XMI_CONFLICT_ABSORBERS;
							goto after_absorbers;
						}
					}
					if (fabs(A->absorbers->det_layers[i].density - B->absorbers->det_layers[i].density)/A->absorbers->det_layers[i].density > XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_ABSORBERS;
						break;
					}
					if (fabs(A->absorbers->det_layers[i].thickness- B->absorbers->det_layers[i].thickness)/A->absorbers->det_layers[i].thickness > XMI_COMPARE_THRESHOLD) {
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

#define XMI_IF_COMPARE_DETECTOR(a) if (fabs(A->detector->a - B->detector->a) > XMI_COMPARE_THRESHOLD){\
	rv |= XMI_CONFLICT_DETECTOR;\
	goto after_detector;\
	}

	XMI_IF_COMPARE_DETECTOR(live_time)
	XMI_IF_COMPARE_DETECTOR(pulse_width)
	XMI_IF_COMPARE_DETECTOR(gain)
	XMI_IF_COMPARE_DETECTOR(zero)
	XMI_IF_COMPARE_DETECTOR(fano)
	XMI_IF_COMPARE_DETECTOR(noise)

	if (A->detector->nchannels != B->detector->nchannels) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}

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
					else if (fabs(A->detector->crystal_layers[i].weight[j]- B->detector->crystal_layers[i].weight[j])/A->detector->crystal_layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
						rv |= XMI_CONFLICT_DETECTOR;
						goto after_detector;
					}
				}
				if (fabs(A->detector->crystal_layers[i].density - B->detector->crystal_layers[i].density)/A->detector->crystal_layers[i].density > XMI_COMPARE_THRESHOLD) {
					rv |= XMI_CONFLICT_DETECTOR;
					break;
				}
				if (fabs(A->detector->crystal_layers[i].thickness- B->detector->crystal_layers[i].thickness)/A->detector->crystal_layers[i].thickness > XMI_COMPARE_THRESHOLD) {
					rv |= XMI_CONFLICT_DETECTOR;
					break;
				}
			}
		}
	}



	after_detector:

	return rv;


}
#endif

/**
 * xmi_composition_new: (constructor)
 * @n_layers: number of layers in the composition (0 is allowed!)
 * @layers: (array length=n_layers) (transfer none) (nullable): array with the layers
 * @reference_layer: index of the layer that corresponds to the layer that will be used to establish the sample to source distance
 *
 * Allocates a new xmi_composition struct and populates it with the provided arguments
 */
xmi_composition* xmi_composition_new(int n_layers, xmi_layer *layers, int reference_layer) {
	if (n_layers < 0) {
		g_warning("xmi_composition_new: n_layers must be greater than or equal to zero");
		return NULL;
	}
	else if (n_layers > 0 && (reference_layer < 1 || reference_layer > n_layers)) {
		g_warning("xmi_composition_new: reference_layer must be at least one and less than or equal to n_layers");
		return NULL;
	}
	else if ((n_layers == 0 && layers != NULL) || (n_layers > 0 && layers == NULL)) {
		g_warning("xmi_composition_new: n_layers and layers inconsistency");
	}
	xmi_composition *rv = g_malloc0(sizeof(xmi_composition));
	rv->n_layers = n_layers;
	rv->reference_layer = reference_layer;
	if (n_layers > 0) {
		rv->layers = g_malloc0(sizeof(xmi_layer) * n_layers);
		int i;
		for (i = 0 ; i < n_layers ; i++) {
			xmi_layer_copy2(&layers[i], &rv->layers[i]);
		}
	}
	return rv;
}

/**
 * xmi_composition_get_layer:
 * @composition: #XmiMsimComposition instance
 * @index: index of the required layer
 *
 * Returns: (transfer full): a copy of the layer within this sample composition, or %NULL if not available
 */
xmi_layer* xmi_composition_get_layer(xmi_composition *composition, int index) {
	g_return_val_if_fail(composition != NULL, NULL);
	g_return_val_if_fail(composition->n_layers > 0, NULL);
	g_return_val_if_fail(index >= 0 && index < composition->n_layers, NULL);

	xmi_layer *rv;
	xmi_layer_copy(&composition->layers[index], &rv);

	return rv;
}

/**
 * xmi_composition_free:
 * @composition: a #xmi_composition struct
 *
 * Frees the resources allocated by xmi_composition_new().
 */
void xmi_composition_free(xmi_composition *composition) {
	if (composition == NULL)
		return;
	int i;

	for (i = 0 ; i < composition->n_layers ; i++)
		xmi_layer_free(composition->layers+i);

	g_free(composition->layers);
	g_free(composition);
}

/**
 * xmi_composition_copy:
 * @A: the original  #xmi_composition struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_composition struct
 */
void xmi_composition_copy(xmi_composition *A, xmi_composition **B) {
	if (A == NULL) {
		*B = NULL;
		return;
	}

	int i;

	//allocate space for B
	*B = (xmi_composition *) g_malloc(sizeof(xmi_composition));
	(*B)->n_layers = A->n_layers;
	(*B)->reference_layer = A->reference_layer;
	(*B)->layers = (xmi_layer *) g_memdup((A)->layers,((A)->n_layers)*sizeof(xmi_layer));
	for (i = 0 ; i < (A)->n_layers ; i++) {
		(*B)->layers[i].Z = (int *) g_memdup((A)->layers[i].Z,((A)->layers[i].n_elements)*sizeof(int));
		(*B)->layers[i].weight = (double *) g_memdup((A)->layers[i].weight,((A)->layers[i].n_elements)*sizeof(double));
	}
}

/**
 * xmi_layer_copy:
 * @A: the original  #xmi_layer struct
 * @B: (out):the destination to copy to
 *
 * Copies a #xmi_layer struct
 */
void xmi_layer_copy(xmi_layer *A, xmi_layer **B) {
	if (A == NULL) {
		*B = NULL;
		return;
	}
	//allocate space for B
	*B = (xmi_layer *) g_malloc(sizeof(xmi_layer));
	(*B)->n_elements = A->n_elements;
	(*B)->density = A->density;
	(*B)->thickness = A->thickness;
	(*B)->Z = (int *) g_memdup(A->Z, A->n_elements*sizeof(int));
	(*B)->weight = (double*) g_memdup(A->weight, A->n_elements*sizeof(double));
}

/**
 * xmi_layer_copy2: (skip)
 */
void xmi_layer_copy2(xmi_layer *A, xmi_layer *B) {
	B->n_elements = A->n_elements;
	B->density = A->density;
	B->thickness = A->thickness;
	B->Z = (int *) g_memdup(A->Z, A->n_elements*sizeof(int));
	B->weight = (double*) g_memdup(A->weight, A->n_elements*sizeof(double));
}

/**
 * xmi_input_init_empty: (constructor)
 *
 * Returns: an #XmiMsimInput instance, with default values, but empty outputfile and composition
 */
xmi_input *xmi_input_init_empty(void) {

	xmi_input *rv;

	rv = g_malloc(sizeof(xmi_input));

	//general
	rv->general = g_malloc(sizeof(xmi_general));
	rv->general->version = g_ascii_strtod(VERSION, NULL);
	rv->general->outputfile = g_strdup("");
	rv->general->comments= g_strdup("");
	rv->general->n_photons_interval = 10000;
	rv->general->n_photons_line = 100000;
	rv->general->n_interactions_trajectory = 4;

	//layer
	rv->composition = (xmi_composition *) g_malloc(sizeof(xmi_composition));
	rv->composition->n_layers = 0;
	rv->composition->layers = NULL;
	rv->composition->reference_layer = -1;

	//geometry
	rv->geometry = (xmi_geometry *) g_malloc(sizeof(xmi_geometry));
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
	rv->excitation = (xmi_excitation *) g_malloc(sizeof(xmi_excitation));
	rv->excitation->n_discrete = 1;
	rv->excitation->n_continuous = 0;
	rv->excitation->continuous = NULL;
	rv->excitation->discrete = (xmi_energy_discrete *) g_malloc(sizeof(xmi_energy_discrete));
	rv->excitation->discrete[0].energy = 28.0;
	rv->excitation->discrete[0].horizontal_intensity= 1E12;
	rv->excitation->discrete[0].vertical_intensity= 1E9;
	rv->excitation->discrete[0].sigma_x= 0.0;
	rv->excitation->discrete[0].sigma_xp= 0.0;
	rv->excitation->discrete[0].sigma_y= 0.0;
	rv->excitation->discrete[0].sigma_yp= 0.0;
	rv->excitation->discrete[0].scale_parameter = 0.0;
	rv->excitation->discrete[0].distribution_type = XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC;

	//absorbers
	rv->absorbers = (xmi_absorbers *) g_malloc(sizeof(xmi_absorbers));
	rv->absorbers->n_exc_layers = 0;
	rv->absorbers->exc_layers = NULL;
	rv->absorbers->n_det_layers = 1;
	rv->absorbers->det_layers = g_malloc(sizeof(xmi_layer));
	rv->absorbers->det_layers[0].n_elements = 1;
	rv->absorbers->det_layers[0].Z = (int *) g_malloc(sizeof(int));
	rv->absorbers->det_layers[0].weight = (double *) g_malloc(sizeof(double));
	rv->absorbers->det_layers[0].Z[0] = 4;
	rv->absorbers->det_layers[0].weight[0] = 1.0;
	rv->absorbers->det_layers[0].density = 1.85;
	rv->absorbers->det_layers[0].thickness = 0.002;

	//detector
	rv->detector = (xmi_detector *) g_malloc(sizeof(xmi_detector));
	rv->detector->detector_type = XMI_DETECTOR_SILI;
	rv->detector->live_time = 1;
	rv->detector->pulse_width= 10E-6;
	rv->detector->gain = 20.0/1000.0;
	rv->detector->zero = 0.0;
	rv->detector->fano = 0.12;
	rv->detector->noise = 0.1;
	rv->detector->nchannels = 2048;
	rv->detector->n_crystal_layers = 1;
	rv->detector->crystal_layers = g_malloc(sizeof(xmi_layer));
	rv->detector->crystal_layers[0].n_elements = 1;
	rv->detector->crystal_layers[0].Z = (int *) g_malloc(sizeof(int));
	rv->detector->crystal_layers[0].weight = (double *) g_malloc(sizeof(double));
	rv->detector->crystal_layers[0].Z[0] = 14;
	rv->detector->crystal_layers[0].weight[0] = 1.0;
	rv->detector->crystal_layers[0].density = 2.33;
	rv->detector->crystal_layers[0].thickness = 0.5;



	return rv;

}

void xmi_exc_absorbers_free(xmi_absorbers *A) {
	if (A == NULL)
		return;
	int i;

	if (A->n_exc_layers > 0) {
		for (i = 0 ; i < A->n_exc_layers ; i++)
			xmi_layer_free(A->exc_layers+i);
		g_free(A->exc_layers);
	}
	A->n_exc_layers = 0;
	A->exc_layers = NULL;
}

void xmi_det_absorbers_free(xmi_absorbers *A) {
	if (A == NULL)
		return;
	int i;

	if (A->n_det_layers > 0) {
		for (i = 0 ; i < A->n_det_layers ; i++)
			xmi_layer_free(A->det_layers+i);
		g_free(A->det_layers);
	}
	A->n_det_layers = 0;
	A->det_layers = NULL;
}

/**
 * xmi_absorbers_free:
 * @absorbers: a #xmi_absorbers struct
 *
 * Frees the resources allocated by xmi_absorbers_new().
 */
void xmi_absorbers_free(xmi_absorbers *absorbers) {
	if (absorbers == NULL)
		return;
	xmi_exc_absorbers_free(absorbers);
	xmi_det_absorbers_free(absorbers);

	g_free(absorbers);
}

void xmi_exc_absorbers_copy(xmi_absorbers *A, xmi_absorbers *B) {
	int i;

	B->n_exc_layers = A->n_exc_layers;
	B->exc_layers = (xmi_layer *) g_memdup(A->exc_layers,(A->n_exc_layers)*sizeof(xmi_layer));
	for (i = 0 ; i < A->n_exc_layers ; i++) {
		B->exc_layers[i].Z = (int *) g_memdup(A->exc_layers[i].Z,(A->exc_layers[i].n_elements)*sizeof(int));
		B->exc_layers[i].weight = (double *) g_memdup(A->exc_layers[i].weight,(A->exc_layers[i].n_elements)*sizeof(double));
	}
}

void xmi_det_absorbers_copy(xmi_absorbers *A, xmi_absorbers *B) {
	int i;

	B->n_det_layers = A->n_det_layers;
	B->det_layers = (xmi_layer *) g_memdup(A->det_layers,(A->n_det_layers)*sizeof(xmi_layer));
	for (i = 0 ; i < A->n_det_layers ; i++) {
		B->det_layers[i].Z = (int *) g_memdup(A->det_layers[i].Z,(A->det_layers[i].n_elements)*sizeof(int));
		B->det_layers[i].weight = (double *) g_memdup(A->det_layers[i].weight,(A->det_layers[i].n_elements)*sizeof(double));
	}
}

/**
 * xmi_absorbers_copy:
 * @A: the original  #xmi_absorbers struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_absorbers struct
 */
void xmi_absorbers_copy(xmi_absorbers *A, xmi_absorbers **B) {
	//allocate space for B
	*B = (xmi_absorbers *) g_malloc(sizeof(xmi_absorbers));

	xmi_exc_absorbers_copy(A, *B);
	xmi_det_absorbers_copy(A, *B);
}

/**
 * xmi_copy_abs_or_crystal2composition: (skip):
 */
void xmi_copy_abs_or_crystal2composition(xmi_layer *layers, int n_layers, xmi_composition **composition) {
	int i;

	*composition = (xmi_composition *) g_malloc(sizeof(xmi_composition));
	(*composition)->n_layers = n_layers;
	if (n_layers > 0) {
		(*composition)->layers = (xmi_layer *) g_malloc(sizeof(xmi_layer)*n_layers);
		for (i = 0 ; i < n_layers ; i++)
			xmi_layer_copy2(layers+i,(*composition)->layers+i);
	}
	else
		(*composition)->layers = NULL;

}

/**
 * xmi_copy_composition2abs_or_crystal: (skip):
 */
void xmi_copy_composition2abs_or_crystal(xmi_composition *composition, xmi_layer **layers, int *n_layers) {
	int i;

	if (composition == NULL) {
		*layers = NULL;
		*n_layers = 0;
		return;
	}

	*n_layers = composition->n_layers;

	if (*n_layers > 0) {
		*layers	= (xmi_layer *) g_malloc(sizeof(xmi_layer)**n_layers);
		for (i = 0 ; i < *n_layers ; i++) {
			xmi_layer_copy2(composition->layers+i, (*layers)+i);
		}
	}
	else
		*layers = NULL;

	return;
}

int xmi_input_validate(xmi_input *a) {
	unsigned int i,j;
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
		else if (a->excitation->discrete[i].distribution_type < XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC || a->excitation->discrete[i].distribution_type > XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
		else if (a->excitation->discrete[i].distribution_type != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC && a->excitation->discrete[i].scale_parameter <= 0.0) {
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
	}
	if (a->excitation->n_continuous == 2) {
		if (a->excitation->continuous[0].horizontal_intensity + a->excitation->continuous[0].vertical_intensity +
		    a->excitation->continuous[1].horizontal_intensity + a->excitation->continuous[1].vertical_intensity == 0.0) {
			rv |= XMI_CONFLICT_EXCITATION;
			goto after_excitation;
		}
	}
	else if (a->excitation->n_continuous > 2) {
		for (i = 1 ; i < a->excitation->n_continuous-1 ; i++) {
			int before = (a->excitation->continuous[i-1].horizontal_intensity + a->excitation->continuous[i-1].vertical_intensity) > 0.0;
			int current = (a->excitation->continuous[i].horizontal_intensity + a->excitation->continuous[i].vertical_intensity) > 0.0;
			int after = (a->excitation->continuous[i+1].horizontal_intensity + a->excitation->continuous[i+1].vertical_intensity) > 0.0;
			if (i == 1 && before + current == 0) {
				rv |= XMI_CONFLICT_EXCITATION;
				goto after_excitation;
			}
			else if (i == a->excitation->n_continuous - 2 && current + after == 0) {
				rv |= XMI_CONFLICT_EXCITATION;
				goto after_excitation;
			}
			else if (before + current + after == 0) {
				rv |= XMI_CONFLICT_EXCITATION;
				goto after_excitation;
			}
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

	if (a->detector->n_crystal_layers < 1) {
		rv |= XMI_CONFLICT_DETECTOR;
		goto after_detector;
	}

	if (a->detector->nchannels < 10) {
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

/**
 * xmi_layer_print: (skip):
 */
void xmi_layer_print(xmi_layer *layer, FILE *fPtr) {

	int i;

	for (i = 0 ; i < layer->n_elements ; i++) {
		fprintf(fPtr, "Z: %i -> weight: %g\n", layer->Z[i], layer->weight[i]);
	}
	fprintf(fPtr, "density: %g\n",layer->density);
	fprintf(fPtr, "thickness: %g\n",layer->thickness);
}

/**
 * xmi_input_print: (skip):
 */
void xmi_input_print(xmi_input *input, FILE *fPtr) {
	int i;

	//general
	fprintf(fPtr, "general\n");
	fprintf(fPtr, "outputfile: %s\n",input->general->outputfile);
	fprintf(fPtr, "comments: %s\n",input->general->comments);
	fprintf(fPtr, "n_photons_interval: %ld\n", input->general->n_photons_interval);
	fprintf(fPtr, "n_photons_line: %ld\n", input->general->n_photons_line);
	fprintf(fPtr, "n_interactions_trajectory: %d\n", input->general->n_interactions_trajectory);
	fprintf(fPtr, "\n");

	//composition
	fprintf(fPtr, "composition\n");
	for (i = 0 ; i < input->composition->n_layers ; i++) {
		fprintf(fPtr, "Layer %d\n", i);
		xmi_layer_print(&input->composition->layers[i], fPtr);
	}
	fprintf(fPtr, "reference_layer: %d\n",input->composition->reference_layer);
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
		if (input->excitation->discrete[i].distribution_type != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC)
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
	for (i = 0 ; i < input->absorbers->n_exc_layers ; i++) {
		fprintf(fPtr, "Layer %d\n", i);
		xmi_layer_print(&input->absorbers->exc_layers[i], fPtr);
	}
	fprintf(fPtr, "Detector absorbers\n");
	for (i = 0 ; i < input->absorbers->n_det_layers ; i++) {
		fprintf(fPtr, "Layer %d\n", i);
		xmi_layer_print(&input->absorbers->det_layers[i], fPtr);
	}
	fprintf(fPtr, "\n");

	//detector
	fprintf(fPtr, "Detector\n");
	fprintf(fPtr, "detectortype: %d\n",input->detector->detector_type);
	fprintf(fPtr, "gain: %g\n", input->detector->gain);
	fprintf(fPtr, "live_time: %g\n", input->detector->live_time);
	fprintf(fPtr, "pulse_width: %g\n", input->detector->pulse_width);
	fprintf(fPtr, "zero: %g\n", input->detector->zero);
	fprintf(fPtr, "fano: %g\n", input->detector->fano);
	fprintf(fPtr, "noise: %g\n", input->detector->noise);
	fprintf(fPtr, "nchannels: %i\n", input->detector->nchannels);
	fprintf(fPtr, "detector crystal\n");
	for (i = 0 ; i < input->detector->n_crystal_layers ; i++) {
		fprintf(fPtr, "Layer %d\n", i);
		xmi_layer_print(&input->detector->crystal_layers[i], fPtr);
	}
	fprintf(fPtr, "\n");
}

#define ARRAY2D_FORTRAN(array,i,j,Ni,Nj) (array[(Nj)*(i)+(j)])
#define ARRAY3D_FORTRAN(array,i,j,k,Ni,Nj,Nk) (array[(Nj)*(Nk)*(i-1)+(Nk)*(j-1)+(k-1)])
#ifndef QUICKLOOK
xmi_output* xmi_output_raw2struct(xmi_input *input, double *brute_history, double *var_red_history,double **channels_conv, double *channels_unconv, char *inputfile, int use_zero_interactions ) {

	xmi_output* output = g_malloc(sizeof(xmi_output));
	int i,j,k;
	int nchannels = input->detector->nchannels;

	//first the easy ones
	output->version = g_ascii_strtod(VERSION, NULL);
	output->input = input;
	xmi_input_copy(input, &output->input);
	output->inputfile = g_strdup(inputfile);
	output->outputfile = g_strdup(input->general->outputfile);
	output->use_zero_interactions = use_zero_interactions;
	output->channels_conv = g_malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
	output->channels_unconv = g_malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
	output->ninteractions = input->general->n_interactions_trajectory;

	for (i = 0 ; i <= input->general->n_interactions_trajectory ; i++) {
		output->channels_unconv[i] = g_malloc0(sizeof(double)*nchannels);
		output->channels_conv[i] = g_malloc0(sizeof(double)*nchannels);
		if (i == 0 && use_zero_interactions == 0)
			continue;
		for (j = 0 ; j < nchannels ; j++) {
			output->channels_unconv[i][j] = ARRAY2D_FORTRAN(channels_unconv,i,j,input->general->n_interactions_trajectory+1,nchannels);
			output->channels_conv[i][j] = channels_conv[i][j];
		}
	}

	int *uniqZ = NULL;
	int nuniqZ = 1;
	int found;
	uniqZ = g_realloc(uniqZ, sizeof(int));
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
				uniqZ = (int *) g_realloc(uniqZ, sizeof(int)*++nuniqZ);
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
		output->brute_force_history = g_realloc(output->brute_force_history, sizeof(xmi_fluorescence_line_counts)*++output->nbrute_force_history);
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
			output->brute_force_history[output->nbrute_force_history-1].lines = g_realloc(output->brute_force_history[output->nbrute_force_history-1].lines, sizeof(xmi_fluorescence_line)*++output->brute_force_history[output->nbrute_force_history-1].n_lines);
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].line_type = g_strdup(xmi_lines[j]);
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].energy = LineEnergy(uniqZ[i], -1*j);
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].total_counts = counts_sum;
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].n_interactions = 0;
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions = NULL;

			//interactions loop
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				if (ARRAY3D_FORTRAN(brute_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory) <= 0.0)
					continue;
				output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions = g_realloc(output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions, ++output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].n_interactions*sizeof(xmi_counts));
				output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions[output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].n_interactions-1].counts = ARRAY3D_FORTRAN(brute_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory);
				output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].interactions[output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].n_interactions-1].interaction_number = k;
			}
		}
	}

	if (var_red_history == NULL) {
		g_free(uniqZ);
		return output;
	}

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
		output->var_red_history = g_realloc(output->var_red_history, sizeof(xmi_fluorescence_line_counts)*++output->nvar_red_history);
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
			output->var_red_history[output->nvar_red_history-1].lines = g_realloc(output->var_red_history[output->nvar_red_history-1].lines, sizeof(xmi_fluorescence_line)*++output->var_red_history[output->nvar_red_history-1].n_lines);
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].line_type = g_strdup(xmi_lines[j]);
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].energy = LineEnergy(uniqZ[i], -1*j);
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].total_counts = counts_sum;
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].n_interactions = 0;
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions = NULL;

			//interactions loop
			for (k = 1 ; k <= input->general->n_interactions_trajectory ; k++) {
				if (ARRAY3D_FORTRAN(var_red_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory) <= 0.0)
					continue;
				output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions = g_realloc(output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions, ++output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].n_interactions*sizeof(xmi_counts));
				output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions[output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].n_interactions-1].counts = ARRAY3D_FORTRAN(var_red_history,uniqZ[i],j,k,100,385,input->general->n_interactions_trajectory);
				output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].interactions[output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].n_interactions-1].interaction_number = k;
			}
		}
	}


	g_free(uniqZ);
	return output;
}
#endif

void xmi_fluorescence_line_counts_free(xmi_fluorescence_line_counts *history, int nhistory) {
	int i,j;

	if (history == NULL)
		return;

	for (i = 0 ; i < nhistory ; i++) {
		for (j = 0 ; j < history[i].n_lines ; j++) {
			g_free(history[i].lines[j].interactions);
			g_free(history[i].lines[j].line_type);
		}
		g_free(history[i].lines);
	}
	g_free(history);
}

void xmi_output_free(xmi_output *output) {
	if (output == NULL)
		return;

	if (output->inputfile)
		g_free(output->inputfile);

	if (output->outputfile)
		g_free(output->outputfile);

	int i;

	for (i = 0 ; i <= output->input->general->n_interactions_trajectory ; i++) {
		g_free(output->channels_conv[i]);
		g_free(output->channels_unconv[i]);
	}
	g_free(output->channels_conv);
	g_free(output->channels_unconv);
	xmi_fluorescence_line_counts_free(output->brute_force_history, output->nbrute_force_history);
	xmi_fluorescence_line_counts_free(output->var_red_history, output->nvar_red_history);

	xmi_input_free(output->input);
	g_free(output);
}

void xmi_archive_copy(xmi_archive *A, xmi_archive **B) {
	xmi_archive *C = g_memdup(A, sizeof(xmi_archive));
	C->xpath1 = g_strdup(A->xpath1);
	C->xpath2 = g_strdup(A->xpath2);
	
	C->output = g_malloc(sizeof(xmi_output **) * (C->nsteps1 + 1));
	C->input = g_malloc(sizeof(xmi_input **) * (C->nsteps1 + 1));
	C->inputfiles = g_malloc(sizeof(char **) * (C->nsteps1 + 1));
	C->outputfiles = g_malloc(sizeof(char **) * (C->nsteps1 + 1));

	int i, j;

	for (i = 0 ; i <= C->nsteps1 ; i++) {
		C->output[i] = g_malloc(sizeof(xmi_output *) * (C->nsteps2 + 1));
		C->input[i] = g_malloc(sizeof(xmi_input *) * (C->nsteps2 + 1));
		C->inputfiles[i] = g_malloc(sizeof(char *) * (C->nsteps2 + 1));
		C->outputfiles[i] = g_malloc(sizeof(char *) * (C->nsteps2 + 1));
		for (j = 0 ; j <= C->nsteps2 ; j++) {
			xmi_output_copy(A->output[i][j], &C->output[i][j]);
			C->input[i][j] = C->output[i][j]->input;
			C->inputfiles[i][j] = C->output[i][j]->inputfile;
			C->outputfiles[i][j] = C->output[i][j]->outputfile;
		}
	}

	*B = C;
}

void xmi_archive_free(xmi_archive *archive) {
	if (archive == NULL)
		return;

	g_free(archive->xpath1);
	g_free(archive->xpath2);
	
	int i,j;
	for (i = 0 ; i <= archive->nsteps1 ; i++) {
		for (j = 0 ; j <= archive->nsteps2 ; j++) {
			xmi_output_free(archive->output[i][j]);
		}
		g_free(archive->input[i]);
		g_free(archive->output[i]);
		g_free(archive->inputfiles[i]);
		g_free(archive->outputfiles[i]);
	}
	g_free(archive->input);
	g_free(archive->output);
	g_free(archive->inputfiles);
	g_free(archive->outputfiles);
	g_free(archive);
}

xmi_archive* xmi_archive_raw2struct(xmi_output ***output, double start_value1, double end_value1, int nsteps1, char *xpath1, double start_value2, double end_value2, int nsteps2, char *xpath2) {
	xmi_archive *archive = g_malloc(sizeof(xmi_archive));
	archive->version = g_ascii_strtod(VERSION, NULL);
	archive->start_value1 = start_value1;
	archive->end_value1 = end_value1;
	archive->nsteps1 = nsteps1;
	archive->xpath1 = g_strdup(xpath1);
	archive->start_value2 = start_value2;
	archive->end_value2= end_value2;
	archive->nsteps2 = nsteps2;
	if (xpath2)
		archive->xpath2 = g_strdup(xpath2);
	else
		archive->xpath2 = NULL;
	archive->output = g_malloc(sizeof(xmi_output **)*(nsteps1+1));
	archive->input = g_malloc(sizeof(xmi_input **)*(nsteps1+1));
	archive->inputfiles = g_malloc(sizeof(char **)*(nsteps1+1));
	archive->outputfiles = g_malloc(sizeof(char **)*(nsteps1+1));

	int i, j;

	for (i = 0 ; i <= nsteps1 ; i++) {
		archive->output[i] = g_malloc(sizeof(xmi_output *)*(nsteps2+1));
		archive->input[i] = g_malloc(sizeof(xmi_input *)*(nsteps2+1));
		archive->inputfiles[i] = g_malloc(sizeof(char *)*(nsteps2+1));
		archive->outputfiles[i] = g_malloc(sizeof(char *)*(nsteps2+1));
		for (j = 0 ; j <= nsteps2 ; j++) {
			xmi_output_copy(output[i][j], &archive->output[i][j]);
			archive->input[i][j] = archive->output[i][j]->input;
			archive->inputfiles[i][j] = archive->output[i][j]->inputfile;
			archive->outputfiles[i][j] = archive->output[i][j]->outputfile;
		}
	}

	return archive;
}

void xmi_output_copy(xmi_output *A, xmi_output **B) {
	xmi_output *C = g_malloc(sizeof(xmi_output));
	C->version = A->version;
	C->inputfile = g_strdup(A->inputfile);
	C->outputfile = g_strdup(A->outputfile);
	xmi_input_copy(A->input, &C->input);
	C->nbrute_force_history = A->nbrute_force_history;
	C->nvar_red_history = A->nvar_red_history;
	C->ninteractions = A->ninteractions;
	C->use_zero_interactions = A->use_zero_interactions;
	int i, j;
	C->channels_conv = g_malloc(sizeof(double *) * (C->ninteractions+1));
	C->channels_unconv = g_malloc(sizeof(double *) * (C->ninteractions+1));
	for (i = 0 ; i <= C->ninteractions ; i++) {
		 C->channels_conv[i] = g_memdup(A->channels_conv[i], sizeof(double)*A->input->detector->nchannels);
		 C->channels_unconv[i] = g_memdup(A->channels_unconv[i], sizeof(double)*A->input->detector->nchannels);
	}

	C->brute_force_history = g_memdup(A->brute_force_history, sizeof(xmi_fluorescence_line_counts ) * C->nbrute_force_history);

	for (i = 0 ; i < C->nbrute_force_history ; i++) {
		C->brute_force_history[i].lines = g_memdup(A->brute_force_history[i].lines, sizeof(xmi_fluorescence_line)*C->brute_force_history[i].n_lines);
		for (j = 0 ; j < C->brute_force_history[i].n_lines ; j++) {
			C->brute_force_history[i].lines[j].interactions = g_memdup(C->brute_force_history[i].lines[j].interactions, sizeof(xmi_counts)*C->brute_force_history[i].lines[j].n_interactions);
			C->brute_force_history[i].lines[j].line_type = g_strdup(C->brute_force_history[i].lines[j].line_type);
		}
	}

	C->var_red_history = g_memdup(A->var_red_history, sizeof(xmi_fluorescence_line_counts ) * C->nvar_red_history);

	for (i = 0 ; i < C->nvar_red_history ; i++) {
		C->var_red_history[i].lines = g_memdup(A->var_red_history[i].lines, sizeof(xmi_fluorescence_line)*C->var_red_history[i].n_lines);
		for (j = 0 ; j < C->var_red_history[i].n_lines ; j++) {
			C->var_red_history[i].lines[j].interactions = g_memdup(C->var_red_history[i].lines[j].interactions, sizeof(xmi_counts)*C->var_red_history[i].lines[j].n_interactions);
			C->var_red_history[i].lines[j].line_type = g_strdup(C->var_red_history[i].lines[j].line_type);
		}
	}

	*B = C;
}

/**
 * xmi_geometry_new: (constructor)
 * @d_sample_source: the distance (in cm) between source and sample. The sample layer marked as reference_layer will be used for this calculation.
 * @n_sample_orientation: (array fixed-size=3): a three coordinate array describing the sample normal vector. The z-component has to be positive!
 * @p_detector_window: (array fixed-size=3): a three coordinate array describing the position (in cm) of the detector window.
 * @n_detector_orientation: (array fixed-size=3): a three coordinate array describing the normal vector of the detector window.
 * @area_detector: the active detector area (in cm2).
 * @collimator_height: the height of the collimator cone, measured from the detector window, in cm.
 * @collimator_diameter: the diameter of the collimator cone opening, in cm2
 * @d_source_slit: the distance between source and (virtual) slits, in cm.
 * @slit_size_x: the height of the slits, in cm.
 * @slit_size_y: the width of the slits, in cm.
 *
 * Returns: a freshly allocated xmi_geometry struct, populated with the arguments that were passed.
 */
xmi_geometry* xmi_geometry_new(double d_sample_source, double n_sample_orientation[3], double p_detector_window[3], double n_detector_orientation[3], double area_detector, double collimator_height, double collimator_diameter, double d_source_slit, double slit_size_x, double slit_size_y) {
	xmi_geometry *rv = g_malloc0(sizeof(xmi_geometry));
	rv->d_sample_source = d_sample_source;
	memcpy(rv->n_sample_orientation, n_sample_orientation, sizeof(double) * 3);
	memcpy(rv->p_detector_window, p_detector_window, sizeof(double) * 3);
	memcpy(rv->n_detector_orientation, n_detector_orientation, sizeof(double) * 3);
	rv->area_detector = area_detector;
	rv->collimator_height = collimator_height;
	rv->collimator_diameter = collimator_diameter;
	rv->d_source_slit= d_source_slit;
	rv->slit_size_x = slit_size_x;
	rv->slit_size_y = slit_size_y;
	return rv;
}

/**
 * xmi_geometry_copy:
 * @A: the original  #xmi_geometry struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_geometry struct
 */
void xmi_geometry_copy(xmi_geometry *A, xmi_geometry **B) {
	//allocate space for B
	*B = (xmi_geometry *) g_memdup(A, sizeof(xmi_geometry));
}

/**
 * xmi_excitation_new: (constructor):
 * @n_discrete: the number of discrete exciting X-ray lines in the exciting spectrum.
 * @discrete: (array length=n_discrete): an array containing the discrete components of the exciting spectrum.
 * @n_continuous: the number of sampling points within the continuous part of the exciting spectrum.
 * @continuous: (array length=n_continuous): an array containing the continuous components of the exciting spectrum.
 *
 * Returns: freshly allocated and initialized excitation struct.
 */
xmi_excitation* xmi_excitation_new(int n_discrete, xmi_energy_discrete *discrete, int n_continuous, xmi_energy_continuous *continuous) {
	xmi_excitation *rv = g_malloc0(sizeof(xmi_excitation));
	rv->n_discrete = n_discrete;
	rv->discrete = g_memdup(discrete, sizeof(xmi_energy_discrete) * n_discrete);
	rv->n_continuous = n_continuous;
	rv->continuous = g_memdup(continuous, sizeof(xmi_energy_continuous) * n_continuous);

	return rv;
}

/**
 * xmi_excitation_copy:
 * @A: the original  #xmi_excitation struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_geometry struct
 */
void xmi_excitation_copy(xmi_excitation *A, xmi_excitation **B) {
	if (A == NULL) {
		*B = NULL;
		return;
	}

	*B = (xmi_excitation *) g_malloc(sizeof(xmi_excitation));
	(*B)->n_discrete = A->n_discrete;
	(*B)->n_continuous = A->n_continuous;

	if ((*B)->n_discrete > 0) {
		(*B)->discrete = (xmi_energy_discrete *) g_memdup(A->discrete,A->n_discrete*sizeof(xmi_energy_discrete));
	}
	else
		(*B)->discrete = NULL;
	if ((*B)->n_continuous > 0) {
		(*B)->continuous = (xmi_energy_continuous *) g_memdup(A->continuous,A->n_continuous*sizeof(xmi_energy_continuous));

	}
	else
		(*B)->continuous = NULL;
}

void xmi_detector_copy(xmi_detector *A, xmi_detector **B) {
	int i;

	*B= (xmi_detector *) g_memdup(A,sizeof(xmi_detector));
	(*B)->crystal_layers = (xmi_layer *) g_memdup(A->crystal_layers,(A->n_crystal_layers)*sizeof(xmi_layer));
	for (i = 0 ; i < A->n_crystal_layers ; i++) {
		(*B)->crystal_layers[i].Z = (int *) g_memdup(A->crystal_layers[i].Z,(A->crystal_layers[i].n_elements)*sizeof(int));
		(*B)->crystal_layers[i].weight = (double *) g_memdup(A->crystal_layers[i].weight,(A->crystal_layers[i].n_elements)*sizeof(double));
	}
}

void xmi_detector_free(xmi_detector *A) {
	if (A == NULL)
		return;

	int i;

	if (A->n_crystal_layers > 0) {
		for (i = 0 ; i < A->n_crystal_layers ; i++)
			xmi_layer_free(A->crystal_layers+i);
		g_free(A->crystal_layers);
	}

	g_free(A);
}

/**
 * xmi_geometry_free:
 * @geometry: a #xmi_geometry struct
 *
 * Frees the resources allocated by xmi_geometry_new().
 */
void xmi_geometry_free(xmi_geometry *geometry) {
	if (geometry == NULL)
		return;
	g_free(geometry);
}

/**
 * xmi_excitation_free:
 * @excitation: a #xmi_excitation struct
 *
 * Frees the resources allocated by xmi_excitation_new().
 */
void xmi_excitation_free(xmi_excitation *excitation) {
	if (excitation == NULL)
		return;
	if (excitation->n_discrete > 0)
		g_free(excitation->discrete);

	if (excitation->n_continuous > 0)
		g_free(excitation->continuous);

	g_free(excitation);
}

/**
 * xmi_excitation_get_energy_discrete:
 * @excitation: #XmiMsimExcitation instance
 * @index: index of the required energy struct
 *
 * Returns: (transfer full): a copy of the energy_discrete struct within this excitation instance, or %NULL if not available
 */
xmi_energy_discrete* xmi_excitation_get_energy_discrete(xmi_excitation *excitation, int index) {
	g_return_val_if_fail(excitation != NULL, NULL);
	g_return_val_if_fail(excitation->n_discrete > 0, NULL);
	g_return_val_if_fail(index >= 0 && index < excitation->n_discrete, NULL);

	return g_memdup(&excitation->discrete[index], sizeof(xmi_energy_discrete));
}

/**
 * xmi_excitation_get_energy_continuous:
 * @excitation: #XmiMsimExcitation instance
 * @index: index of the required energy struct
 *
 * Returns: (transfer full): a copy of the energy_continuous struct within this excitation instance, or %NULL if not available
 */
xmi_energy_continuous* xmi_excitation_get_energy_continuous(xmi_excitation *excitation, int index) {
	g_return_val_if_fail(excitation != NULL, NULL);
	g_return_val_if_fail(excitation->n_continuous > 0, NULL);
	g_return_val_if_fail(index >= 0 && index < excitation->n_continuous, NULL);

	return g_memdup(&excitation->continuous[index], sizeof(xmi_energy_continuous));
}

/**
 * xmi_energy_discrete_new: (constructor):
 * @energy: The energy of the X-ray line (keV).
 * @horizontal_intensity: The horizontally polarized X-ray intensity (photons/sec)
 * @vertical_intensity: The vertically polarized X-ray intensity (photons/sec)
 * @sigma_x: If both sigma_x and sigma_y are non-zero, the photon starting position will be sampled from bi-Gaussian distribution based on these values. Otherwise a point source will be assumed.
 * @sigma_xp: If both sigma_xp and sigma_yp are non-zero, and the source is Gaussian, then the Source-slits distance takes on a new role as it becomes the distance between the actual focus and the source position. In this way a convergent beam can be defined, emitted by a Gaussian source at the origin. For the specific case of focusing on the sample the Sample-source distance should be set to the Source-slits distance.
 * @sigma_y: If both sigma_x and sigma_y are non-zero, the photon starting position will be sampled from bi-Gaussian distribution based on these values. Otherwise a point source will be assumed.
 * @sigma_yp: If both sigma_xp and sigma_yp are non-zero, and the source is Gaussian, then the Source-slits distance takes on a new role as it becomes the distance between the actual focus and the source position. In this way a convergent beam can be defined, emitted by a Gaussian source at the origin. For the specific case of focusing on the sample the Sample-source distance should be set to the Source-slits distance.
 * @distribution_type: The distribution type that can be assumed by the line.
 * @scale_parameter: If the distribution_type is not XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC, than this value will be used as scale parameter for sampling energies from the distribution.
 *
 * Returns: a freshly allocated and initialized #xmi_energy_discrete instance.
 */
xmi_energy_discrete* xmi_energy_discrete_new(double energy, double horizontal_intensity, double vertical_intensity, double sigma_x, double sigma_xp, double sigma_y, double sigma_yp, XmiEnergyDiscreteDistribution distribution_type, double scale_parameter) {
	xmi_energy_discrete *rv = g_malloc0(sizeof(xmi_energy_discrete));
	rv->energy = energy;
	rv->horizontal_intensity = horizontal_intensity;
	rv->vertical_intensity = vertical_intensity;
	rv->sigma_x = sigma_x;
	rv->sigma_y = sigma_y;
	rv->sigma_xp = sigma_xp;
	rv->sigma_yp = sigma_yp;
	rv->distribution_type = distribution_type;
	rv->scale_parameter = scale_parameter;

	return rv;
}

/**
 * xmi_energy_discrete_copy:
 * @A: the original  #xmi_energy_discrete struct
 * @B: (out):the destination to copy to
 *
 * Copies a #xmi_energy_discrete struct
 */
void xmi_energy_discrete_copy(xmi_energy_discrete *A, xmi_energy_discrete **B) {
	if (A == NULL) {
		*B = NULL;
		return;
	}
	*B = g_memdup(A, sizeof(xmi_energy_discrete));
}

/**
 * xmi_energy_discrete_free:
 * @A: a #xmi_energy_discrete struct
 *
 * Frees the resources allocated by xmi_energy_discrete_new().
 */
void xmi_energy_discrete_free(xmi_energy_discrete *A) {
	g_free(A);
}

/**
 * xmi_energy_continuous_new: (constructor):
 * @energy: The energy at this sampling point in the X-ray exciting continuum (keV).
 * @horizontal_intensity: The horizontally polarized X-ray intensity density (photons/sec/keV)
 * @vertical_intensity: The vertically polarized X-ray intensity density (photons/sec/keV)
 * @sigma_x: If both sigma_x and sigma_y are non-zero, the photon starting position will be sampled from bi-Gaussian distribution based on these values. Otherwise a point source will be assumed.
 * @sigma_xp: If both sigma_xp and sigma_yp are non-zero, and the source is Gaussian, then the Source-slits distance takes on a new role as it becomes the distance between the actual focus and the source position. In this way a convergent beam can be defined, emitted by a Gaussian source at the origin. For the specific case of focusing on the sample the Sample-source distance should be set to the Source-slits distance.
 * @sigma_y: If both sigma_x and sigma_y are non-zero, the photon starting position will be sampled from bi-Gaussian distribution based on these values. Otherwise a point source will be assumed.
 * @sigma_yp: If both sigma_xp and sigma_yp are non-zero, and the source is Gaussian, then the Source-slits distance takes on a new role as it becomes the distance between the actual focus and the source position. In this way a convergent beam can be defined, emitted by a Gaussian source at the origin. For the specific case of focusing on the sample the Sample-source distance should be set to the Source-slits distance.
 *
 * Returns: a freshly allocated and initialized #xmi_energy_continuous instance.
 */
xmi_energy_continuous* xmi_energy_continuous_new(double energy, double horizontal_intensity, double vertical_intensity, double sigma_x, double sigma_xp, double sigma_y, double sigma_yp) {
	xmi_energy_continuous *rv = g_malloc0(sizeof(xmi_energy_continuous));
	rv->energy = energy;
	rv->horizontal_intensity = horizontal_intensity;
	rv->vertical_intensity = vertical_intensity;
	rv->sigma_x = sigma_x;
	rv->sigma_y = sigma_y;
	rv->sigma_xp = sigma_xp;
	rv->sigma_yp = sigma_yp;

	return rv;
}

/**
 * xmi_energy_continuous_copy:
 * @A: the original  #xmi_energy_continuous struct
 * @B: (out):the destination to copy to
 *
 * Copies a #xmi_energy_continuous struct
 */
void xmi_energy_continuous_copy(xmi_energy_continuous *A, xmi_energy_continuous **B) {
	if (A == NULL) {
		*B = NULL;
		return;
	}
	*B = g_memdup(A, sizeof(xmi_energy_continuous));
}

/**
 * xmi_energy_continuous_free:
 * @A: a #xmi_energy_continuous struct
 *
 * Frees the resources allocated by xmi_energy_continuous_new().
 */
void xmi_energy_continuous_free(xmi_energy_continuous *A) {
	g_free(A);
}

#ifndef QUICKLOOK
int xmi_output_compare(xmi_output *A, xmi_output *B) {

	// lets say the version may differ since it became only recently meaningful
	// obviously also the inputfile may differ

	if (xmi_input_compare(A->input, B->input) != 0) {
		return 1;
	}

	if (A->ninteractions != B->ninteractions) {
		return 1;
	}

	if (A->use_zero_interactions != B->use_zero_interactions) {
		return 1;
	}

	int i,j,k;

	if (A->nbrute_force_history != B->nbrute_force_history) {
		return 1;
	}

	for (i = 0 ; i < A->nbrute_force_history ; i++) {
		if (A->brute_force_history[i].atomic_number !=
		    B->brute_force_history[i].atomic_number) {
			return 1;
		}

		if (A->brute_force_history[i].total_counts !=
		    B->brute_force_history[i].total_counts) {
			return 1;
		}

		if (A->brute_force_history[i].n_lines !=
		    B->brute_force_history[i].n_lines) {
			return 1;
		}

		for (j = 0 ; j < A->brute_force_history[i].n_lines ; j++) {
			if (g_strcmp0(A->brute_force_history[i].lines[j].line_type,
			           B->brute_force_history[i].lines[j].line_type) != 0) {
				return 1;
			}

			if (A->brute_force_history[i].lines[j].energy !=
			    B->brute_force_history[i].lines[j].energy) {
				return 1;
			}

			if (A->brute_force_history[i].lines[j].total_counts !=
			    B->brute_force_history[i].lines[j].total_counts) {
				return 1;
			}

			if (A->brute_force_history[i].lines[j].n_interactions !=
			    B->brute_force_history[i].lines[j].n_interactions) {
				return 1;
			}
			for (k = 0 ; k < A->brute_force_history[i].lines[j].n_interactions ; k++) {
				if (A->brute_force_history[i].lines[j].interactions[k].counts !=
				    B->brute_force_history[i].lines[j].interactions[k].counts){
				    	return 1;
				}

				if (A->brute_force_history[i].lines[j].interactions[k].interaction_number !=
				    B->brute_force_history[i].lines[j].interactions[k].interaction_number){
				    	return 1;
				}
			}
		}
	}

	if (A->nvar_red_history != B->nvar_red_history) {
		return 1;
	}

	for (i = 0 ; i < A->nvar_red_history ; i++) {
		if (A->var_red_history[i].atomic_number !=
		    B->var_red_history[i].atomic_number) {
			return 1;
		}

		if (A->var_red_history[i].total_counts !=
		    B->var_red_history[i].total_counts) {
			return 1;
		}

		if (A->var_red_history[i].n_lines !=
		    B->var_red_history[i].n_lines) {
			return 1;
		}

		for (j = 0 ; j < A->var_red_history[i].n_lines ; j++) {
			if (g_strcmp0(A->var_red_history[i].lines[j].line_type,
			           B->var_red_history[i].lines[j].line_type) != 0) {
				return 1;
			}

			if (A->var_red_history[i].lines[j].energy !=
			    B->var_red_history[i].lines[j].energy) {
				return 1;
			}

			if (A->var_red_history[i].lines[j].total_counts !=
			    B->var_red_history[i].lines[j].total_counts) {
				return 1;
			}

			if (A->var_red_history[i].lines[j].n_interactions !=
			    B->var_red_history[i].lines[j].n_interactions) {
				return 1;
			}
			for (k = 0 ; k < A->var_red_history[i].lines[j].n_interactions ; k++) {
				if (A->var_red_history[i].lines[j].interactions[k].counts !=
				    B->var_red_history[i].lines[j].interactions[k].counts){
				    	return 1;
				}

				if (A->var_red_history[i].lines[j].interactions[k].interaction_number !=
				    B->var_red_history[i].lines[j].interactions[k].interaction_number){
				    	return 1;
				}
			}
		}
	}

	for (i = (A->use_zero_interactions ? 0 : 1) ; i <= A->input->general->n_interactions_trajectory ; i++) {
		for (j = 0 ; j < A->input->detector->nchannels ; j++) {
			if (A->channels_conv[i][j] !=
			    B->channels_conv[i][j]) {
				return 1;
			}
			if (A->channels_unconv[i][j] !=
			    B->channels_unconv[i][j]) {
				return 1;
			}
		}
	}

	return 0;
}

int xmi_archive_compare(xmi_archive *A, xmi_archive *B) {

	if (A->start_value1 != B->start_value1) {
		return 1;
	}

	if (A->end_value1 != B->end_value1) {
		return 1;
	}

	if (g_strcmp0(A->xpath1, B->xpath1) != 0) {
		return 1;
	}

	if (A->nsteps1 != B->nsteps1) {
		return 1;
	}

	if (A->nsteps2 != B->nsteps2) {
		return 1;
	}

	if (A->xpath2 != NULL) {
		if (B->xpath2 == NULL) {
			return 1;
		}

		if (A->start_value2 != B->start_value2) {
			return 1;
		}

		if (A->end_value2 != B->end_value2) {
			return 1;
		}

		if (g_strcmp0(A->xpath2, B->xpath2) != 0) {
			return 1;
		}
	}
	else if (B->xpath2 != NULL) {
		return 1;
	}

	int i,j;

	for (i = 0 ; i <= A->nsteps1 ; i++) {
		for (j = 0 ; j <= A->nsteps2 ; j++) {
			if (xmi_output_compare(A->output[i][j],
			                       B->output[i][j]) != 0) {
				return 1;
			}
		}
	}

	return 0;
}

static double xmi_get_history_counts_for_element_line(xmi_fluorescence_line_counts *history, int nhistory, int Z, int line_macro) {
	int i, j;
	
	for (i = 0 ; i < nhistory ; i++) {
		xmi_fluorescence_line_counts counts = history[i];
		if (counts.atomic_number != Z)
			continue;
		for (j = 0 ; j < counts.n_lines ; j++) {
			xmi_fluorescence_line line = counts.lines[j];
			if (strcmp(line.line_type, xmi_lines[abs(line_macro)]) != 0)
				continue;
			return line.total_counts;
		}
	}

	return 0.0;
}
double xmi_output_get_counts_for_element_line(xmi_output *output, int Z, int line_macro) {
	// first try variance reduction, if not available use brute force
	if (output->nvar_red_history > 0) {
		return xmi_get_history_counts_for_element_line(output->var_red_history, output->nvar_red_history, Z, line_macro);
	} else if (output->nbrute_force_history > 0) {
		return xmi_get_history_counts_for_element_line(output->brute_force_history, output->nbrute_force_history, Z, line_macro);
	}
	return 0.0;
}

#endif
int xmi_cmp_int(const void *a, const void *b) {
	return *((int *) a) - *((int *) b);
}

static const xmi_main_options __default_main_options = {
        .use_M_lines = 1,
        .use_cascade_auger = 1,
        .use_cascade_radiative = 1,
        .use_variance_reduction = 1,
        .use_sum_peaks = 0,
        .use_escape_peaks = 1,
        .escape_ratios_mode = 0,
        .verbose = 0,
        .use_poisson = 0,
        .use_opencl = 1,
        .omp_num_threads = 1,
        .extra_verbose = 0,
        .custom_detector_response = NULL,
        .use_advanced_compton = 0,
        .use_default_seeds = 0
};

/**
 * xmi_main_options_new: (constructor)
 *
 * Allocates a new #xmi_main_options.
 *
 * The contents of the returned structure correspond to the defaults.
 *
 * Returns: (transfer full): the newly allocated #xmi_main_options structure.
 *   Use xmi_main_options_free() to free the resources allocated by this function
 */
xmi_main_options* xmi_main_options_new(void) {
	xmi_main_options *rv = g_memdup(&__default_main_options, sizeof(xmi_main_options));
        rv->omp_num_threads = xmi_omp_get_max_threads();

	return rv;
}

/**
 * xmi_main_options_free:
 * @options: a #xmi_main_options struct
 *
 * Frees the resources allocated by xmi_main_options_new().
 *
 */
void xmi_main_options_free(xmi_main_options *options) {
	if (!options)
		return;
	g_free(options->custom_detector_response);	
	g_free(options);
}

/**
 * xmi_main_options_copy:
 * @A: the original  #xmi_main_options struct
 * @B: (out):the destination to copy to
 *
 * Copies a #xmi_main_options struct
 */
void xmi_main_options_copy(xmi_main_options *A, xmi_main_options **B) {
	*B = g_memdup(A, sizeof(xmi_main_options));
	if (!*B)
		return;
	(*B)->custom_detector_response = g_strdup(A->custom_detector_response);
}
