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
#include "xmi_private.h"
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

/**
 * xmi_layer_equals:
 * @A: first xmi_layer struct to check for equality
 * @B: second xmi_layer struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_layer_equals(xmi_layer *A, xmi_layer *B) {
	g_return_val_if_fail(A != NULL && B != NULL, FALSE);

	int j;
	if (A->n_elements != B->n_elements) {
		return FALSE;
	}
	for (j = 0 ; j < A->n_elements ; j++) {
		if (A->Z[j] != B->Z[j]) {
			return FALSE;
		}
		if (fabs(A->weight[j] - B->weight[j])/A->weight[j] > XMI_COMPARE_THRESHOLD) {
			return FALSE;
		}
		if (fabs(A->density - B->density)/A->density > XMI_COMPARE_THRESHOLD) {
			return FALSE;
		}
		if (fabs(A->thickness- B->thickness)/A->thickness > XMI_COMPARE_THRESHOLD) {
			return FALSE;
		}
	}
	return TRUE;
}

/**
 * xmi_input_new: (constructor):
 * @general:
 * @composition:
 * @geometry:
 * @excitation:
 * @absorbers:
 * @detector:
 *
 * Returns: a newly allocated xmi_input struct, initialized with the provided arguments
 */
xmi_input *xmi_input_new(xmi_general *general, xmi_composition *composition, xmi_geometry *geometry, xmi_excitation *excitation, xmi_absorbers *absorbers, xmi_detector *detector) {

	xmi_input *rv = g_malloc0(sizeof(xmi_input));
	if (rv == NULL)
		return NULL; // this is highly unlikely!
	xmi_general_copy(general, &rv->general);
	xmi_composition_copy(composition, &rv->composition);
	xmi_geometry_copy(geometry, &rv->geometry);
	xmi_excitation_copy(excitation, &rv->excitation);
	xmi_absorbers_copy(absorbers, &rv->absorbers);
	xmi_detector_copy(detector, &rv->detector);
	
	return rv;
}

#define INPUT_SETTER(name) \
	void xmi_input_set_ ## name(xmi_input *input, xmi_ ## name *name) { \
		g_return_if_fail(input != NULL); \
		xmi_ ## name ## _free(input->name); \
		xmi_ ## name ## _copy(name, &input->name); \
	}

INPUT_SETTER(general)
INPUT_SETTER(composition)
INPUT_SETTER(geometry)
INPUT_SETTER(excitation)
INPUT_SETTER(absorbers)
INPUT_SETTER(detector)

/**
 * xmi_input_free:
 * @input: a #xmi_input struct
 *
 * Frees the resources allocated by xmi_input_new().
 */
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
	g_free(A);
}

/**
 * xmi_general_equals:
 * @A: first xmi_general struct to check for equality
 * @B: second xmi_general  struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_general_equals(xmi_general *A, xmi_general *B) {
	g_return_val_if_fail(A != NULL && B != NULL, FALSE);

	//let's ignore the version check for now shall we...
	/*if (A->general->version != B->general->version) {
	 	return FALSE;
	}*/
	if (g_strcmp0(A->outputfile, B->outputfile) != 0) {
		return FALSE;
	}

	if (A->n_photons_interval != B->n_photons_interval) {
		return FALSE;
	}

	if (A->n_photons_line != B->n_photons_line) {
		return FALSE;
	}

	if (A->n_interactions_trajectory != B->n_interactions_trajectory) {
		return FALSE;
	}

	if (g_strcmp0(A->comments, B->comments) != 0) {
		return FALSE;
	}
	return TRUE;
}


/**
 * xmi_input_copy:
 * @A: the original  #xmi_input struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_input struct
 */
void xmi_input_copy(xmi_input *A, xmi_input **B) {
	//allocate space for B
	*B = g_malloc0(sizeof(xmi_input));

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

/**
 * xmi_energy_discrete_equals:
 * @a: first xmi_energy_discrete struct to check for equality
 * @b: second xmi_energy_discrete struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_energy_discrete_equals(xmi_energy_discrete *a, xmi_energy_discrete *b) {
	if (fabs(a->energy - b->energy) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->horizontal_intensity - b->horizontal_intensity) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->vertical_intensity - b->vertical_intensity) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->sigma_x - b->sigma_x) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->sigma_xp - b->sigma_xp) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->sigma_y - b->sigma_y) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->sigma_yp - b->sigma_yp) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (a->distribution_type != b->distribution_type)
		return FALSE;
	if (a->distribution_type != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC && fabs(a->scale_parameter - b->scale_parameter) > XMI_COMPARE_THRESHOLD)
		return FALSE;

	return TRUE;
}

/**
 * xmi_energy_continuous_equals:
 * @a: first xmi_energy_continuous struct to check for equality
 * @b: second xmi_energy_continuous struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_energy_continuous_equals(xmi_energy_continuous *a, xmi_energy_continuous *b) {
	if (fabs(a->energy - b->energy) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->horizontal_intensity - b->horizontal_intensity) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->vertical_intensity - b->vertical_intensity) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->sigma_x - b->sigma_x) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->sigma_xp - b->sigma_xp) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->sigma_y - b->sigma_y) > XMI_COMPARE_THRESHOLD)
		return FALSE;
	if (fabs(a->sigma_yp - b->sigma_yp) > XMI_COMPARE_THRESHOLD)
		return FALSE;

	return TRUE;
}

#ifndef QUICKLOOK

/**
 * xmi_input_equals:
 * @A: first xmi_input struct to check for equality
 * @B: second xmi_input struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise. Use xmi_input_compare if more detailed information is required.
 */
gboolean xmi_input_equals(xmi_input *A, xmi_input *B) {
	return xmi_input_compare(A, B) == 0;
}

/**
 * xmi_input_compare:
 * @A: first xmi_input struct to compare
 * @B: second xmi_input struct to compare
 *
 * Returns: 0 if both are equal, an OR-ed XmiInputFlags otherwise
 */
XmiInputFlags xmi_input_compare(xmi_input *A, xmi_input *B) {
	g_return_val_if_fail(A != NULL && B != NULL, 0);
	int rv;
	int i,j;
	double *temparr1;
	double *temparr2;

	rv = 0;

	//general
	if (xmi_general_equals(A->general, B->general) == FALSE)
		rv |= XMI_INPUT_GENERAL;

	//composition
	if (xmi_composition_equals(A->composition, B->composition) == FALSE)
		rv |= XMI_INPUT_COMPOSITION;

	//geometry
	if (xmi_geometry_equals(A->geometry, B->geometry) == FALSE)
		rv |= XMI_INPUT_GEOMETRY;

	//excitation
	if (xmi_excitation_equals(A->excitation, B->excitation) == FALSE)
		rv |= XMI_INPUT_EXCITATION;

	//absorbers
	if (xmi_absorbers_equals(A->absorbers, B->absorbers) == FALSE)
		rv |= XMI_INPUT_ABSORBERS;

	//detector
	if (xmi_detector_equals(A->detector, B->detector) == FALSE)
		rv |= XMI_INPUT_DETECTOR;

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
	*B = g_malloc0(sizeof(xmi_composition));
	(*B)->n_layers = A->n_layers;
	(*B)->reference_layer = A->reference_layer;
	(*B)->layers = (xmi_layer *) g_memdup((A)->layers,((A)->n_layers)*sizeof(xmi_layer));
	for (i = 0 ; i < (A)->n_layers ; i++) {
		(*B)->layers[i].Z = (int *) g_memdup((A)->layers[i].Z,((A)->layers[i].n_elements)*sizeof(int));
		(*B)->layers[i].weight = (double *) g_memdup((A)->layers[i].weight,((A)->layers[i].n_elements)*sizeof(double));
	}
}

/**
 * xmi_composition_equals:
 * @A: first xmi_composition struct to check for equality
 * @B: second xmi_composition struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_composition_equals(xmi_composition *A, xmi_composition *B) {
	g_return_val_if_fail(A != NULL && B != NULL, FALSE);

	int j;
	if (A->n_layers != B->n_layers) {
		return FALSE;
	}
	if (A->reference_layer != B->reference_layer)
		return FALSE;
	for (j = 0 ; j < A->n_layers; j++) {
		if (xmi_layer_equals(&A->layers[j], &B->layers[j]) == FALSE)
			return FALSE;
	}
	return TRUE;
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
	*B = g_malloc0(sizeof(xmi_layer));
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

	rv = g_malloc0(sizeof(xmi_input));

	//general
	rv->general = g_malloc0(sizeof(xmi_general));
	rv->general->version = g_ascii_strtod(VERSION, NULL);
	rv->general->outputfile = g_strdup("");
	rv->general->comments= g_strdup("");
	rv->general->n_photons_interval = 10000;
	rv->general->n_photons_line = 100000;
	rv->general->n_interactions_trajectory = 4;

	//layer
	rv->composition = g_malloc0(sizeof(xmi_composition));
	rv->composition->n_layers = 0;
	rv->composition->layers = NULL;
	rv->composition->reference_layer = -1;

	//geometry
	rv->geometry = g_malloc0(sizeof(xmi_geometry));
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
	rv->excitation = g_malloc0(sizeof(xmi_excitation));
	rv->excitation->n_discrete = 1;
	rv->excitation->n_continuous = 0;
	rv->excitation->continuous = NULL;
	rv->excitation->discrete = g_malloc0(sizeof(xmi_energy_discrete));
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
	rv->absorbers = g_malloc0(sizeof(xmi_absorbers));
	rv->absorbers->n_exc_layers = 0;
	rv->absorbers->exc_layers = NULL;
	rv->absorbers->n_det_layers = 1;
	rv->absorbers->det_layers = g_malloc0(sizeof(xmi_layer));
	rv->absorbers->det_layers[0].n_elements = 1;
	rv->absorbers->det_layers[0].Z = g_malloc0(sizeof(int));
	rv->absorbers->det_layers[0].weight = g_malloc0(sizeof(double));
	rv->absorbers->det_layers[0].Z[0] = 4;
	rv->absorbers->det_layers[0].weight[0] = 1.0;
	rv->absorbers->det_layers[0].density = 1.85;
	rv->absorbers->det_layers[0].thickness = 0.002;

	//detector
	rv->detector = g_malloc0(sizeof(xmi_detector));
	rv->detector->detector_type = XMI_DETECTOR_CONVOLUTION_PROFILE_SILI;
	rv->detector->live_time = 1;
	rv->detector->pulse_width= 10E-6;
	rv->detector->gain = 20.0/1000.0;
	rv->detector->zero = 0.0;
	rv->detector->fano = 0.12;
	rv->detector->noise = 0.1;
	rv->detector->nchannels = 2048;
	rv->detector->n_crystal_layers = 1;
	rv->detector->crystal_layers = g_malloc0(sizeof(xmi_layer));
	rv->detector->crystal_layers[0].n_elements = 1;
	rv->detector->crystal_layers[0].Z = g_malloc0(sizeof(int));
	rv->detector->crystal_layers[0].weight = g_malloc0(sizeof(double));
	rv->detector->crystal_layers[0].Z[0] = 14;
	rv->detector->crystal_layers[0].weight[0] = 1.0;
	rv->detector->crystal_layers[0].density = 2.33;
	rv->detector->crystal_layers[0].thickness = 0.5;



	return rv;

}

/**
 * xmi_absorbers_new: (constructor):
 * @n_exc_layers: the number of absorbing layers in the excitation channel (between source and sample). 0 is allowed!
 * @exc_layers: (array length=n_exc_layers) (transfer none) (nullable): an array containing the excitation channel absorbing layers.
 * @n_det_layers: the number of absorbing layers in the detection channel (between sample and detector). 0 is allowed!
 * @det_layers: (array length=n_det_layers) (transfer none) (nullable): an array containing the detection channel absorbing layers.
 *
 * Allocates a new xmi_absorption struct and populates it with the provided arguments
 */
xmi_absorbers* xmi_absorbers_new(int n_exc_layers, xmi_layer *exc_layers, int n_det_layers, xmi_layer *det_layers) {
	if (n_exc_layers < 0) {
		g_warning("xmi_absorbers_new: n_exc_layers must be greater than or equal to zero");
		return NULL;
	}
	if (n_det_layers < 0) {
		g_warning("xmi_absorbers_new: n_det_layers must be greater than or equal to zero");
		return NULL;
	}
	if ((n_exc_layers == 0 && exc_layers != NULL) || (n_exc_layers > 0 && exc_layers == NULL)) {
		g_warning("xmi_absorbers_new: n_exc_layers and exc_layers inconsistency");
		return NULL;
	}
	if ((n_det_layers == 0 && det_layers != NULL) || (n_det_layers > 0 && det_layers == NULL)) {
		g_warning("xmi_absorbers_new: n_det_layers and det_layers inconsistency");
		return NULL;
	}

	xmi_absorbers *rv = g_malloc0(sizeof(xmi_absorbers));
	rv->n_exc_layers = n_exc_layers;
	if (n_exc_layers > 0) {
		rv->exc_layers = g_malloc0(sizeof(xmi_layer) * n_exc_layers);
		int i;
		for (i = 0 ; i < n_exc_layers ; i++) {
			xmi_layer_copy2(&exc_layers[i], &rv->exc_layers[i]);
		}
	}
	rv->n_det_layers = n_det_layers;
	if (n_det_layers > 0) {
		rv->det_layers = g_malloc0(sizeof(xmi_layer) * n_det_layers);
		int i;
		for (i = 0 ; i < n_det_layers ; i++) {
			xmi_layer_copy2(&det_layers[i], &rv->det_layers[i]);
		}
	}
	
	return rv;
}

/**
 * xmi_absorbers_get_exc_layer:
 * @absorbers: #XmiMsimAbsorbers instance
 * @index: index of the required layer
 *
 * Returns: (transfer full): a copy of the excitation channel layer, or %NULL if not available
 */
xmi_layer* xmi_absorbers_get_exc_layer(xmi_absorbers *absorbers, int index) {
	g_return_val_if_fail(absorbers != NULL, NULL);
	g_return_val_if_fail(absorbers->n_exc_layers > 0, NULL);
	g_return_val_if_fail(index >= 0 && index < absorbers->n_exc_layers, NULL);

	xmi_layer *rv;
	xmi_layer_copy(&absorbers->exc_layers[index], &rv);

	return rv;
}

/**
 * xmi_absorbers_get_det_layer:
 * @absorbers: #XmiMsimAbsorbers instance
 * @index: index of the required layer
 *
 * Returns: (transfer full): a copy of the detector channel layer, or %NULL if not available
 */
xmi_layer* xmi_absorbers_get_det_layer(xmi_absorbers *absorbers, int index) {
	g_return_val_if_fail(absorbers != NULL, NULL);
	g_return_val_if_fail(absorbers->n_det_layers > 0, NULL);
	g_return_val_if_fail(index >= 0 && index < absorbers->n_det_layers, NULL);

	xmi_layer *rv;
	xmi_layer_copy(&absorbers->det_layers[index], &rv);

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
	if (A == NULL) {
		*B = NULL;
		return;
	}

	//allocate space for B
	*B = g_malloc0(sizeof(xmi_absorbers));

	xmi_exc_absorbers_copy(A, *B);
	xmi_det_absorbers_copy(A, *B);
}

/**
 * xmi_absorbers_equals:
 * @A: first xmi_absorbers struct to check for equality
 * @B: second xmi_absorbers struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_absorbers_equals(xmi_absorbers *A, xmi_absorbers *B) {
	g_return_val_if_fail(A != NULL && B != NULL, FALSE);
	int i;

	if (A->n_exc_layers == 0 && B->n_exc_layers == 0 && A->n_det_layers == 0 && B->n_det_layers == 0) {
		return TRUE;
	}

	if (A->n_exc_layers != B->n_exc_layers) {
		return FALSE;
	}

	for (i = 0 ; i < A->n_exc_layers ; i++) {
		if (xmi_layer_equals(&A->exc_layers[i], &B->exc_layers[i]) == FALSE) {
			return FALSE;
		}
	}

	if (A->n_det_layers != B->n_det_layers) {
		return FALSE;
	}

	for (i = 0 ; i < A->n_det_layers ; i++) {
		if (xmi_layer_equals(&A->det_layers[i], &B->det_layers[i]) == FALSE) {
			return FALSE;
		}
	}

	return TRUE;
}

void xmi_copy_abs_or_crystal2composition(xmi_layer *layers, int n_layers, xmi_composition **composition) {
	int i;

	*composition = g_malloc0(sizeof(xmi_composition));
	(*composition)->n_layers = n_layers;
	if (n_layers > 0) {
		(*composition)->layers = g_malloc0(sizeof(xmi_layer)*n_layers);
		for (i = 0 ; i < n_layers ; i++)
			xmi_layer_copy2(layers+i,(*composition)->layers+i);
	}
	else
		(*composition)->layers = NULL;

}

void xmi_copy_composition2abs_or_crystal(xmi_composition *composition, xmi_layer **layers, int *n_layers) {
	int i;

	if (composition == NULL) {
		*layers = NULL;
		*n_layers = 0;
		return;
	}

	*n_layers = composition->n_layers;

	if (*n_layers > 0) {
		*layers	= g_malloc0(sizeof(xmi_layer)**n_layers);
		for (i = 0 ; i < *n_layers ; i++) {
			xmi_layer_copy2(composition->layers+i, (*layers)+i);
		}
	}
	else
		*layers = NULL;

	return;
}

/**
 * xmi_input_validate:
 * @input: xmi_input struct to validate.
 *
 * Returns: 0 if valid, an OR-ed XmiInputFlags otherwise
 */
XmiInputFlags xmi_input_validate(xmi_input *input) {
	g_return_val_if_fail(input != NULL, XMI_INPUT_GENERAL | XMI_INPUT_COMPOSITION | XMI_INPUT_GEOMETRY | XMI_INPUT_EXCITATION | XMI_INPUT_ABSORBERS | XMI_INPUT_DETECTOR);
	unsigned int i,j;
	int rv = 0;
	double sum;

	//validate general
	if (!input->general) {
		rv |= XMI_INPUT_GENERAL;
		goto after_general;
	}

	if (input->general->n_photons_interval <= 0) {
		rv |= XMI_INPUT_GENERAL;
		goto after_general;
	}

	if (input->general->n_photons_line <= 0) {
		rv |= XMI_INPUT_GENERAL;
		goto after_general;
	}

	if (input->general->n_interactions_trajectory <= 0) {
		rv |= XMI_INPUT_GENERAL;
		goto after_general;
	}

	if (strlen(input->general->outputfile) == 0) {
		rv |= XMI_INPUT_GENERAL;
		goto after_general;
	}

after_general:

	//composition
	if (!input->composition) {
		rv |= XMI_INPUT_COMPOSITION;
		goto after_composition;
	}

	if (input->composition->n_layers < 1) {
		rv |= XMI_INPUT_COMPOSITION;
		goto after_composition;
	}

	if (input->composition->reference_layer < 1 || input->composition->reference_layer > input->composition->n_layers) {
		rv |= XMI_INPUT_COMPOSITION;
		goto after_composition;
	}

	for (i = 0 ; i < input->composition->n_layers ; i++) {
		sum = 0.0;
		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			if (input->composition->layers[i].Z[j] < 1 || input->composition->layers[i].Z[j] > 94) {
				rv |= XMI_INPUT_COMPOSITION;
				goto after_composition;
			}
			else if (input->composition->layers[i].weight[j] < 0.0 || input->composition->layers[i].weight[j] > 1.0) {
				rv |= XMI_INPUT_COMPOSITION;
				goto after_composition;
			}
			sum += input->composition->layers[i].weight[j];
		}
		if (sum <= 0.0) {
			rv |= XMI_INPUT_COMPOSITION;
			goto after_composition;
		}
		if (input->composition->layers[i].density <= 0.0) {
			rv |= XMI_INPUT_COMPOSITION;
			goto after_composition;
		}
		if (input->composition->layers[i].thickness <= 0.0) {
			rv |= XMI_INPUT_COMPOSITION;
			goto after_composition;
		}
	}

after_composition:

	//geometry
	if (!input->geometry) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}


	if (input->geometry->n_sample_orientation[2] <= 0.0) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}
	if (input->geometry->d_sample_source <= 0.0) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}
	if (input->geometry->area_detector <= 0.0) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}
	if (input->geometry->collimator_height < 0.0) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}
	if (input->geometry->collimator_diameter < 0.0) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}
	if (input->geometry->d_source_slit <= 0.0) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}

	if (input->geometry->slit_size_x <= 0.0) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}

	if (input->geometry->slit_size_y <= 0.0) {
		rv |= XMI_INPUT_GEOMETRY;
		goto after_geometry;
	}

after_geometry:

	// excitation
	if (!input->excitation) {
		rv |= XMI_INPUT_EXCITATION;
		goto after_excitation;
	}

	if (input->excitation->n_discrete == 0 && input->excitation->n_continuous < 2) {
		rv |= XMI_INPUT_EXCITATION;
		goto after_excitation;
	}
	else if (input->excitation->n_continuous == 1) {
		rv |= XMI_INPUT_EXCITATION;
		goto after_excitation;
	}
	for (i = 0 ; i < input->excitation->n_discrete ; i++) {
		if (input->excitation->discrete[i].energy <= 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->discrete[i].horizontal_intensity < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->discrete[i].vertical_intensity < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->discrete[i].vertical_intensity+input->excitation->discrete[i].horizontal_intensity <= 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->discrete[i].sigma_x < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->discrete[i].sigma_y < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->discrete[i].distribution_type < XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC || input->excitation->discrete[i].distribution_type > XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->discrete[i].distribution_type != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC && input->excitation->discrete[i].scale_parameter <= 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
	}
	for (i = 0 ; i < input->excitation->n_continuous ; i++) {
		if (input->excitation->continuous[i].energy < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->continuous[i].horizontal_intensity < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->continuous[i].vertical_intensity < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->continuous[i].vertical_intensity+input->excitation->continuous[i].horizontal_intensity < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->continuous[i].sigma_x < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
		else if (input->excitation->continuous[i].sigma_y < 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
	}
	if (input->excitation->n_continuous == 2) {
		if (input->excitation->continuous[0].horizontal_intensity + input->excitation->continuous[0].vertical_intensity +
		    input->excitation->continuous[1].horizontal_intensity + input->excitation->continuous[1].vertical_intensity == 0.0) {
			rv |= XMI_INPUT_EXCITATION;
			goto after_excitation;
		}
	}
	else if (input->excitation->n_continuous > 2) {
		for (i = 1 ; i < input->excitation->n_continuous-1 ; i++) {
			int before = (input->excitation->continuous[i-1].horizontal_intensity + input->excitation->continuous[i-1].vertical_intensity) > 0.0;
			int current = (input->excitation->continuous[i].horizontal_intensity + input->excitation->continuous[i].vertical_intensity) > 0.0;
			int after = (input->excitation->continuous[i+1].horizontal_intensity + input->excitation->continuous[i+1].vertical_intensity) > 0.0;
			if (i == 1 && before + current == 0) {
				rv |= XMI_INPUT_EXCITATION;
				goto after_excitation;
			}
			else if (i == input->excitation->n_continuous - 2 && current + after == 0) {
				rv |= XMI_INPUT_EXCITATION;
				goto after_excitation;
			}
			else if (before + current + after == 0) {
				rv |= XMI_INPUT_EXCITATION;
				goto after_excitation;
			}
		}
	}

after_excitation:


	//absorbers
	if (!input->absorbers) {
		rv |= XMI_INPUT_ABSORBERS;
		goto after_absorbers;
	}

	for (i = 0 ; i < input->absorbers->n_exc_layers ; i++) {
		sum = 0.0;
		for (j = 0 ; j < input->absorbers->exc_layers[i].n_elements ; j++) {
			if (input->absorbers->exc_layers[i].Z[j] < 1 || input->absorbers->exc_layers[i].Z[j] > 94) {
				rv |= XMI_INPUT_ABSORBERS;
				goto after_absorbers;
			}
			else if (input->absorbers->exc_layers[i].weight[j] < 0.0 || input->absorbers->exc_layers[i].weight[j] > 1.0) {
				rv |= XMI_INPUT_ABSORBERS;
				goto after_absorbers;
			}
			sum += input->absorbers->exc_layers[i].weight[j];
		}
		if (sum <= 0.0) {
			rv |= XMI_INPUT_ABSORBERS;
			goto after_absorbers;
		}
		if (input->absorbers->exc_layers[i].density <= 0.0) {
			rv |= XMI_INPUT_ABSORBERS;
			goto after_absorbers;
		}
		if (input->absorbers->exc_layers[i].thickness <= 0.0) {
			rv |= XMI_INPUT_ABSORBERS;
			goto after_absorbers;
		}
	}

	for (i = 0 ; i < input->absorbers->n_det_layers ; i++) {
		sum = 0.0;
		for (j = 0 ; j < input->absorbers->det_layers[i].n_elements ; j++) {
			if (input->absorbers->det_layers[i].Z[j] < 1 || input->absorbers->det_layers[i].Z[j] > 94) {
				rv |= XMI_INPUT_ABSORBERS;
				goto after_absorbers;
			}
			else if (input->absorbers->det_layers[i].weight[j] < 0.0 || input->absorbers->det_layers[i].weight[j] > 1.0) {
				rv |= XMI_INPUT_ABSORBERS;
				goto after_absorbers;
			}
			sum += input->absorbers->det_layers[i].weight[j];
		}
		if (sum <= 0.0) {
			rv |= XMI_INPUT_ABSORBERS;
			goto after_absorbers;
		}
		if (input->absorbers->det_layers[i].density <= 0.0) {
			rv |= XMI_INPUT_ABSORBERS;
			goto after_absorbers;
		}
		if (input->absorbers->det_layers[i].thickness <= 0.0) {
			rv |= XMI_INPUT_ABSORBERS;
			goto after_absorbers;
		}
	}

after_absorbers:

	//crystal
	if (!input->detector) {
		rv |= XMI_INPUT_DETECTOR;
		goto after_detector;
	}

	if (input->detector->live_time <= 0.0) {
		rv |= XMI_INPUT_DETECTOR;
		goto after_detector;
	}
	if (input->detector->pulse_width <= 0.0) {
		rv |= XMI_INPUT_DETECTOR;
		goto after_detector;
	}
	if (input->detector->gain <= 0.0) {
		rv |= XMI_INPUT_DETECTOR;
		goto after_detector;
	}
	if (input->detector->fano <= 0.0) {
		rv |= XMI_INPUT_DETECTOR;
		goto after_detector;
	}
	if (input->detector->noise <= 0.0) {
		rv |= XMI_INPUT_DETECTOR;
		goto after_detector;
	}

	if (input->detector->n_crystal_layers < 1) {
		rv |= XMI_INPUT_DETECTOR;
		goto after_detector;
	}

	if (input->detector->nchannels < 10) {
		rv |= XMI_INPUT_DETECTOR;
		goto after_detector;
	}

	for (i = 0 ; i < input->detector->n_crystal_layers ; i++) {
		sum = 0.0;
		for (j = 0 ; j < input->detector->crystal_layers[i].n_elements ; j++) {
			if (input->detector->crystal_layers[i].Z[j] < 1 || input->detector->crystal_layers[i].Z[j] > 94) {
				rv |= XMI_INPUT_DETECTOR;
				goto after_detector;
			}
			else if (input->detector->crystal_layers[i].weight[j] < 0.0 || input->detector->crystal_layers[i].weight[j] > 1.0) {
				rv |= XMI_INPUT_DETECTOR;
				goto after_detector;
			}
			sum += input->detector->crystal_layers[i].weight[j];
		}
		if (sum <= 0.0) {
			rv |= XMI_INPUT_DETECTOR;
			goto after_detector;
		}
		if (input->detector->crystal_layers[i].density <= 0.0) {
			rv |= XMI_INPUT_DETECTOR;
			goto after_detector;
		}
		if (input->detector->crystal_layers[i].thickness <= 0.0) {
			rv |= XMI_INPUT_DETECTOR;
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

	xmi_output* output = g_malloc0(sizeof(xmi_output));
	int i,j,k;
	int nchannels = input->detector->nchannels;

	//first the easy ones
	output->version = g_ascii_strtod(VERSION, NULL);
	output->input = input;
	xmi_input_copy(input, &output->input);
	output->inputfile = g_strdup(inputfile);
	output->outputfile = g_strdup(input->general->outputfile);
	output->use_zero_interactions = use_zero_interactions;
	output->channels_conv = g_malloc0(sizeof(double *)*(input->general->n_interactions_trajectory+1));
	output->channels_unconv = g_malloc0(sizeof(double *)*(input->general->n_interactions_trajectory+1));
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
			output->brute_force_history[output->nbrute_force_history-1].lines[output->brute_force_history[output->nbrute_force_history-1].n_lines-1].energy = LineEnergy(uniqZ[i], -1*j, NULL);
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
			output->var_red_history[output->nvar_red_history-1].lines[output->var_red_history[output->nvar_red_history-1].n_lines-1].energy = LineEnergy(uniqZ[i], -1*j, NULL);
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

/**
 * xmi_output_free:
 * @output: a #xmi_output struct
 *
 * Frees an xmi_output struct and its contents
 */
void xmi_output_free(xmi_output *output) {
	if (output == NULL)
		return;

	if (output->inputfile)
		g_free(output->inputfile);

	if (output->outputfile)
		g_free(output->outputfile);

	int i;

	if (output->channels_conv) {
		for (i = 0 ; i <= output->input->general->n_interactions_trajectory ; i++) {
			g_free(output->channels_conv[i]);
		}
		g_free(output->channels_conv);
	}
	if (output->channels_unconv) {
		for (i = 0 ; i <= output->input->general->n_interactions_trajectory ; i++) {
			g_free(output->channels_unconv[i]);
		}
		g_free(output->channels_unconv);
	}

	xmi_fluorescence_line_counts_free(output->brute_force_history, output->nbrute_force_history);
	xmi_fluorescence_line_counts_free(output->var_red_history, output->nvar_red_history);

	xmi_input_free(output->input);
	g_free(output);
}

/**
 * xmi_archive_ref:
 * @A: a valid #xmi_archive struct
 *
 * Increases the reference count of A by 1
 *
 * Returns: the passed in #xmi_archive
 */
xmi_archive* xmi_archive_ref(xmi_archive *A) {
	g_return_val_if_fail(A != NULL, NULL);

	g_atomic_int_inc (&A->ref_count);

	return A;
}

/**
 * xmi_archive_unref:
 * @archive: a #xmi_archive struct
 *
 * Reduces the reference count of an xmi_archive struct by 1. If it hits zero, all memory will be released
 */
void xmi_archive_unref(xmi_archive *archive) {
	g_return_if_fail(archive != NULL);

	if (g_atomic_int_dec_and_test(&archive->ref_count)) {
		if (archive->single_data)
			g_ptr_array_unref(archive->single_data);

		if (archive->output)
			g_ptr_array_unref(archive->output);

		if (archive->dims)
			g_array_unref(archive->dims);

		g_free(archive);
	}
}

/**
 * xmi_archive_new: (constructor):
 * @single_data: (element-type XmiMsim.BatchSingleData):
 * @output: (element-type XmiMsim.Output):
 *
 * Returns: a newly allocated xmi_output struct, initialized with the provided arguments
 */
xmi_archive* xmi_archive_new(GPtrArray *single_data, GPtrArray *output) {
	g_return_val_if_fail(single_data != NULL, NULL);
	g_return_val_if_fail(output != NULL, NULL);

	xmi_archive *archive = g_malloc0(sizeof(xmi_archive));
	archive->version = g_ascii_strtod(VERSION, NULL);
	archive->ref_count = 1;
	archive->single_data = g_ptr_array_ref(single_data);
	archive->output = g_ptr_array_ref(output);

	GArray *dims = g_array_sized_new(FALSE, TRUE, sizeof(int), single_data->len);

	guint i;
	for (i = 0 ; i < single_data->len ; i++) {
		xmi_batch_single_data *data = g_ptr_array_index(single_data, i);
		int dim = data->nsteps + 1;
		g_array_append_val(dims, dim);
	}

	archive->dims = dims;

	return archive;
}

/**
 * xmi_output_copy:
 * @A: the original  #xmi_output struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_output struct
 */
void xmi_output_copy(xmi_output *A, xmi_output **B) {
	xmi_output *C = g_malloc0(sizeof(xmi_output));
	C->version = A->version;
	C->inputfile = g_strdup(A->inputfile);
	C->outputfile = g_strdup(A->outputfile);
	xmi_input_copy(A->input, &C->input);
	C->nbrute_force_history = A->nbrute_force_history;
	C->nvar_red_history = A->nvar_red_history;
	C->ninteractions = A->ninteractions;
	C->use_zero_interactions = A->use_zero_interactions;
	int i, j;
	C->channels_conv = g_malloc0(sizeof(double *) * (C->ninteractions+1));
	C->channels_unconv = g_malloc0(sizeof(double *) * (C->ninteractions+1));
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
 * xmi_output_get_spectrum_convoluted:
 * @output: an #xmi_output instance.
 * @after_interactions: specifies the requested spectrum by indicating how many interactions must have experienced.
 *
 * Returns: (element-type double) (transfer full): The requested convoluted spectrum in a GArray containing doubles.
 */
GArray* xmi_output_get_spectrum_convoluted(xmi_output *output, int after_interactions) {
	g_return_val_if_fail(output != NULL, NULL);
	g_return_val_if_fail(output->input != NULL, NULL);
	g_return_val_if_fail(output->input->detector != NULL, NULL);
	g_return_val_if_fail(after_interactions >= 0 && after_interactions <= output->ninteractions, NULL);

	GArray *rv = g_array_sized_new(FALSE, FALSE, sizeof(double), output->input->detector->nchannels);
	g_array_append_vals(rv, output->channels_conv[after_interactions], output->input->detector->nchannels);
	return rv;
}

/**
 * xmi_output_get_spectrum_unconvoluted:
 * @output: an #xmi_output instance.
 * @after_interactions: specifies the requested spectrum by indicating how many interactions must have experienced.
 *
 * Returns: (element-type double) (transfer full): The requested unconvoluted spectrum in a GArray containing doubles.
 */
GArray* xmi_output_get_spectrum_unconvoluted(xmi_output *output, int after_interactions) {
	g_return_val_if_fail(output != NULL, NULL);
	g_return_val_if_fail(output->input != NULL, NULL);
	g_return_val_if_fail(output->input->detector != NULL, NULL);
	g_return_val_if_fail(after_interactions >= 0 && after_interactions <= output->ninteractions, NULL);

	GArray *rv = g_array_sized_new(FALSE, FALSE, sizeof(double), output->input->detector->nchannels);
	g_array_append_vals(rv, output->channels_unconv[after_interactions], output->input->detector->nchannels);
	return rv;
}

static void get_history(GHashTable *table, xmi_fluorescence_line_counts *history, int nhistory, int ninteractions) {
	int i;

	for (i = 0 ; i < nhistory ; i++) {
		//int *Z = g_malloc(sizeof(int));
		xmi_history_element *element = g_malloc0(sizeof(xmi_history_element));
		element->ref_count = 1;
		//fprintf(stderr, "Za: %d\n", history[i].atomic_number);
		int Z = history[i].atomic_number;
		//fprintf(stderr, "Zb: %d\n", *Z);
		element->total_counts = history[i].total_counts;
		element->lines = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify) xmi_history_element_line_free);
		g_hash_table_insert(table, GINT_TO_POINTER(Z), element);
		int j;
		for (j = 0 ; j < history[i].n_lines ; j++) {
			gchar *line_type = g_strdup(history[i].lines[j].line_type);
			xmi_history_element_line *line = g_malloc0(sizeof(xmi_history_element_line));
			line->ref_count = 1;
			line->energy = history[i].lines[j].energy;
			line->total_counts = history[i].lines[j].total_counts;
			line->interactions = g_array_sized_new(FALSE, TRUE, sizeof(double), ninteractions + 1);
			g_hash_table_insert(element->lines, line_type, line);
			int k;
			for (k = 0 ; k <= ninteractions ; k++) {
				double nil = 0.0;
				g_array_append_val(line->interactions, nil);
			}
			for (k = 0 ; k < history[i].lines[j].n_interactions ; k++) {
				int interaction = history[i].lines[j].interactions[k].interaction_number;
				double counts = history[i].lines[j].interactions[k].counts;
				g_array_index(line->interactions, double, interaction) =  counts;
			}
		}
	}
}

void xmi_history_element_free(xmi_history_element *element) {
	g_return_if_fail(element != NULL);
	// until pygobject fixes support for GHashTables with boxed types as values, there should be no memory being freed here.
	return;
	if (g_atomic_int_dec_and_test(&element->ref_count)) {
		g_hash_table_unref(element->lines);	
		g_free(element);
	}
	/*xmi_history_element_line *line;
	GHashTableIter iter;
	g_hash_table_iter_init(&iter, element->lines);
	while (g_hash_table_iter_next(&iter, NULL, &line)) {
		g_hash_table_ref(line->interactions);
	}*/
}

void xmi_history_element_line_free(xmi_history_element_line *line) {
	g_return_if_fail(line != NULL);
	// until pygobject fixes support for GHashTables with boxed types as values, there should be no memory being freed here.
	return;
	if (g_atomic_int_dec_and_test(&line->ref_count)) {
		g_array_unref(line->interactions);	
		g_free(line);
	}
}

/**
 * xmi_output_get_history:
 * @output: an #xmi_output instance.
 * 
 * Returns: (element-type int XmiMsim.HistoryElement) (transfer full): a table with the simulation history an per element, per line and per interaction basis.
 */
GHashTable* xmi_output_get_history(xmi_output *output) {
	g_return_val_if_fail(output != NULL, NULL);

	GHashTable *rv = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, (GDestroyNotify) xmi_history_element_free);

	if (output->nbrute_force_history > 0 ) {
		get_history(rv, output->brute_force_history, output->nbrute_force_history, output->ninteractions);
	}
	else if (output->nvar_red_history > 0) {
		get_history(rv, output->var_red_history, output->nvar_red_history, output->ninteractions);
	
	}
	return rv;
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
	if (A == NULL) {
		*B = NULL;
		return;
	}
	//allocate space for B
	*B = g_memdup(A, sizeof(xmi_geometry));
}

/**
 * xmi_excitation_new: (constructor):
 * @n_discrete: the number of discrete exciting X-ray lines in the exciting spectrum.
 * @discrete: (array length=n_discrete) (nullable): an array containing the discrete components of the exciting spectrum.
 * @n_continuous: the number of sampling points within the continuous part of the exciting spectrum.
 * @continuous: (array length=n_continuous) (nullable): an array containing the continuous components of the exciting spectrum.
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

	*B = g_malloc0(sizeof(xmi_excitation));
	(*B)->n_discrete = A->n_discrete;
	(*B)->n_continuous = A->n_continuous;

	if ((*B)->n_discrete > 0) {
		(*B)->discrete = g_memdup(A->discrete, A->n_discrete * sizeof(xmi_energy_discrete));
	}
	else
		(*B)->discrete = NULL;
	if ((*B)->n_continuous > 0) {
		(*B)->continuous = g_memdup(A->continuous, A->n_continuous * sizeof(xmi_energy_continuous));

	}
	else
		(*B)->continuous = NULL;
}

/**
 * xmi_excitation_equals:
 * @A: first xmi_excitation struct to check for equality
 * @B: second xmi_excitation struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_excitation_equals(xmi_excitation *A, xmi_excitation *B) {
	g_return_val_if_fail(A != NULL && B != NULL, FALSE);
	int i;
	if (A->n_discrete == 0 && B->n_discrete == 0 && A->n_continuous == 0 && B->n_continuous == 0) {
		return TRUE;
	}
	if (A->n_discrete != B->n_discrete) {
		return FALSE;
	}
	if (A->n_continuous != B->n_continuous) {
		return FALSE;
	}
	if (A->n_discrete > 0) {
		for (i = 0 ; i < A->n_discrete ; i++) {
			if (xmi_energy_discrete_equals(&A->discrete[i], &B->discrete[i]) == FALSE)
				return FALSE;
		}
	}
	if (A->n_continuous > 0) {
		for (i = 0 ; i < A->n_continuous ; i++) {
			if (xmi_energy_continuous_equals(&A->continuous[i], &B->continuous[i]) == FALSE)
				return FALSE;
		}
	}
	return TRUE;
}

/**
 * xmi_detector_new: (constructor):
 * @detector_type: sets which builtin detector convolution profile should be used in the detector response function.
 * @live_time: the total amount of time that photons were detected. This is equal to the real time corrected with the dead time.
 * @pulse_width: the time that is necessary for one incoming photon to be processed by the detector. Will only be taken into account when the simulation is started with pile-up support.
 * @gain: the channels width (keV/channel)
 * @zero: the spectrum offset, AKA the energy corresponding to the first channel (keV)
 * @fano: the Fano factor for the detector material.
 * @noise: electronic noise (keV)
 * @nchannels: number of channels of the generated spectrum.
 * @n_crystal_layers: the number of layers in the channel. 0 is allowed!
 * @crystal_layers: (array length=n_crystal_layers) (transfer none) (nullable): an array containing the crystal layers.
 *
 * Allocates a new xmi_detector struct and populates it with the provided arguments
 */
xmi_detector* xmi_detector_new(XmiDetectorConvolutionProfile detector_type, double live_time, double pulse_width, double gain, double zero, double fano, double noise, int nchannels, int n_crystal_layers, xmi_layer *crystal_layers) {
	if (n_crystal_layers < 0) {
		g_warning("xmi_detector_new: n_crystal_layers must be greater than or equal to zero");
		return NULL;
	}
	if ((n_crystal_layers == 0 && crystal_layers != NULL) || (n_crystal_layers > 0 && crystal_layers == NULL)) {
		g_warning("xmi_detector_new: n_crystal_layers and crystal_layers inconsistency");
		return NULL;
	}
	xmi_detector *rv = g_malloc0(sizeof(xmi_detector));
	rv->detector_type = detector_type;
	rv->live_time = live_time;
	rv->pulse_width = pulse_width;
	rv->gain = gain;
	rv->zero = zero;
	rv->fano = fano;
	rv->noise = noise;
	rv->nchannels = nchannels;
	rv->n_crystal_layers = n_crystal_layers;
	if (n_crystal_layers > 0) {
		rv->crystal_layers = g_malloc0(sizeof(xmi_layer) * n_crystal_layers);
		int i;
		for (i = 0 ; i < n_crystal_layers ; i++) {
			xmi_layer_copy2(&crystal_layers[i], &rv->crystal_layers[i]);
		}
	}
	return rv;
}

/**
 * xmi_detector_get_crystal_layer:
 * @detector: #XmiMsimDetector instance
 * @index: index of the required layer
 *
 * Returns: (transfer full): a copy of the detector crystal layer, or %NULL if not available
 */
xmi_layer* xmi_detector_get_crystal_layer(xmi_detector *detector, int index) {
	g_return_val_if_fail(detector != NULL, NULL);
	g_return_val_if_fail(detector->n_crystal_layers > 0, NULL);
	g_return_val_if_fail(index >= 0 && index < detector->n_crystal_layers, NULL);

	xmi_layer *rv;
	xmi_layer_copy(&detector->crystal_layers[index], &rv);

	return rv;
}

/**
 * xmi_detector_copy:
 * @A: the original  #xmi_detector struct
 * @B: (out): the destination to copy to
 *
 * Copies a #xmi_detector struct
 */
void xmi_detector_copy(xmi_detector *A, xmi_detector **B) {
	if (A == NULL) {
		*B = NULL;
		return;
	}

	int i;

	*B = g_memdup(A, sizeof(xmi_detector));
	(*B)->crystal_layers = g_memdup(A->crystal_layers,(A->n_crystal_layers) * sizeof(xmi_layer));
	for (i = 0 ; i < A->n_crystal_layers ; i++) {
		(*B)->crystal_layers[i].Z = g_memdup(A->crystal_layers[i].Z, (A->crystal_layers[i].n_elements) * sizeof(int));
		(*B)->crystal_layers[i].weight = g_memdup(A->crystal_layers[i].weight, (A->crystal_layers[i].n_elements) * sizeof(double));
	}
}

/**
 * xmi_detector_free:
 * @detector: a #xmi_absorbers struct
 *
 * Frees the resources allocated by xmi_detector_new().
 */
void xmi_detector_free(xmi_detector *detector) {
	if (detector == NULL)
		return;

	int i;

	if (detector->n_crystal_layers > 0) {
		for (i = 0 ; i < detector->n_crystal_layers ; i++)
			xmi_layer_free(detector->crystal_layers+i);
		g_free(detector->crystal_layers);
	}

	g_free(detector);
}

/**
 * xmi_detector_equals:
 * @A: first xmi_detector struct to check for equality
 * @B: second xmi_detector struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_detector_equals(xmi_detector *A, xmi_detector *B) {
	g_return_val_if_fail(A != NULL && B != NULL, FALSE);
	int i;

	if (A->detector_type != B->detector_type) {
		return FALSE;
	}

#define XMI_IF_COMPARE_DETECTOR(a) if (fabs(A->a - B->a) > XMI_COMPARE_THRESHOLD){\
	return FALSE; \
	}

	XMI_IF_COMPARE_DETECTOR(live_time)
	XMI_IF_COMPARE_DETECTOR(pulse_width)
	XMI_IF_COMPARE_DETECTOR(gain)
	XMI_IF_COMPARE_DETECTOR(zero)
	XMI_IF_COMPARE_DETECTOR(fano)
	XMI_IF_COMPARE_DETECTOR(noise)

	if (A->nchannels != B->nchannels) {
		return FALSE;
	}

	if (A->n_crystal_layers != B->n_crystal_layers) {
		return FALSE;
	}

	for (i = 0 ; i < A->n_crystal_layers ; i++) {
		if (xmi_layer_equals(&A->crystal_layers[i], &B->crystal_layers[i]) == FALSE)
			return FALSE;
	}

	return TRUE;
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
/**
 * xmi_geometry_equals:
 * @A: first xmi_geometry struct to check for equality
 * @B: second xmi_geometry struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_geometry_equals(xmi_geometry *A, xmi_geometry *B) {
	g_return_val_if_fail(A != NULL && B != NULL, FALSE);

#define XMI_IF_COMPARE_GEOMETRY(a) if (fabs(A->a - B->a)/fabs(A->a) > XMI_COMPARE_THRESHOLD){\
		return FALSE; \
	}
#define XMI_IF_COMPARE_GEOMETRY2(a) if (fabs(A->a - B->a) > XMI_COMPARE_THRESHOLD){\
		return FALSE; \
	}

#define XMI_IF_COMPARE_GEOMETRY3(a,b) if (fabs(a - b) > XMI_COMPARE_THRESHOLD){\
		return FALSE; \
	}

	XMI_IF_COMPARE_GEOMETRY(d_sample_source)
	double *temparr1 = g_memdup(A->n_sample_orientation,sizeof(double) * 3);
	double *temparr2 = g_memdup(B->n_sample_orientation,sizeof(double) * 3);
	xmi_normalize_vector_double(temparr1, 3);
	xmi_normalize_vector_double(temparr2, 3);

	XMI_IF_COMPARE_GEOMETRY3(temparr1[0], temparr2[0])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[1], temparr2[1])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[2], temparr2[2])
	g_free(temparr1);
	g_free(temparr2);

	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[0])
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[1])
	XMI_IF_COMPARE_GEOMETRY2(p_detector_window[2])

	temparr1 = g_memdup(A->n_detector_orientation,sizeof(double) * 3);
	temparr2 = g_memdup(B->n_detector_orientation,sizeof(double) * 3);
	xmi_normalize_vector_double(temparr1, 3);
	xmi_normalize_vector_double(temparr2, 3);

	XMI_IF_COMPARE_GEOMETRY3(temparr1[0], temparr2[0])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[1], temparr2[1])
	XMI_IF_COMPARE_GEOMETRY3(temparr1[2], temparr2[2])
	g_free(temparr1);
	g_free(temparr2);

	XMI_IF_COMPARE_GEOMETRY(area_detector)
	XMI_IF_COMPARE_GEOMETRY2(collimator_height)
	XMI_IF_COMPARE_GEOMETRY2(collimator_diameter)
	XMI_IF_COMPARE_GEOMETRY2(d_source_slit)
	XMI_IF_COMPARE_GEOMETRY2(slit_size_x)
	XMI_IF_COMPARE_GEOMETRY2(slit_size_y)
	return TRUE;
}

/**
 * xmi_output_equals:
 * @A: first xmi_output struct to check for equality
 * @B: second xmi_output struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_output_equals(xmi_output *A, xmi_output *B) {

	// lets say the version may differ since it became only recently meaningful
	// obviously also the inputfile may differ

	if (xmi_input_compare(A->input, B->input) != 0) {
		return FALSE;
	}

	if (A->ninteractions != B->ninteractions) {
		return FALSE;
	}

	if (A->use_zero_interactions != B->use_zero_interactions) {
		return FALSE;
	}

	int i,j,k;

	if (A->nbrute_force_history != B->nbrute_force_history) {
		return FALSE;
	}

	for (i = 0 ; i < A->nbrute_force_history ; i++) {
		if (A->brute_force_history[i].atomic_number !=
		    B->brute_force_history[i].atomic_number) {
			return FALSE;
		}

		if (A->brute_force_history[i].total_counts !=
		    B->brute_force_history[i].total_counts) {
			return FALSE;
		}

		if (A->brute_force_history[i].n_lines !=
		    B->brute_force_history[i].n_lines) {
			return FALSE;
		}

		for (j = 0 ; j < A->brute_force_history[i].n_lines ; j++) {
			if (g_strcmp0(A->brute_force_history[i].lines[j].line_type,
			           B->brute_force_history[i].lines[j].line_type) != 0) {
				return FALSE;
			}

			if (A->brute_force_history[i].lines[j].energy !=
			    B->brute_force_history[i].lines[j].energy) {
				return FALSE;
			}

			if (A->brute_force_history[i].lines[j].total_counts !=
			    B->brute_force_history[i].lines[j].total_counts) {
				return FALSE;
			}

			if (A->brute_force_history[i].lines[j].n_interactions !=
			    B->brute_force_history[i].lines[j].n_interactions) {
				return FALSE;
			}
			for (k = 0 ; k < A->brute_force_history[i].lines[j].n_interactions ; k++) {
				if (A->brute_force_history[i].lines[j].interactions[k].counts !=
				    B->brute_force_history[i].lines[j].interactions[k].counts){
				    	return FALSE;
				}

				if (A->brute_force_history[i].lines[j].interactions[k].interaction_number !=
				    B->brute_force_history[i].lines[j].interactions[k].interaction_number){
				    	return FALSE;
				}
			}
		}
	}

	if (A->nvar_red_history != B->nvar_red_history) {
		return FALSE;
	}

	for (i = 0 ; i < A->nvar_red_history ; i++) {
		if (A->var_red_history[i].atomic_number !=
		    B->var_red_history[i].atomic_number) {
			return FALSE;
		}

		if (A->var_red_history[i].total_counts !=
		    B->var_red_history[i].total_counts) {
			return FALSE;
		}

		if (A->var_red_history[i].n_lines !=
		    B->var_red_history[i].n_lines) {
			return FALSE;
		}

		for (j = 0 ; j < A->var_red_history[i].n_lines ; j++) {
			if (g_strcmp0(A->var_red_history[i].lines[j].line_type,
			           B->var_red_history[i].lines[j].line_type) != 0) {
				return FALSE;
			}

			if (A->var_red_history[i].lines[j].energy !=
			    B->var_red_history[i].lines[j].energy) {
				return FALSE;
			}

			if (A->var_red_history[i].lines[j].total_counts !=
			    B->var_red_history[i].lines[j].total_counts) {
				return FALSE;
			}

			if (A->var_red_history[i].lines[j].n_interactions !=
			    B->var_red_history[i].lines[j].n_interactions) {
				return FALSE;
			}
			for (k = 0 ; k < A->var_red_history[i].lines[j].n_interactions ; k++) {
				if (A->var_red_history[i].lines[j].interactions[k].counts !=
				    B->var_red_history[i].lines[j].interactions[k].counts){
				    	return FALSE;
				}

				if (A->var_red_history[i].lines[j].interactions[k].interaction_number !=
				    B->var_red_history[i].lines[j].interactions[k].interaction_number){
				    	return FALSE;
				}
			}
		}
	}

	for (i = (A->use_zero_interactions ? 0 : 1) ; i <= A->input->general->n_interactions_trajectory ; i++) {
		for (j = 0 ; j < A->input->detector->nchannels ; j++) {
			if (A->channels_conv[i][j] !=
			    B->channels_conv[i][j]) {
				return FALSE;
			}
			if (A->channels_unconv[i][j] !=
			    B->channels_unconv[i][j]) {
				return FALSE;
			}
		}
	}

	return TRUE;
}

/**
 * xmi_archive_equals:
 * @A: first xmi_archive struct to check for equality
 * @B: second xmi_archive struct to check for equality
 *
 * Returns: %TRUE if both are equal, %FALSE otherwise.
 */
gboolean xmi_archive_equals(xmi_archive *A, xmi_archive *B) {
	if (A == NULL || B == NULL)
		return FALSE;

	if (A == B)
		return TRUE;

	if (A->single_data != B->single_data) {
		if (A->single_data->len != B->single_data->len)
			return FALSE;
		unsigned int i;
		for (i = 0 ; i < A->single_data->len ; i++) {
			if (!xmi_batch_single_data_equals(g_ptr_array_index(A->single_data, i), g_ptr_array_index(B->single_data, i)))
				return FALSE;
		}
	
	}

	if (A->output != B->output) {
		if (A->output->len != B->output->len)
			return FALSE;
		unsigned int i;
		for (i = 0 ; i < A->output->len ; i++) {
			if (!xmi_output_equals(g_ptr_array_index(A->output, i), g_ptr_array_index(B->output, i)))
				return FALSE;
		}
	}

	return TRUE;
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

#ifndef QUICKLOOK
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

/**
 * xmi_batch_single_data_new: (constructor)
 * @xpath:
 * @start:
 * @end:
 * @nsteps:
 *
 * Allocates a new #xmi_batch_single_data instance.
 *
 * Returns: (transfer full): 
 */
xmi_batch_single_data* xmi_batch_single_data_new(gchar *xpath, gdouble start, gdouble end, guint nsteps) {
	xmi_batch_single_data *rv = g_malloc0(sizeof(xmi_batch_single_data));
	rv->xpath = g_strdup(xpath);
	rv->start = start;
	rv->end = end;
	rv->nsteps = nsteps;

	return rv;
}

void xmi_batch_single_data_copy(xmi_batch_single_data *A, xmi_batch_single_data **B) {
	g_return_if_fail(A != NULL && B != NULL);
	xmi_batch_single_data *rv = g_malloc0(sizeof(xmi_batch_single_data));
	*rv = *A;
	rv->xpath = g_strdup(A->xpath);
	*B = rv;
}

void xmi_batch_single_data_free(xmi_batch_single_data *A) {
	if (!A)
		return;
	g_free(A->xpath);
	g_free(A);
}

gboolean xmi_batch_single_data_equals(xmi_batch_single_data *A, xmi_batch_single_data *B) {
	g_return_val_if_fail(A != NULL && B != NULL, FALSE);
	
	if (A == B)
		return TRUE;

	if (g_strcmp0(A->xpath, B->xpath) != 0)
		return FALSE;

	if (fabs(A->start - B->start) > 1E-10)
		return FALSE;

	if (fabs(A->end - B->end) > 1E-10)
		return FALSE;
	
	if (A->nsteps != B->nsteps)
		return FALSE;

	return TRUE;
}
#endif
