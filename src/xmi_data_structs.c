#include "xmi_data_structs.h"
#include "xmi_aux.h"
#include <stdlib.h>
#include <string.h>


static void xmi_free_layer (struct xmi_layer *layer) {
	free(layer->Z);
	free(layer->weight);
}





void xmi_free_input(struct xmi_input *input) {
	int i;

	//general
	free(input->general->outputfile);
	free(input->general);

	//composition
	for (i = 0 ; i < input->composition->n_layers ; i++) 
		xmi_free_layer(input->composition->layers+i);
	 
	free(input->composition->layers);
	free(input->composition);

	//geometry
	free(input->geometry);

	//excitation
	if (input->excitation->n_discrete > 0)
		free(input->excitation->discrete);

	if (input->excitation->n_continuous > 0)
		free(input->excitation->continuous);

	free(input->excitation);

	//absorbers
	if (input->absorbers->n_exc_layers > 0)
		for (i = 0 ; i < input->absorbers->n_exc_layers ; i++) 
			xmi_free_layer(input->absorbers->exc_layers+i);
		
	if (input->absorbers->n_det_layers > 0)
		for (i = 0 ; i < input->absorbers->n_det_layers ; i++) 
			xmi_free_layer(input->absorbers->det_layers+i);

	free(input->absorbers);

	//detector
	if (input->detector->n_crystal_layers > 0)	
		for (i = 0 ; i < input->detector->n_crystal_layers ; i++) 
			xmi_free_layer(input->detector->crystal_layers+i);

	free(input->detector);

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

	//composition
	(*B)->composition = (struct xmi_composition *) malloc(sizeof(struct xmi_composition));
	(*B)->composition->n_layers = (A)->composition->n_layers;
	(*B)->composition->layers = (struct xmi_layer *) xmi_memdup((A)->composition->layers,((A)->composition->n_layers)*sizeof(struct xmi_layer));
	for (i = 0 ; i < (A)->composition->n_layers ; i++) {
		(*B)->composition->layers[i].Z = (int *) xmi_memdup((A)->composition->layers[i].Z,((A)->composition->layers[i].n_elements)*sizeof(int));
		(*B)->composition->layers[i].weight = (double *) xmi_memdup((A)->composition->layers[i].weight,((A)->composition->layers[i].n_elements)*sizeof(double));
	}
	
	//geometry
	(*B)->geometry = (struct xmi_geometry *) xmi_memdup((A)->geometry,sizeof(struct xmi_geometry));

	//excitation
	
	(*B)->excitation = (struct xmi_excitation *) malloc(sizeof(struct xmi_excitation));
	(*B)->excitation->n_discrete = (A)->excitation->n_discrete;
	(*B)->excitation->n_continuous = (A)->excitation->n_continuous;

	if ((*B)->excitation->n_discrete > 0) {
		(*B)->excitation->discrete = (struct xmi_energy *) xmi_memdup((A)->excitation->discrete,(A)->excitation->n_discrete*sizeof(struct xmi_energy));
	}
	if ((*B)->excitation->n_continuous > 0) {
		(*B)->excitation->continuous = (struct xmi_energy *) xmi_memdup((A)->excitation->continuous,(A)->excitation->n_continuous*sizeof(struct xmi_energy));
	}

	//absorbers
	(*B)->absorbers = (struct xmi_absorbers*) malloc(sizeof(struct xmi_absorbers));
	(*B)->absorbers->n_exc_layers = (A)->absorbers->n_exc_layers; 
	(*B)->absorbers->n_det_layers = (A)->absorbers->n_det_layers; 

	if ((*B)->absorbers->n_exc_layers > 0) {
		(*B)->absorbers->exc_layers = (struct xmi_layer *) xmi_memdup((A)->absorbers->exc_layers,((A)->absorbers->n_exc_layers)*sizeof(struct xmi_layer));
		for (i = 0 ; i < (A)->absorbers->n_exc_layers ; i++) {
			(*B)->absorbers->exc_layers[i].Z = (int *) xmi_memdup((A)->absorbers->exc_layers[i].Z,((A)->absorbers->exc_layers[i].n_elements)*sizeof(int));
			(*B)->absorbers->exc_layers[i].weight = (double *) xmi_memdup((A)->absorbers->exc_layers[i].weight,((A)->absorbers->exc_layers[i].n_elements)*sizeof(double));
		}
	}
	if ((*B)->absorbers->n_det_layers > 0) {
		(*B)->absorbers->det_layers = (struct xmi_layer *) xmi_memdup((A)->absorbers->det_layers,((A)->absorbers->n_det_layers)*sizeof(struct xmi_layer));
		for (i = 0 ; i < (A)->absorbers->n_det_layers ; i++) {
			(*B)->absorbers->det_layers[i].Z = (int *) xmi_memdup((A)->absorbers->det_layers[i].Z,((A)->absorbers->det_layers[i].n_elements)*sizeof(int));
			(*B)->absorbers->det_layers[i].weight = (double *) xmi_memdup((A)->absorbers->det_layers[i].weight,((A)->absorbers->det_layers[i].n_elements)*sizeof(double));
		}
	}

	//detector
	(*B)->detector = (struct xmi_detector *) xmi_memdup((A)->detector,sizeof(struct xmi_detector));
	(*B)->detector->crystal_layers = (struct xmi_layer *) xmi_memdup((A)->detector->crystal_layers,((A)->detector->n_crystal_layers)*sizeof(struct xmi_layer));
	for (i = 0 ; i < (A)->detector->n_crystal_layers ; i++) {
		(*B)->detector->crystal_layers[i].Z = (int *) xmi_memdup((A)->detector->crystal_layers[i].Z,((A)->detector->crystal_layers[i].n_elements)*sizeof(int));
		(*B)->detector->crystal_layers[i].weight = (double *) xmi_memdup((A)->detector->crystal_layers[i].weight,((A)->detector->crystal_layers[i].n_elements)*sizeof(double));
	}

	


}




