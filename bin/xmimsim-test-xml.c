#include "xmi_xml.h"
#include "stdio.h"



int main (int argc, char *argv[]) {

	struct xmi_input *input;
	int rv;
	int i,j;

	if (argc < 2) {
		fprintf(stderr,"one argument required\n");
		return 1;
	}

	rv = xmi_read_input_xml(argv[1],&input);

	fprintf(stdout,"rv: %i\n",rv);

	fprintf(stdout,"version: %i\n",input->general->version);
	fprintf(stdout,"outputfile: %s\n",input->general->outputfile);
	fprintf(stdout,"n_photons_interval: %li\n",input->general->n_photons_interval);
	fprintf(stdout,"n_photons_line: %li\n",input->general->n_photons_line);
	fprintf(stdout,"n_interactions_trajectory: %i\n",input->general->n_interactions_trajectory);

	for (i = 0 ; i < input->composition->n_layers ; i++) {
		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			fprintf(stdout,"Z: %i   weight: %lf\n",input->composition->layers[i].Z[j],input->composition->layers[i].weight[j]);
		}
		fprintf(stdout,"density: %lf\n",input->composition->layers[i].density);
		fprintf(stdout,"thickness: %lf\n\n",input->composition->layers[i].thickness);
	} 


	return 0;

}
