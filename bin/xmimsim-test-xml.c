#include "xmi_xml.h"
#include "stdio.h"



int main (int argc, char *argv[]) {

	struct xmi_input *input;
	int rv;

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
	
	return 0;

}
