#include "xmi_xml.h"
#include "stdio.h"



int main (int argc, char *argv[]) {

	struct xmi_input *input,*input2;
	int rv;
	int i,j;

	if (argc < 2) {
		fprintf(stderr,"one argument required\n");
		return 1;
	}

	rv = xmi_read_input_xml(argv[1],&input);

	fprintf(stdout,"rv: %i\n",rv);

	if (rv == 0 )
		return 0;

	fprintf(stdout,"version: %lf\n",input->general->version);
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

	fprintf(stdout,"d_sample_source: %lf\n",input->geometry->d_sample_source);
	fprintf(stdout,"n_sample_orientation[0]: %lf\n",input->geometry->n_sample_orientation[0]);
	fprintf(stdout,"n_sample_orientation[1]: %lf\n",input->geometry->n_sample_orientation[1]);
	fprintf(stdout,"n_sample_orientation[2]: %lf\n",input->geometry->n_sample_orientation[2]);
	fprintf(stdout,"p_detector_window[0]: %lf\n",input->geometry->p_detector_window[0]);
	fprintf(stdout,"p_detector_window[1]: %lf\n",input->geometry->p_detector_window[1]);
	fprintf(stdout,"p_detector_window[2]: %lf\n",input->geometry->p_detector_window[2]);
	fprintf(stdout,"n_detector_orientation[0]: %lf\n",input->geometry->n_detector_orientation[0]);
	fprintf(stdout,"n_detector_orientation[1]: %lf\n",input->geometry->n_detector_orientation[1]);
	fprintf(stdout,"n_detector_orientation[2]: %lf\n",input->geometry->n_detector_orientation[2]);
	fprintf(stdout,"area_detector: %lf\n",input->geometry->area_detector);
	fprintf(stdout,"acceptance_detector: %lf\n",input->geometry->acceptance_detector);
	fprintf(stdout,"d_source_slit: %lf\n",input->geometry->d_source_slit);
	fprintf(stdout,"slit_size_x: %lf\n",input->geometry->slit_size_x);
	fprintf(stdout,"slit_size_y: %lf\ni\n",input->geometry->slit_size_y);

	fprintf(stdout,"excitation\n");
	for (i = 0 ; i < input->excitation->n_discrete ; i++) { 
		fprintf(stdout,"discrete: %lf -> %lf and %lf\n",input->excitation->discrete[i].energy,input->excitation->discrete[i].horizontal_intensity,input->excitation->discrete[i].vertical_intensity);
		fprintf(stdout,"sigma_x: %lf\n",input->excitation->discrete[i].sigma_x);
		fprintf(stdout,"sigma_xp: %lf\n",input->excitation->discrete[i].sigma_xp);
		fprintf(stdout,"sigma_y: %lf\n",input->excitation->discrete[i].sigma_y);
		fprintf(stdout,"sigma_yp: %lf\n",input->excitation->discrete[i].sigma_yp);
	}
	for (i = 0 ; i < input->excitation->n_continuous ; i++) { 
		fprintf(stdout,"continuous: %lf -> %lf and %lf\n",input->excitation->continuous[i].energy,input->excitation->continuous[i].horizontal_intensity,input->excitation->continuous[i].vertical_intensity);
		fprintf(stdout,"sigma_x: %lf\n",input->excitation->continuous[i].sigma_x);
		fprintf(stdout,"sigma_xp: %lf\n",input->excitation->continuous[i].sigma_xp);
		fprintf(stdout,"sigma_y: %lf\n",input->excitation->continuous[i].sigma_y);
		fprintf(stdout,"sigma_yp: %lf\n",input->excitation->continuous[i].sigma_yp);
	}
	
	fprintf(stdout,"absorbers\n");
	for (i = 0 ; i < input->absorbers->n_exc_layers ; i++) {
		for (j = 0 ; j < input->absorbers->exc_layers[i].n_elements ; j++) {
			fprintf(stdout,"Z: %i   weight: %lf\n",input->absorbers->exc_layers[i].Z[j],input->absorbers->exc_layers[i].weight[j]);
		}
		fprintf(stdout,"density: %lf\n",input->absorbers->exc_layers[i].density);
		fprintf(stdout,"thickness: %lf\n\n",input->absorbers->exc_layers[i].thickness);
	} 
	
	for (i = 0 ; i < input->absorbers->n_det_layers ; i++) {
		for (j = 0 ; j < input->absorbers->det_layers[i].n_elements ; j++) {
			fprintf(stdout,"Z: %i   weight: %lf\n",input->absorbers->det_layers[i].Z[j],input->absorbers->det_layers[i].weight[j]);
		}
		fprintf(stdout,"density: %lf\n",input->absorbers->det_layers[i].density);
		fprintf(stdout,"thickness: %lf\n\n",input->absorbers->det_layers[i].thickness);
	} 

	fprintf(stdout,"detector\n");
	fprintf(stdout,"Detector type: %i\n",input->detector->detector_type);
	fprintf(stdout,"Detector gain: %lf\n",input->detector->gain);
	fprintf(stdout,"Detector zero: %lf\n",input->detector->zero);
	fprintf(stdout,"Detector fanp: %lf\n",input->detector->fano);
	fprintf(stdout,"Detector noise: %lf\n",input->detector->noise);
	fprintf(stdout,"Detector max_convolution_energy: %lf\n",input->detector->max_convolution_energy);
	for (i = 0 ; i < input->detector->n_crystal_layers ; i++) {
		for (j = 0 ; j < input->detector->crystal_layers[i].n_elements ; j++) {
			fprintf(stdout,"Z: %i   weight: %lf\n",input->detector->crystal_layers[i].Z[j],input->detector->crystal_layers[i].weight[j]);
		}
		fprintf(stdout,"density: %lf\n",input->detector->crystal_layers[i].density);
		fprintf(stdout,"thickness: %lf\n\n",input->detector->crystal_layers[i].thickness);
	} 


	rv = xmi_write_input_xml("test_output.xml",input);

	fprintf(stdout,"rv: %i\n",rv);


	xmi_copy_input(input,&input2);

	rv = xmi_write_input_xml("test_output_copy.xml",input2);

	fprintf(stdout,"rv: %i\n",rv);
	xmi_free_input(input);
	xmi_free_input(input2);

	rv = xmi_read_input_xml("test_output.xml",&input);
	fprintf(stdout,"rv read copy_a: %i\n",rv);
	rv = xmi_read_input_xml("test_output_copy.xml",&input2);
	fprintf(stdout,"rv read copy_b: %i\n",rv);


	rv= xmi_compare_input(input,input2);
	fprintf(stdout,"compare rv: %i\n",rv);

	xmi_free_input(input);
	xmi_free_input(input2);

	return 0;

}
