#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include <glib.h>
#include <unistd.h>

int main(int argc, char *argv[]) {


	xmi_input *input;
	xmi_output *output;
	xmi_inputFPtr inputFPtr;
	xmi_main_options *options;
	
	// set options
	options = xmi_main_options_new();
	options->use_sum_peaks = 0;
	options->verbose = 1;
	options->use_default_seeds = 1;
	options->use_escape_peaks = 0;

	// init test
	g_assert(test_init() == 1);

	// read the files
	g_assert_nonnull(input = xmi_input_read_from_xml_file(SRM1155_XMSI, NULL));
	g_assert_nonnull(output = xmi_output_read_from_xml_file(SRM1155_XMSO, NULL));

	// copy to the corresponding fortran variable
	xmi_input_C2F(input, &inputFPtr);

	// initialization
	g_assert(xmi_init_input(&inputFPtr) == 1);

	// get raw value at channel 1077 by first calculating spectrum without sum peaks
	double *channels = output->channels_unconv[output->ninteractions];
	double *channels_without_pile_up;
	
	xmi_detector_convolute_spectrum(inputFPtr, channels, &channels_without_pile_up, options, NULL, output->ninteractions);

	double counts_without_pile_up = channels_without_pile_up[1077];

	fprintf(stdout, "counts_without_pile_up: %g\n", counts_without_pile_up);	
	
	// now with pile up
	options->use_sum_peaks = 1;
	double *channels_with_pile_up;

	xmi_detector_convolute_spectrum(inputFPtr, channels, &channels_with_pile_up, options, NULL, output->ninteractions);

	double counts_with_pile_up = channels_with_pile_up[1077];
	
	fprintf(stdout, "counts_with_pile_up: %g\n", counts_with_pile_up);	

	// and now the big test -> which fails with easyRNG 1.0
	g_assert(counts_with_pile_up/counts_without_pile_up > 100.0);

	g_free(channels_without_pile_up);
	g_free(channels_with_pile_up);
	xmi_output_free(output);
	xmi_input_free(input);

	return 0;
}
