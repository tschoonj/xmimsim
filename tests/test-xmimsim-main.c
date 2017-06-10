#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include <glib.h>
#include <unistd.h>
#include <xraylib.h>


extern long hits_per_single;

int main(int argc, char **argv) {

	const char compound[] = COMPOUND;
	struct xmi_solid_angle *solid_angle_def = NULL;
	double *channels;
	double *brute_history;
	double *var_red_history;
	int i;
	struct xmi_main_options options = xmi_get_default_main_options();
	options.use_escape_peaks = 0;
	options.verbose = 1;

	// init test
	g_assert(test_init() == 1);
	
	xmi_init_hdf5();

	// read compound
	struct compoundData *cd = CompoundParser(compound);
	g_assert(cd != NULL);

	// generate appropriate xmimsimdata.h5 file
	gchar *data_file = g_strdup_printf("xmimsimdata-%s.h5", compound);
	//if (!g_file_test(data_file, G_FILE_TEST_EXISTS))
		g_assert(xmi_db(data_file, cd->Elements, cd->nElements));

	// get default input-file
	struct xmi_input *input = xmi_init_empty_input();

	// calculate first interaction only
	input->general->n_interactions_trajectory = 1;

	// add compound to composition
	struct xmi_layer *layer = compoundData2xmi_layer(cd);
	layer->thickness = 1.0;
	layer->density = 1.0;
	input->composition->n_layers = 1;
	input->composition->layers = layer;
	
	// increase sample-detector distance to have more reliable fundamental parameter calculation
	input->geometry->p_detector_window[1] = 10.0;

	// copy input to fortran variable
	xmi_inputFPtr inputFPtr;
	xmi_input_C2F(input, &inputFPtr);
	
	// initialization
	g_assert(xmi_init_input(&inputFPtr) == 1);
	
	// read data file
	xmi_hdf5FPtr hdf5FPtr;
	g_assert(xmi_init_from_hdf5(data_file, inputFPtr, &hdf5FPtr, options) == 1);
	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);

	// start random number acquisition
	g_assert(xmi_start_random_acquisition() == 1);

	// reduce hits_per_single to shorten calculation time
	hits_per_single = 2000;

	// run the actual calculation
	xmi_solid_angle_calculation(inputFPtr, &solid_angle_def, NULL, options);

	// check solid_angle_def is not NULL
	g_assert(solid_angle_def != NULL);
	
	// run simulation
	g_assert(xmi_main_msim(inputFPtr, hdf5FPtr, 1, &channels, options, &brute_history, &var_red_history, solid_angle_def) == 1);

	// apply detector response function
	double **channels_conv = (double **) g_malloc(sizeof(double *)*(input->general->n_interactions_trajectory+1));
	double **channels_def_ptrs = g_malloc(sizeof(double *) * (input->general->n_interactions_trajectory+1));
	for (i = 0 ; i <= input->general->n_interactions_trajectory ; i++)
		channels_def_ptrs[i] = channels+i*input->detector->nchannels;
	
	xmi_detector_convolute_all(inputFPtr, channels_def_ptrs, channels_conv, brute_history, var_red_history, options, NULL, input->general->n_interactions_trajectory, 0);

	// convert to xmi_output
	struct xmi_output *output = xmi_output_raw2struct(input, brute_history, var_red_history, channels_conv, channels, "test.xmso", 0);

	// analyze output by comparing results with FPM

	// cleanup
	xmi_free_output(output);
	for (i = 1 ; i <= input->general->n_interactions_trajectory ; i++)
		xmi_deallocate(channels_conv[i] );
	g_free(channels_conv);
	g_free(channels);
	g_free(channels_def_ptrs);
	xmi_free_solid_angle(solid_angle_def);
	unlink(data_file);
	g_free(data_file);
	FreeCompoundData(cd);
	xmi_free_input(input);
	xmi_free_input_F(&inputFPtr);

	g_assert(xmi_end_random_acquisition() == 1);

	return 0;
}
