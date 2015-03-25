#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include <glib.h>
#include <math.h>

#define TEST_XMSI_URL "http://github.com/tschoonj/xmimsim/wiki/test.xmsi"
#define TEST_XMSI "test.xmsi"


int main(int argc, char *argv[]) {
	struct xmi_input *input = NULL;
	struct xmi_input *input_copy = NULL;

	//init test
	g_assert(test_init() == 1);
	
	//download file
	g_assert(test_download_file(TEST_XMSI_URL) == 1);


	//read the file
	g_assert(xmi_read_input_xml(TEST_XMSI, &input) == 1);

	//validate the file
	g_assert(xmi_validate_input(input) == 0);

	//check the values
	//general
	g_assert_cmpstr("test.xmso", == , input->general->outputfile);
	g_assert_cmpint(10000, == , input->general->n_photons_interval);
	g_assert_cmpint(1000000, == , input->general->n_photons_line);
	g_assert_cmpint(4, == , input->general->n_interactions_trajectory);

	//composition
	g_assert_cmpint(2, ==, input->composition->n_layers);

	g_assert_cmpint(3, ==, input->composition->layers[0].n_elements);
	g_assert_cmpint(7, ==, input->composition->layers[0].Z[0]);
	g_assert(fabs(0.7 - input->composition->layers[0].weight[0]) < 1E-10);
	g_assert_cmpint(8, ==, input->composition->layers[0].Z[1]);
	g_assert(fabs(0.29 - input->composition->layers[0].weight[1]) < 1E-10);
	g_assert_cmpint(18, ==, input->composition->layers[0].Z[2]);
	g_assert(fabs(0.01 - input->composition->layers[0].weight[2]) < 1E-10);
	g_assert(fabs(0.001 - input->composition->layers[0].density) < 1E-10);
	g_assert(fabs(2 - input->composition->layers[0].thickness) < 1E-10);

	g_assert_cmpint(4, ==, input->composition->layers[1].n_elements);
	g_assert_cmpint(8, ==, input->composition->layers[1].Z[0]);
	g_assert(fabs(0.276479 - input->composition->layers[1].weight[0]) < 1E-10);
	g_assert_cmpint(16, ==, input->composition->layers[1].Z[1]);
	g_assert(fabs(0.100464 - input->composition->layers[1].weight[1]) < 1E-10);
	g_assert_cmpint(29, ==, input->composition->layers[1].Z[2]);
	g_assert(fabs(0.199048 - input->composition->layers[1].weight[2]) < 1E-10);
	g_assert_cmpint(92, ==, input->composition->layers[1].Z[3]);
	g_assert(fabs(0.424009- input->composition->layers[1].weight[3]) < 1E-10);
	g_assert(fabs(2.5 - input->composition->layers[1].density) < 1E-10);
	g_assert(fabs(1 - input->composition->layers[1].thickness) < 1E-10);

	g_assert_cmpint(2, ==, input->composition->reference_layer);

	//geometry
	g_assert(fabs(100 - input->geometry->d_sample_source) < 1E-10);
	g_assert(fabs(0 - input->geometry->n_sample_orientation[0]) < 1E-10);
	g_assert(fabs(0.707107 - input->geometry->n_sample_orientation[1]) < 1E-10);
	g_assert(fabs(0.707107 - input->geometry->n_sample_orientation[2]) < 1E-10);
	g_assert(fabs(0 - input->geometry->p_detector_window[0]) < 1E-10);
	g_assert(fabs(-1 - input->geometry->p_detector_window[1]) < 1E-10);
	g_assert(fabs(100 - input->geometry->p_detector_window[2]) < 1E-10);
	g_assert(fabs(0 - input->geometry->n_detector_orientation[0]) < 1E-10);
	g_assert(fabs(1 - input->geometry->n_detector_orientation[1]) < 1E-10);
	g_assert(fabs(0 - input->geometry->n_detector_orientation[2]) < 1E-10);
	g_assert(fabs(0.3 - input->geometry->area_detector) < 1E-10);
	g_assert(fabs(0 - input->geometry->collimator_height) < 1E-10);
	g_assert(fabs(0 - input->geometry->collimator_diameter) < 1E-10);
	g_assert(fabs(100 - input->geometry->d_source_slit) < 1E-10);
	g_assert(fabs(0.001 - input->geometry->slit_size_x) < 1E-10);
	g_assert(fabs(0.001 - input->geometry->slit_size_y) < 1E-10);
	
	//excitation
	g_assert_cmpint(1, ==, input->excitation->n_discrete);
	g_assert_cmpint(0, ==, input->excitation->n_continuous);
	g_assert(fabs(20 - input->excitation->discrete[0].energy) < 1E-10);
	g_assert(fabs(1E9 - input->excitation->discrete[0].horizontal_intensity) < 1E-10);
	g_assert(fabs(1E9 - input->excitation->discrete[0].vertical_intensity) < 1E-10);
	g_assert(fabs(0 - input->excitation->discrete[0].sigma_x) < 1E-10);
	g_assert(fabs(0 - input->excitation->discrete[0].sigma_xp) < 1E-10);
	g_assert(fabs(0 - input->excitation->discrete[0].sigma_y) < 1E-10);
	g_assert(fabs(0 - input->excitation->discrete[0].sigma_yp) < 1E-10);
	
	//absorbers
	g_assert_cmpint(0, ==, input->absorbers->n_exc_layers);
	g_assert_cmpint(1, ==, input->absorbers->n_det_layers);
	g_assert_cmpint(1, ==, input->absorbers->det_layers[0].n_elements);
	g_assert_cmpint(4, ==, input->absorbers->det_layers[0].Z[0]);
	g_assert(fabs(1.0 - input->absorbers->det_layers[0].weight[0]) < 1E-10);
	g_assert(fabs(1. - input->absorbers->det_layers[0].weight[0]) < 1E-10);

	//make a copy
	xmi_copy_input(input, &input_copy);
	
	//compare
	g_assert(xmi_compare_input(input, input_copy) == 0);

	xmi_free_input(input);
	xmi_free_input(input_copy);

	return 0;
}
