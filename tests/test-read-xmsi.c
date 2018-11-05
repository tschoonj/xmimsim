#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>


int main(int argc, char *argv[]) {
	xmi_input *input = NULL;
	xmi_input *input_copy = NULL;

	//init test
	g_assert(test_init() == 1);

	//download file
	g_assert(test_download_file(TEST_XMSI_URL) == 1);

	//read the file
	g_assert(xmi_read_input_xml(TEST_XMSI, &input, NULL) == 1);

	//validate the file
	g_assert(xmi_input_validate(input) == 0);

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
	double n_sample_orientation_ref[3] = {0, 0.707107, 0.707107};
	xmi_normalize_vector_double(n_sample_orientation_ref, 3);
	g_assert(fabs(100 - input->geometry->d_sample_source) < 1E-10);
	g_assert(fabs(n_sample_orientation_ref[0] - input->geometry->n_sample_orientation[0]) < 1E-10);
	g_assert(fabs(n_sample_orientation_ref[1] - input->geometry->n_sample_orientation[1]) < 1E-10);
	g_assert(fabs(n_sample_orientation_ref[2] - input->geometry->n_sample_orientation[2]) < 1E-10);

	g_assert(fabs(0 - input->geometry->p_detector_window[0]) < 1E-10);
	g_assert(fabs(-1 - input->geometry->p_detector_window[1]) < 1E-10);
	g_assert(fabs(100 - input->geometry->p_detector_window[2]) < 1E-10);

	double n_detector_orientation_ref[3] = {0, 1, 0};
	xmi_normalize_vector_double(n_detector_orientation_ref, 3);
	g_assert(fabs(n_detector_orientation_ref[0] - input->geometry->n_detector_orientation[0]) < 1E-10);
	g_assert(fabs(n_detector_orientation_ref[1] - input->geometry->n_detector_orientation[1]) < 1E-10);
	g_assert(fabs(n_detector_orientation_ref[2] - input->geometry->n_detector_orientation[2]) < 1E-10);
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
	g_assert(fabs(1.85 - input->absorbers->det_layers[0].density) < 1E-10);
	g_assert(fabs(0.002 - input->absorbers->det_layers[0].thickness) < 1E-10);

	//detector
	g_assert_cmpint(XMI_DETECTOR_CONVOLUTION_PROFILE_SILI, ==, input->detector->detector_type);
	g_assert(fabs(1. - input->detector->live_time) < 1E-10);
	g_assert(fabs(1e-05 - input->detector->pulse_width) < 1E-10);
	g_assert_cmpint(2048, ==, input->detector->nchannels);
	g_assert(fabs(0.02 - input->detector->gain) < 1E-10);
	g_assert(fabs(0. - input->detector->zero) < 1E-10);
	g_assert(fabs(0.12 - input->detector->fano) < 1E-10);
	g_assert(fabs(0.1 - input->detector->noise) < 1E-10);
	g_assert_cmpint(1, ==, input->detector->n_crystal_layers);
	g_assert_cmpint(1, ==, input->detector->crystal_layers[0].n_elements);
	g_assert_cmpint(14, ==, input->detector->crystal_layers[0].Z[0]);
	g_assert(fabs(1.0 - input->detector->crystal_layers[0].weight[0]) < 1E-10);
	g_assert(fabs(2.33 - input->detector->crystal_layers[0].density) < 1E-10);
	g_assert(fabs(0.5 - input->detector->crystal_layers[0].thickness) < 1E-10);


	//make a copy
	xmi_input_copy(input, &input_copy);

	//compare
	g_assert(xmi_input_compare(input, input_copy) == 0);

	xmi_input_free(input);
	xmi_input_free(input_copy);

	// now some tests that are supposed to fail
	GError *error = NULL;
	g_assert(xmi_read_input_xml("non-existent-file.xmsi", &input, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);

	g_assert(replace_xml_tag(TEST_XMSI, TEST_XMSI_COPY, "/xmimsim/general/n_photons_interval", "hsdhodhoosda") == 1);
	g_assert(xmi_read_input_xml(TEST_XMSI_COPY, &input, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	unlink(TEST_XMSI_COPY);

	g_assert(remove_xml_tags(TEST_XMSI, TEST_XMSI_COPY, "/xmimsim/general/n_photons_interval") == 1);
	g_assert(xmi_read_input_xml(TEST_XMSI_COPY, &input, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	unlink(TEST_XMSI_COPY);

	return 0;
}

