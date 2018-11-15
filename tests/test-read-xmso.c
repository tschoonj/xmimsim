#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>



int main(int argc, char *argv[]) {
	xmi_output *output = NULL;
	xmi_output *output_copy = NULL;

	//init test
	g_assert(test_init() == 1);

	//download file
	g_assert(test_download_file(TEST_XMSO_URL) == 1);

	//read the file
	g_assert_nonnull(output = xmi_output_read_from_xml_file(TEST_XMSO, NULL));

	//validate the input
	g_assert(xmi_input_validate(output->input) == 0);

	//make a copy
	xmi_output_copy(output, &output_copy);
	g_assert(xmi_input_validate(output_copy->input) == 0);
	g_assert(xmi_input_compare(output->input, output_copy->input) == 0);
	g_assert_true(xmi_output_equals(output, output_copy));

	xmi_output_free(output);
	xmi_output_free(output_copy);

	// now some tests that are supposed to fail
	GError *error = NULL;
	g_assert_null(output = xmi_output_read_from_xml_file("non-existent-file.xmso", &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);

	g_assert(replace_xml_tag(TEST_XMSO, TEST_XMSO_COPY, "/xmimsim-results/spectrum_conv/channel[1]/counts[1]", "hsdhodhoosda") == 1);
	g_assert_null(output = xmi_output_read_from_xml_file(TEST_XMSO_COPY, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	unlink(TEST_XMSO_COPY);

	g_assert(remove_xml_tags(TEST_XMSO, TEST_XMSO_COPY, "/xmimsim-results/brute_force_history") == 1);
	g_assert_null(output = xmi_output_read_from_xml_file(TEST_XMSO_COPY, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	unlink(TEST_XMSO_COPY);
	return 0;
}
