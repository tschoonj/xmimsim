#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>


int main(int argc, char *argv[]) {
	struct xmi_archive *archive = NULL;
	int i,j;

	//init test
	g_assert(test_init() == 1);

	//download file
	g_assert(test_download_file(TEST_XMSA_URL_1) == 1);

	//read the file
	g_assert(xmi_read_archive_xml(TEST_XMSA_1, &archive, NULL) == 1);

	//some testing of the input and output
	for (i = 0 ; i <= archive->nsteps1 ; i++) {
		for (j = 0 ; j <= archive->nsteps2 ; j++) {
			struct xmi_output *output_copy = NULL;
			xmi_copy_output(archive->output[i][j], &output_copy);
			g_assert(xmi_validate_input(output_copy->input) == 0);
			g_assert(xmi_compare_input(archive->input[i][j], output_copy->input) == 0);
			xmi_free_output(output_copy);
		}
	}

	xmi_free_archive(archive);

	//download file
	g_assert(test_download_file(TEST_XMSA_URL_2) == 1);

	//read the file
	g_assert(xmi_read_archive_xml(TEST_XMSA_2, &archive, NULL) == 1);

	//some testing of the input and output
	for (i = 0 ; i <= archive->nsteps1 ; i++) {
		for (j = 0 ; j <= archive->nsteps2 ; j++) {
			struct xmi_output *output_copy = NULL;
			xmi_copy_output(archive->output[i][j], &output_copy);
			g_assert(xmi_validate_input(output_copy->input) == 0);
			g_assert(xmi_compare_input(archive->input[i][j], output_copy->input) == 0);
			xmi_free_output(output_copy);
		}
	}

	xmi_free_archive(archive);

	// now some tests that are supposed to fail
	GError *error = NULL;
	g_assert(xmi_read_archive_xml("non-existent-file.xmsa", &archive, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);

	/* TODO: enable when check for xpath1 validity is added to xmi_read_archive_xml
	g_assert(replace_xml_tag(TEST_XMSA_1, TEST_XMSA_COPY_1, "/xmimsim-archive/xpath1", "hsdhodhoosda") == 1);
	g_assert(xmi_read_archive_xml(TEST_XMSA_COPY_1, &archive, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);
	unlink(TEST_XMSA_COPY_1);
	*/
	g_assert(remove_xml_tags(TEST_XMSA_1, TEST_XMSA_COPY_1, "/xmimsim-archive/end_value1") == 1);
	g_assert(xmi_read_archive_xml(TEST_XMSA_COPY_1, &archive, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);
	unlink(TEST_XMSA_COPY_1);
	return 0;
}

