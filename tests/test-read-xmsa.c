#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>


int main(int argc, char *argv[]) {
	struct xmi_archive *archive = NULL;
	int i,j;

	//init test
	g_assert(test_init() == 1);
	
	//download file
	g_assert(test_download_file(TEST_XMSA_URL_1) == 1);

	//read the file
	g_assert(xmi_read_archive_xml(TEST_XMSA_1, &archive) == 1);

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
	g_assert(xmi_read_archive_xml(TEST_XMSA_2, &archive) == 1);

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
	return 0;
}

