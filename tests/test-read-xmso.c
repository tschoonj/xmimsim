#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>


#define TEST_XMSO_URL "http://github.com/tschoonj/xmimsim/wiki/test.xmso"
#define TEST_XMSO "test.xmso"


int main(int argc, char *argv[]) {
	struct xmi_output *output = NULL;
	struct xmi_output *output_copy = NULL;

	//init test
	g_assert(test_init() == 1);
	
	//download file
	g_assert(test_download_file(TEST_XMSO_URL) == 1);

	//read the file
	g_assert(xmi_read_output_xml(TEST_XMSO, &output) == 1);

	//validate the input
	g_assert(xmi_validate_input(output->input) == 0);

	//make a copy
	xmi_copy_output(output, &output_copy);
	g_assert(xmi_validate_input(output_copy->input) == 0);
	g_assert(xmi_compare_input(output->input, output_copy->input) == 0);
	
	xmi_free_output(output);
	xmi_free_output(output_copy);

	return 0;
}
