#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>


int main(int argc, char *argv[]) {
	struct xmi_output *output = NULL;
	struct xmi_output *output_copy = NULL;

	//init test
	g_assert(test_init() == 1);

	//read the file
	g_assert(xmi_read_output_xml(TEST_XMSO, &output, NULL) == 1);

	//copy to a new file
	g_assert(xmi_write_output_xml(TEST_XMSO_COPY, output, NULL) == 1);

	//read the copy
	g_assert(xmi_read_output_xml(TEST_XMSO_COPY, &output_copy, NULL) == 1);

	//ensure they are identical
	g_assert(xmi_compare_output(output, output_copy) == 0);

	xmi_free_output(output_copy);

	//delete the file
	unlink(TEST_XMSO_COPY);

	// this test should fail
	GError *error = NULL;
	g_assert(xmi_write_output_xml("non-existent-folder" G_DIR_SEPARATOR_S TEST_XMSO_COPY, output, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);
	
	xmi_free_output(output);

	return 0;
}

