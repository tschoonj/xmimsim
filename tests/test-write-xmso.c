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

	//read the file
	g_assert_nonnull(output = xmi_output_read_from_xml_file(TEST_XMSO, NULL));

	//copy to a new file
	g_assert_true(xmi_output_write_to_xml_file(output, TEST_XMSO_COPY, NULL));

	//read the copy
	g_assert_nonnull(output_copy = xmi_output_read_from_xml_file(TEST_XMSO_COPY, NULL));

	//ensure they are identical
	g_assert_true(xmi_output_equals(output, output_copy));

	xmi_output_free(output_copy);

	//delete the file
	unlink(TEST_XMSO_COPY);

	// this test should fail
	GError *error = NULL;
	g_assert_false(xmi_output_write_to_xml_file(output, "non-existent-folder" G_DIR_SEPARATOR_S TEST_XMSO_COPY, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);
	
	xmi_output_free(output);

	return 0;
}

