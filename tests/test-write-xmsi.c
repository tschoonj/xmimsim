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

	//read the file
	g_assert(xmi_read_input_xml(TEST_XMSI, &input, NULL) == 1);

	//copy to a new file
	g_assert(xmi_write_input_xml(TEST_XMSI_COPY, input, NULL) == 1);

	//read the copy
	g_assert(xmi_read_input_xml(TEST_XMSI_COPY, &input_copy, NULL) == 1);

	//ensure they are identical
	g_assert(xmi_compare_input(input, input_copy) == 0);

	xmi_free_input(input_copy);

	//delete the file
	unlink(TEST_XMSI_COPY);

	// this test should fail
	GError *error = NULL;
	g_assert(xmi_write_input_xml("non-existent-folder" G_DIR_SEPARATOR_S TEST_XMSI_COPY, input, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);
	
	xmi_free_input(input);

	return 0;
}
