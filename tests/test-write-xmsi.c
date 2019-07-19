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
	g_assert_nonnull(input = xmi_input_read_from_xml_file(TEST_XMSI, NULL));

	//copy to a new file
	g_assert_true(xmi_input_write_to_xml_file(input, TEST_XMSI_COPY, NULL));

	//read the copy
	g_assert_nonnull(input_copy = xmi_input_read_from_xml_file(TEST_XMSI_COPY, NULL));

	//ensure they are identical
	g_assert(xmi_input_compare(input, input_copy) == 0);

	xmi_input_free(input_copy);

	//delete the file
	unlink(TEST_XMSI_COPY);

	// this test should fail
	GError *error = NULL;
	g_assert_false(xmi_input_write_to_xml_file(input, "non-existent-folder" G_DIR_SEPARATOR_S TEST_XMSI_COPY, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	

	//test xml_string methods
	gchar *xml_string = NULL;
	g_assert_true(xmi_input_write_to_xml_string(input, &xml_string, NULL));
	g_assert_nonnull(input_copy = xmi_input_read_from_xml_string(xml_string, NULL));

	//ensure they are identical
	g_assert(xmi_input_compare(input, input_copy) == 0);

	xmi_input_free(input_copy);
		
	xmi_input_free(input);

	//delete the original file
	unlink(TEST_XMSI);

	return 0;
}
