#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include <glib.h>

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

	//make a copy
	xmi_copy_input(input, &input_copy);
	

	xmi_free_input(input);
	xmi_free_input(input_copy);

	return 0;
}

