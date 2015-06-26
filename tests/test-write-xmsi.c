#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>


int main(int argc, char *argv[]) {
	struct xmi_input *input = NULL;
	struct xmi_input *input_copy = NULL;

	//init test
	g_assert(test_init() == 1);

	//read the file
	g_assert(xmi_read_input_xml(TEST_XMSI, &input) == 1);

	//copy to a new file
	g_assert(xmi_write_input_xml(TEST_XMSI_COPY, input) == 1);

	//read the copy
	g_assert(xmi_read_input_xml(TEST_XMSI_COPY, &input_copy) == 1);

	//ensure they are identical
	g_assert(xmi_compare_input(input, input_copy) == 0);

	xmi_free_input(input);
	xmi_free_input(input_copy);

	//delete the file
	unlink(TEST_XMSI_COPY);

	return 0;
}
