#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>


int main(int argc, char *argv[]) {
	xmi_archive *archive = NULL;
	xmi_archive *archive_copy = NULL;

	//init test
	g_assert(test_init() == 1);

	//read the file
	g_assert_nonnull(archive = xmi_archive_read_from_xml_file(TEST_XMSA_1, NULL));

	//copy to a new file
	g_assert_true(xmi_archive_write_to_xml_file(archive, TEST_XMSA_COPY_1, NULL));

	//read the copy
	g_assert_nonnull(archive_copy = xmi_archive_read_from_xml_file(TEST_XMSA_COPY_1, NULL));

	//ensure they are identical
	g_assert_true(xmi_archive_equals(archive, archive_copy));

	xmi_archive_free(archive);
	xmi_archive_free(archive_copy);

	//delete the file
	unlink(TEST_XMSA_COPY_1);

	//read the file
	g_assert_nonnull(archive = xmi_archive_read_from_xml_file(TEST_XMSA_2, NULL));

	//copy to a new file
	g_assert_true(xmi_archive_write_to_xml_file(archive, TEST_XMSA_COPY_2, NULL));

	//read the copy
	g_assert_nonnull(archive_copy = xmi_archive_read_from_xml_file(TEST_XMSA_COPY_2, NULL));

	//ensure they are identical
	g_assert_true(xmi_archive_equals(archive, archive_copy));

	xmi_archive_free(archive_copy);

	//delete the file
	unlink(TEST_XMSA_COPY_2);

	// this test should fail
	GError *error = NULL;
	g_assert_false(xmi_archive_write_to_xml_file(archive, "non-existent-folder" G_DIR_SEPARATOR_S TEST_XMSA_COPY_1, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);
	
	xmi_archive_free(archive);

	return 0;
}

