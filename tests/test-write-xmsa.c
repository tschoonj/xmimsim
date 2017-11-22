#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>


int main(int argc, char *argv[]) {
	struct xmi_archive *archive = NULL;
	struct xmi_archive *archive_copy = NULL;

	//init test
	g_assert(test_init() == 1);

	//read the file
	g_assert(xmi_read_archive_xml(TEST_XMSA_1, &archive, NULL) == 1);

	//copy to a new file
	g_assert(xmi_write_archive_xml(TEST_XMSA_COPY_1, archive, NULL) == 1);

	//read the copy
	g_assert(xmi_read_archive_xml(TEST_XMSA_COPY_1, &archive_copy, NULL) == 1);

	//ensure they are identical
	g_assert(xmi_compare_archive(archive, archive_copy) == 0);

	xmi_free_archive(archive);
	xmi_free_archive(archive_copy);

	//delete the file
	unlink(TEST_XMSA_COPY_1);

	//read the file
	g_assert(xmi_read_archive_xml(TEST_XMSA_2, &archive, NULL) == 1);

	//copy to a new file
	g_assert(xmi_write_archive_xml(TEST_XMSA_COPY_2, archive, NULL) == 1);

	//read the copy
	g_assert(xmi_read_archive_xml(TEST_XMSA_COPY_2, &archive_copy, NULL) == 1);

	//ensure they are identical
	g_assert(xmi_compare_archive(archive, archive_copy) == 0);

	xmi_free_archive(archive_copy);

	//delete the file
	unlink(TEST_XMSA_COPY_2);

	// this test should fail
	GError *error = NULL;
	g_assert(xmi_write_archive_xml("non-existent-folder" G_DIR_SEPARATOR_S TEST_XMSA_COPY_1, archive, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);
	
	xmi_free_archive(archive);

	return 0;
}

