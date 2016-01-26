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
	g_assert(xmi_read_archive_xml(TEST_XMSA_1, &archive) == 1);

	//copy to a new file
	g_assert(xmi_write_archive_xml(TEST_XMSA_COPY_1, archive) == 1);

	//read the copy
	g_assert(xmi_read_archive_xml(TEST_XMSA_COPY_1, &archive_copy) == 1);

	//ensure they are identical
	g_assert(xmi_compare_archive(archive, archive_copy) == 0);

	xmi_free_archive(archive);
	xmi_free_archive(archive_copy);

	//delete the file
	unlink(TEST_XMSA_COPY_1);

	//read the file
	g_assert(xmi_read_archive_xml(TEST_XMSA_2, &archive) == 1);

	//copy to a new file
	g_assert(xmi_write_archive_xml(TEST_XMSA_COPY_2, archive) == 1);

	//read the copy
	g_assert(xmi_read_archive_xml(TEST_XMSA_COPY_2, &archive_copy) == 1);

	//ensure they are identical
	g_assert(xmi_compare_archive(archive, archive_copy) == 0);

	xmi_free_archive(archive);
	xmi_free_archive(archive_copy);

	//delete the file
	unlink(TEST_XMSA_COPY_2);

	return 0;
}

