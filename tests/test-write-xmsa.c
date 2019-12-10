#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <math.h>
#include <unistd.h>

typedef struct {
	gchar something;
} SetupData;

typedef struct {
	gchar *url;
	gchar *plain;
	gchar *copy;
} UserData;

static void setup_data(SetupData *data, gconstpointer user_data) {

}

static void test_read_write_read(SetupData *data, gconstpointer _user_data) {
	const UserData *user_data = _user_data;
	xmi_archive *archive = NULL;
	xmi_archive *archive_copy = NULL;

	//download files
  	if (g_access(user_data->plain, R_OK) != 0)
	  g_assert_cmpint(test_download_file(user_data->url), ==, 1);

	//read the file
	g_assert_nonnull(archive = xmi_archive_read_from_xml_file(user_data->plain, NULL));

	//copy to a new file
	g_assert_true(xmi_archive_write_to_xml_file(archive, user_data->copy, NULL));

	//read the copy
	g_assert_nonnull(archive_copy = xmi_archive_read_from_xml_file(user_data->copy, NULL));

	//ensure they are identical
	g_assert_true(xmi_archive_equals(archive, archive_copy));

	g_assert_cmpint(archive->ref_count, ==, 1);
	xmi_archive_unref(archive);
	g_assert_cmpint(archive_copy->ref_count, ==, 1);
	xmi_archive_unref(archive_copy);
}

static void test_expected_failure(SetupData *data, gconstpointer _user_data) {
	const UserData *user_data = _user_data;

	xmi_archive *archive = NULL;

	//download files
  	if (g_access(user_data->plain, R_OK) != 0)
	  g_assert_cmpint(test_download_file(user_data->url), ==, 1);

	//read the file
	g_assert_nonnull(archive = xmi_archive_read_from_xml_file(user_data->plain, NULL));
	GError *error = NULL;
	g_assert_false(xmi_archive_write_to_xml_file(archive, g_build_filename("non-existent-folder", user_data->copy, NULL), &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_error_free(error);
	
	g_assert_cmpint(archive->ref_count, ==, 1);
	xmi_archive_unref(archive);
}

static void teardown_data(SetupData *data, gconstpointer _user_data) {
	const UserData *user_data = _user_data;
	g_assert_cmpint(g_unlink(user_data->copy), ==, 0);
}

int main(int argc, char *argv[]) {

	//init test
	g_assert_cmpint(test_init(), ==, 1);
	g_test_init(&argc, &argv, NULL);

	UserData user_data1 = {
		.url = TEST_XMSA_URL_1_OLD,
		.plain = TEST_XMSA_1_OLD,
		.copy = TEST_XMSA_COPY_1_OLD
	};
	g_test_add("/write-xmsa/1D/old",
		SetupData,
		&user_data1,
		setup_data,
		test_read_write_read,
		teardown_data
	);

	UserData user_data2 = {
		.url = TEST_XMSA_URL_2_OLD,
		.plain = TEST_XMSA_2_OLD,
		.copy = TEST_XMSA_COPY_2_OLD
	};
	g_test_add("/write-xmsa/2D/old",
		SetupData,
		&user_data2,
		setup_data,
		test_read_write_read,
		teardown_data
	);

	UserData user_data3 = {
		.url = TEST_XMSA_URL_1_OLD,
		.plain = TEST_XMSA_1_OLD,
		.copy = TEST_XMSA_COPY_1_OLD
	};
	g_test_add("/write-xmsa/expected-failure/old",
		SetupData,
		&user_data3,
		setup_data,
		test_expected_failure,
		NULL	
	);

	UserData user_data4 = {
		.url = TEST_XMSA_URL_1_NEW,
		.plain = TEST_XMSA_1_NEW,
		.copy = TEST_XMSA_COPY_1_NEW
	};
	g_test_add("/write-xmsa/1D/new",
		SetupData,
		&user_data4,
		setup_data,
		test_read_write_read,
		teardown_data
	);

	UserData user_data5 = {
		.url = TEST_XMSA_URL_2_NEW,
		.plain = TEST_XMSA_2_NEW,
		.copy = TEST_XMSA_COPY_2_NEW
	};
	g_test_add("/write-xmsa/2D/new",
		SetupData,
		&user_data5,
		setup_data,
		test_read_write_read,
		teardown_data
	);

	UserData user_data6 = {
		.url = TEST_XMSA_URL_1_NEW,
		.plain = TEST_XMSA_1_NEW,
		.copy = TEST_XMSA_COPY_1_NEW
	};
	g_test_add("/write-xmsa/expected-failure/new",
		SetupData,
		&user_data6,
		setup_data,
		test_expected_failure,
		NULL	
	);

	return g_test_run();
}

