#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <math.h>
#include <unistd.h>

typedef struct {
	gchar temp;
} SetupData;

typedef struct {
	gchar *url;
	gchar *plain;
	gchar *copy;
	double counts_3_101;
	double var_red_history_2_total_counts;
	double var_red_history_2_lines_2_total_counts;
	double var_red_history_2_lines_2_interactions_2_counts;
	GArray *indices_arr;
} UserData;

static void setup_data(SetupData *data, gconstpointer _user_data) {
	const UserData *user_data = _user_data;

	if (user_data && user_data->plain && g_access(user_data->plain, R_OK) != 0)
	  g_assert_cmpint(test_download_file(user_data->url), ==, 1);
}

static void teardown_data(SetupData *data, gconstpointer _user_data) {
	const UserData *user_data = _user_data;
}

static void test_verify_data(SetupData *data, gconstpointer _user_data) {
	const UserData *user_data = _user_data;

	xmi_archive *archive = NULL;
	xmi_archive *archive_copy = NULL;
	unsigned int i,j;

	//read the file
	g_assert_nonnull(archive = xmi_archive_read_from_xml_file(user_data->plain, NULL));

	//some testing of the input and output
	{
		xmi_output *temp = g_ptr_array_index(archive->output, xmi_row_major_array_get_offset(archive->dims, user_data->indices_arr));
		g_assert_cmpfloat(fabs(temp->channels_conv[3][101] - user_data->counts_3_101), <, 0.01);

		g_assert_true(temp->var_red_history[2].atomic_number == 20);
		g_assert_cmpfloat(fabs(temp->var_red_history[2].total_counts - user_data->var_red_history_2_total_counts), <, 10.0);
		g_assert_cmpstr(temp->var_red_history[2].lines[2].line_type, ==, "KM2");
		g_assert_cmpfloat(fabs(temp->var_red_history[2].lines[2].total_counts - user_data->var_red_history_2_lines_2_total_counts), <, 0.1);
		g_assert_cmpfloat(fabs(temp->var_red_history[2].lines[2].interactions[2].counts - user_data->var_red_history_2_lines_2_interactions_2_counts), <, 0.01);
	}
	
	for (i = 0 ; i < archive->output->len; i++) {
		xmi_output *output_copy = NULL;
		xmi_output_copy(g_ptr_array_index(archive->output, i), &output_copy);
		g_assert(xmi_input_validate(output_copy->input) == 0);
		g_assert(xmi_input_compare(((xmi_output *) g_ptr_array_index(archive->output, i))->input, output_copy->input) == 0);
		xmi_output_free(output_copy);
	}

	//make a copy
	archive_copy = xmi_archive_ref(archive);
	g_assert_cmpint(archive->ref_count, ==, 2);
	g_assert_cmpint(archive_copy->ref_count, ==, 2);
	g_assert_true(xmi_archive_equals(archive, archive_copy));
	xmi_archive_unref(archive);
	g_assert_cmpint(archive->ref_count, ==, 1);
	g_assert_cmpint(archive_copy->ref_count, ==, 1);
	xmi_archive_unref(archive_copy);
}

static void test_expected_failures(SetupData *data, gconstpointer _user_data) {
	const UserData *user_data = _user_data;

	// now some tests that are supposed to fail
	GError *error = NULL;
	xmi_archive *archive;
	g_assert_null(archive = xmi_archive_read_from_xml_file("non-existent-file.xmsa", &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);

	g_assert_cmpint(remove_xml_tags(user_data->plain, user_data->copy, "/xmimsim-archive/end_value1"), ==, 1);
	g_assert_null(archive = xmi_archive_read_from_xml_file(user_data->copy, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	g_unlink(user_data->copy);
}


int main(int argc, char *argv[]) {

	//init test
  	g_test_init(&argc, &argv, NULL);
	g_assert_cmpint(test_init(), ==, 1);

	int indices_1D[1] = {1};
	GArray *indices_arr_1D = g_array_sized_new(FALSE, FALSE, sizeof(int), 1);
	g_array_append_vals(indices_arr_1D, indices_1D, 1);

	int indices_2D[2] = {5, 5};
	GArray *indices_arr_2D = g_array_sized_new(FALSE, FALSE, sizeof(int), 2);
	g_array_append_vals(indices_arr_2D, indices_2D, 2);

  	UserData user_data1 = {
		.url = TEST_XMSA_URL_1_OLD,
		.plain = TEST_XMSA_1_OLD,
		.copy = TEST_XMSA_COPY_1_OLD,
		.counts_3_101 = 42013.9,
		.var_red_history_2_total_counts = 1.86431e+06,
		.var_red_history_2_lines_2_total_counts = 87864.5,
		.var_red_history_2_lines_2_interactions_2_counts = 498.023,
		.indices_arr = indices_arr_1D
	};
	g_test_add("/read-xmsa/verify-data/1D/old",
		SetupData,
		&user_data1,
		setup_data,
		test_verify_data,
		teardown_data
		);

  	UserData user_data2 = {
		.url = TEST_XMSA_URL_2_OLD,
		.plain = TEST_XMSA_2_OLD,
		.copy = TEST_XMSA_COPY_2_OLD,
		.counts_3_101 = 23249.5,
		.var_red_history_2_total_counts = 1.24347e+07,
		.var_red_history_2_lines_2_total_counts = 584126,
		.var_red_history_2_lines_2_interactions_2_counts = 1672.38,
		.indices_arr = indices_arr_2D
	};
	g_test_add("/read-xmsa/verify-data/2D/old",
		SetupData,
		&user_data2,
		setup_data,
		test_verify_data,
		teardown_data
		);

  	UserData user_data3 = {
		.url = TEST_XMSA_URL_1_NEW,
		.plain = TEST_XMSA_1_NEW,
		.copy = TEST_XMSA_COPY_1_NEW,
		.counts_3_101 = 41713.8,
		.var_red_history_2_total_counts = 2.16209e+06,
		.var_red_history_2_lines_2_total_counts = 101510,
		.var_red_history_2_lines_2_interactions_2_counts = 787.47,
		.indices_arr = indices_arr_1D
	};
	g_test_add("/read-xmsa/verify-data/1D/new",
		SetupData,
		&user_data3,
		setup_data,
		test_verify_data,
		teardown_data
		);

  	UserData user_data4 = {
		.url = TEST_XMSA_URL_2_NEW,
		.plain = TEST_XMSA_2_NEW,
		.copy = TEST_XMSA_COPY_2_NEW,
		.counts_3_101 = 24822.2,
		.var_red_history_2_total_counts = 1.43601e+07,
		.var_red_history_2_lines_2_total_counts = 674216,
		.var_red_history_2_lines_2_interactions_2_counts = 1956.04,
		.indices_arr = indices_arr_2D
	};
	g_test_add("/read-xmsa/verify-data/2D/new",
		SetupData,
		&user_data4,
		setup_data,
		test_verify_data,
		teardown_data
		);

	g_test_add("/read-xmsa/expected-failures",
		SetupData,
		&user_data4,
		setup_data,
		test_expected_failures,
		teardown_data
		);


	return g_test_run();
}

