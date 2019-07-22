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
	unsigned int i,j;

	//init test
	g_assert_cmpint(test_init(), ==, 1);

	//download file
	g_assert_cmpint(test_download_file(TEST_XMSA_URL_1), ==, 1);

	//read the file
	g_assert_nonnull(archive = xmi_archive_read_from_xml_file(TEST_XMSA_1, NULL));

	//some testing of the input and output
	{
		int indices[1] = {1};
		GArray *indices_arr = g_array_sized_new(FALSE, FALSE, sizeof(int), 1);
		g_array_append_vals(indices_arr, indices, 1);
		xmi_output *temp = g_ptr_array_index(archive->output, xmi_row_major_array_get_offset(archive->dims, indices_arr));
		g_array_unref(indices_arr);
		g_assert_true(fabs(temp->channels_conv[3][101]) - 42013.9 < 0.01);

		g_assert_true(temp->var_red_history[2].atomic_number == 20);
		g_assert_true(fabs(temp->var_red_history[2].total_counts - 1.86431e+06) < 10.0);
		g_assert_cmpstr(temp->var_red_history[2].lines[2].line_type, ==, "KM2");
		g_assert_true(fabs(temp->var_red_history[2].lines[2].total_counts - 87864.5) < 0.1);
		g_assert_true(fabs(temp->var_red_history[2].lines[2].interactions[2].counts - 498.023) < 0.01);
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

	//download file
	g_assert(test_download_file(TEST_XMSA_URL_2) == 1);

	//read the file
	g_assert_nonnull(archive = xmi_archive_read_from_xml_file(TEST_XMSA_2, NULL));

	//some testing of the input and output
	{
		int indices[2] = {5, 5};
		GArray *indices_arr = g_array_sized_new(FALSE, FALSE, sizeof(int), 2);
		g_array_append_vals(indices_arr, indices, 2);
		xmi_output *temp = g_ptr_array_index(archive->output, xmi_row_major_array_get_offset(archive->dims, indices_arr));
		g_array_unref(indices_arr);
		g_assert_true(fabs(temp->channels_conv[3][101]) - 23249.5 < 0.01);

		g_assert_true(temp->var_red_history[2].atomic_number == 20);
		g_assert_true(fabs(temp->var_red_history[2].total_counts - 1.24347e+07) < 10.0);
		g_assert_cmpstr(temp->var_red_history[2].lines[2].line_type, ==, "KM2");
		g_assert_true(fabs(temp->var_red_history[2].lines[2].total_counts - 584126) < 0.1);
		g_assert_true(fabs(temp->var_red_history[2].lines[2].interactions[2].counts - 1672.38) < 0.01);
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

	// now some tests that are supposed to fail
	GError *error = NULL;
	g_assert_null(archive = xmi_archive_read_from_xml_file("non-existent-file.xmsa", &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);

	// bad xpath1 test: enable when implemented
	/*g_assert(replace_xml_tag(TEST_XMSA_1, TEST_XMSA_COPY_1, "/xmimsim-archive/xpath1", "hsdhodhoosda") == 1);
	g_assert(xmi_read_archive_xml(TEST_XMSA_COPY_1, &archive, &error) == 0);
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	unlink(TEST_XMSA_COPY_1);*/

	g_assert(remove_xml_tags(TEST_XMSA_1, TEST_XMSA_COPY_1, "/xmimsim-archive/end_value1") == 1);
	g_assert_null(archive = xmi_archive_read_from_xml_file(TEST_XMSA_COPY_1, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	unlink(TEST_XMSA_COPY_1);
	return 0;
}

