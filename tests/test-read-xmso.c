#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>



int main(int argc, char *argv[]) {
	xmi_output *output = NULL;
	xmi_output *output_copy = NULL;

	//init test
	g_assert(test_init() == 1);

	//download file
	g_assert(test_download_file(TEST_XMSO_URL) == 1);

	//read the file
	g_assert_nonnull(output = xmi_output_read_from_xml_file(TEST_XMSO, NULL));

	//validate the input
	g_assert(xmi_input_validate(output->input) == 0);

	//make a copy
	xmi_output_copy(output, &output_copy);
	g_assert(xmi_input_validate(output_copy->input) == 0);
	g_assert(xmi_input_compare(output->input, output_copy->input) == 0);
	g_assert_true(xmi_output_equals(output, output_copy));

	xmi_output_free(output_copy);

	// print history from GHashTable
	GHashTable *history = xmi_output_get_history(output);
	GHashTableIter iter1;
	g_hash_table_iter_init(&iter1, history);
	gpointer Z;
	xmi_history_element *element;
	while (g_hash_table_iter_next(&iter1, &Z, (gpointer *) &element)) {
		fprintf(stderr, "Z: %d -> %g\n", GPOINTER_TO_INT(Z), element->total_counts);	
		GHashTableIter iter2;
		gchar *line;
		xmi_history_element_line *element_line;
		g_hash_table_iter_init(&iter2, element->lines);
		while (g_hash_table_iter_next(&iter2, (gpointer *) &line, (gpointer *) &element_line)) {
			fprintf(stderr, "\tline: %s -> %g -> %g keV\n",  line, element_line->total_counts, element_line->energy);
			int interaction;
			for (interaction = 0 ; interaction < element_line->interactions->len ; interaction++) {
				fprintf(stderr, "\t\tinteraction: %d -> %g\n", interaction, g_array_index(element_line->interactions, double, interaction));
			}
		}
	}
	// Cu-KL3
	// a.get_history()[29].lines['KL3'].interactions
	// [0.0, 341906.0, 80777.5, 3453.95, 142.371]
	g_assert(fabs(g_array_index(((xmi_history_element_line *) g_hash_table_lookup(((xmi_history_element *) g_hash_table_lookup(history, GINT_TO_POINTER(29)))->lines, "KL3"))->interactions, double, 2) - 80777.5) < 1E-6);

	g_hash_table_destroy(history);

	xmi_output_free(output);

	// now some tests that are supposed to fail
	GError *error = NULL;
	g_assert_null(output = xmi_output_read_from_xml_file("non-existent-file.xmso", &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);

	g_assert(replace_xml_tag(TEST_XMSO, TEST_XMSO_COPY, "/xmimsim-results/spectrum_conv/channel[1]/counts[1]", "hsdhodhoosda") == 1);
	g_assert_null(output = xmi_output_read_from_xml_file(TEST_XMSO_COPY, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	unlink(TEST_XMSO_COPY);

	g_assert(remove_xml_tags(TEST_XMSO, TEST_XMSO_COPY, "/xmimsim-results/brute_force_history") == 1);
	g_assert_null(output = xmi_output_read_from_xml_file(TEST_XMSO_COPY, &error));
	g_assert_true(g_error_matches(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_XML));
	fprintf(stdout, "message: %s\n", error->message);
	g_clear_error(&error);
	unlink(TEST_XMSO_COPY);
	return 0;
}
