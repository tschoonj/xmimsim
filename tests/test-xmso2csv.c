#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <glib.h>
#include <gio/gio.h>
#include <math.h>
#include <unistd.h>
#include <glib/gstdio.h>

typedef struct {
  xmi_output *output;
  gchar *xmso_file;
  gchar *csv_file;
} SetupData;

static void setup_data(SetupData *data, gconstpointer user_data) {
  data->xmso_file = g_strdup_printf("%s.xmso", user_data);
  data->csv_file = g_strdup_printf("%s.csv", user_data);

  GFile *xmso_orig_gfile = g_file_new_for_path(TEST_XMSO);
  GFile *xmso_copy_gfile = g_file_new_for_path(data->xmso_file);

  g_assert_true(g_file_copy(xmso_orig_gfile, xmso_copy_gfile, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL));

  g_object_unref(xmso_orig_gfile);
  g_object_unref(xmso_copy_gfile);

  g_assert_nonnull(data->output = xmi_output_read_from_xml_file(data->xmso_file, NULL));
}

static void teardown_data(SetupData *data, gconstpointer user_data) {
  g_assert_cmpint(g_unlink(data->xmso_file), ==, 0);
  g_assert_cmpint(g_unlink(data->csv_file), ==, 0);

  xmi_output_free(data->output);
  g_free(data->xmso_file);
  g_free(data->csv_file);
}

static void test_api_convoluted(SetupData *data, gconstpointer user_data) {
  g_assert_cmpint(xmi_xmso_to_csv_xslt(data->xmso_file, data->csv_file, 1), ==, 1);

  test_compare_channels_and_csv(data->output->input->detector->nchannels, data->output->channels_conv, data->csv_file);
}

static void test_api_unconvoluted(SetupData *data, gconstpointer user_data) {
  g_assert_cmpint(xmi_xmso_to_csv_xslt(data->xmso_file, data->csv_file, 0), ==, 1);

  test_compare_channels_and_csv(data->output->input->detector->nchannels, data->output->channels_unconv, data->csv_file);
}

static void test_executable_convoluted(SetupData *data, gconstpointer user_data) {
  GError *error = NULL;
  GSubprocess *process = g_subprocess_new(G_SUBPROCESS_FLAGS_STDERR_MERGE | G_SUBPROCESS_FLAGS_STDOUT_PIPE, &error, g_getenv("XMSO2CSV_EXEC"), data->xmso_file, data->csv_file, NULL);
  if (error) {
    g_critical("g_subprocess_new error: %s", error->message);
    exit(1);
  }
  g_assert_nonnull(process);
  g_assert_true(g_subprocess_wait(process, NULL, NULL));
  g_assert_true(g_subprocess_get_successful(process));

  test_compare_channels_and_csv(data->output->input->detector->nchannels, data->output->channels_conv, data->csv_file);
}

static void test_executable_unconvoluted(SetupData *data, gconstpointer user_data) {
  GError *error = NULL;
  GSubprocess *process = g_subprocess_new(G_SUBPROCESS_FLAGS_STDERR_MERGE | G_SUBPROCESS_FLAGS_STDOUT_PIPE, &error, g_getenv("XMSO2CSV_EXEC"), "-u", data->xmso_file, data->csv_file, NULL);
  if (error) {
    g_critical("g_subprocess_new error: %s", error->message);
    exit(1);
  }
  g_assert_nonnull(process);
  g_assert_true(g_subprocess_wait(process, NULL, NULL));
  g_assert_true(g_subprocess_get_successful(process));

  test_compare_channels_and_csv(data->output->input->detector->nchannels, data->output->channels_unconv, data->csv_file);
}

int main(int argc, char *argv[]) {

  //init test
  g_test_init(&argc, &argv, NULL);
  g_assert_cmpint(test_init(), ==, 1);

  //download file
  if (g_access(TEST_XMSO, R_OK) != 0)
	  g_assert_cmpint(test_download_file(TEST_XMSO_URL), ==, 1);

	g_test_add("/xmso2csv/api/convoluted",
			SetupData,
			"test-xmso2csv\u03b1",
			setup_data,
			test_api_convoluted,
			teardown_data
			);

	g_test_add("/xmso2csv/api/unconvoluted",
			SetupData,
			"test-xmso2csv\u03b2",
			setup_data,
			test_api_unconvoluted,
			teardown_data
			);

	g_test_add("/xmso2csv/executable/convoluted",
			SetupData,
			"test-xmso2csv\u03b3",
			setup_data,
			test_executable_convoluted,
			teardown_data
			);

	g_test_add("/xmso2csv/executable/unconvoluted",
			SetupData,
			"test-xmso2csv\u03b4",
			setup_data,
			test_executable_unconvoluted,
			teardown_data
			);

  return g_test_run();
}
