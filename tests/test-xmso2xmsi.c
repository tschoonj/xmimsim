#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <glib.h>
#include <gio/gio.h>
#include <math.h>
#include <glib/gstdio.h>

xmi_output *output;

typedef struct {
  gchar *xmso_file;
  gchar *xmsi_file;
  gchar *xmso_file_new;
} SetupData;

typedef struct {
  gchar *prefix;
  int use_executable;
  int change_outputfile;
} UserData;

static void setup_data(SetupData *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  data->xmso_file = g_strdup_printf("%s.xmso", user_data->prefix);
  data->xmsi_file = g_strdup_printf("%s.xmsi", user_data->prefix);
  if (user_data->change_outputfile)
    data->xmso_file_new = g_strdup_printf("%s-new.xmsi", user_data->prefix);

  GFile *xmso_orig_gfile = g_file_new_for_path(TEST_XMSO);
  GFile *xmso_copy_gfile = g_file_new_for_path(data->xmso_file);

  g_assert_true(g_file_copy(xmso_orig_gfile, xmso_copy_gfile, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL));

  g_object_unref(xmso_orig_gfile);
  g_object_unref(xmso_copy_gfile);
}

static void teardown_data(SetupData *data, gconstpointer user_data) {
  g_assert_cmpint(g_unlink(data->xmso_file), ==, 0);
  g_assert_cmpint(g_unlink(data->xmsi_file), ==, 0);

  g_free(data->xmso_file);
  g_free(data->xmsi_file);
  g_free(data->xmso_file_new);
}

static void test_wrapper(SetupData *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;

  if (user_data->use_executable) {
    GError *error = NULL;
    GSubprocess *process = NULL;
    if (user_data->change_outputfile) {
      process = g_subprocess_new(
        G_SUBPROCESS_FLAGS_STDERR_MERGE | G_SUBPROCESS_FLAGS_STDOUT_PIPE,
        &error,
        g_getenv("XMSO2XMSI_EXEC"),
        g_strdup_printf("--outputfile=%s", data->xmso_file_new),
        data->xmso_file,
        data->xmsi_file,
        NULL);
    }
    else {
      process = g_subprocess_new(
        G_SUBPROCESS_FLAGS_STDERR_MERGE | G_SUBPROCESS_FLAGS_STDOUT_PIPE,
        &error,
        g_getenv("XMSO2XMSI_EXEC"),
        data->xmso_file,
        data->xmsi_file,
        NULL);
    }
    if (error) {
      g_critical("g_subprocess_new error: %s", error->message);
      exit(1);
    }
    g_assert_nonnull(process);
    g_assert_true(g_subprocess_wait(process, NULL, NULL));
    g_assert_true(g_subprocess_get_successful(process));
  }
  else {
    g_assert_cmpint(xmi_xmso_to_xmsi_xslt(data->xmso_file, data->xmsi_file, data->xmso_file_new), ==, 1);
  }

  if (user_data->change_outputfile) {
    xmi_input *input;
    g_assert_nonnull(input = xmi_input_read_from_xml_file(data->xmsi_file, NULL));
    g_assert(xmi_input_compare(input, output->input) == XMI_INPUT_GENERAL);
    g_assert_cmpstr(input->general->outputfile, ==, data->xmso_file_new);
    xmi_input_free(input);
  }
  else {
    xmi_input *input = NULL;
    g_assert_nonnull(input = xmi_input_read_from_xml_file(data->xmsi_file, NULL));
    g_assert(xmi_input_compare(input, output->input) == 0);
    xmi_input_free(input);
  }

}

int main(int argc, char *argv[]) {

  //init test
  g_test_init(&argc, &argv, NULL);
  g_assert_cmpint(test_init(), ==, 1);

  //download file
  if (g_access(TEST_XMSO, R_OK) != 0)
	  g_assert_cmpint(test_download_file(TEST_XMSO_URL), ==, 1);

  g_assert_nonnull(output = xmi_output_read_from_xml_file(TEST_XMSO, NULL));

  UserData user_data1 = {.prefix = "test-xmso2xmsi\u03b1", .use_executable = 0, .change_outputfile = 1};
	g_test_add("/xmso2xmsi/api/change-outputfile",
		SetupData,
		&user_data1,
		setup_data,
		test_wrapper,
		teardown_data
		);

  UserData user_data2 = {.prefix = "test-xmso2xmsi\u03b2", .use_executable = 0, .change_outputfile = 0};
	g_test_add("/xmso2xmsi/api/keep-outputfile",
		SetupData,
		&user_data2,
		setup_data,
		test_wrapper,
		teardown_data
		);

  UserData user_data3 = {.prefix = "test-xmso2xmsi\u03b3", .use_executable = 1, .change_outputfile = 1};
	g_test_add("/xmso2xmsi/executable/change-outputfile",
		SetupData,
		&user_data3,
		setup_data,
		test_wrapper,
		teardown_data
		);

  UserData user_data4 = {.prefix = "test-xmso2xmsi\u03b4", .use_executable = 1, .change_outputfile = 0};
	g_test_add("/xmso2xmsi/executable/keep-outputfile",
		SetupData,
		&user_data4,
		setup_data,
		test_wrapper,
		teardown_data
		);

  int rv = g_test_run();
  
  xmi_output_free(output);

  return rv;
}
