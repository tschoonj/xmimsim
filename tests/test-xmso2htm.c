#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <libxml/HTMLparser.h>
#include <gio/gio.h>
#include <glib/gstdio.h>

typedef struct {
  gchar *xmso_file;
  gchar *htm_file;
} SetupData;

typedef struct {
  gchar *prefix;
  int use_convoluted;
  int use_executable;
} UserData;

static void setup_data(SetupData *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  data->xmso_file = g_strdup_printf("%s.xmso", user_data->prefix);
  data->htm_file = g_strdup_printf("%s.htm", user_data->prefix);

  GFile *xmso_orig_gfile = g_file_new_for_path(TEST_XMSO);
  GFile *xmso_copy_gfile = g_file_new_for_path(data->xmso_file);

  g_assert_true(g_file_copy(xmso_orig_gfile, xmso_copy_gfile, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL));

  g_object_unref(xmso_orig_gfile);
  g_object_unref(xmso_copy_gfile);
}

static void teardown_data(SetupData *data, gconstpointer user_data) {
  g_assert_cmpint(g_unlink(data->xmso_file), ==, 0);
  g_assert_cmpint(g_unlink(data->htm_file), ==, 0);

  g_free(data->xmso_file);
  g_free(data->htm_file);
}

static void test_wrapper(SetupData *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;

  if (user_data->use_executable) {
    GError *error = NULL;
    GSubprocess *process = NULL;
    if (user_data->use_convoluted) {
      process = g_subprocess_new(
        G_SUBPROCESS_FLAGS_STDERR_MERGE | G_SUBPROCESS_FLAGS_STDOUT_PIPE,
        &error,
        g_getenv("XMSO2HTM_EXEC"),
        data->xmso_file,
        data->htm_file,
        NULL);
    }
    else {
      process = g_subprocess_new(
        G_SUBPROCESS_FLAGS_STDERR_MERGE | G_SUBPROCESS_FLAGS_STDOUT_PIPE,
        &error,
        g_getenv("XMSO2HTM_EXEC"),
        "--unconvoluted",
        data->xmso_file,
        data->htm_file,
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
    g_assert_cmpint(xmi_xmso_to_htm_xslt(data->xmso_file, data->htm_file, user_data->use_convoluted), ==, 1);
  }

  htmlDocPtr ptr = htmlReadFile(data->htm_file, NULL, HTML_PARSE_PEDANTIC);
  g_assert(ptr != NULL);
  xmlFreeDoc(ptr);
}

int main(int argc, char *argv[]) {

  //init test
  g_test_init(&argc, &argv, NULL);
  g_assert_cmpint(test_init(), ==, 1);

  //download file
  if (g_access(TEST_XMSO, R_OK) != 0)
	  g_assert_cmpint(test_download_file(TEST_XMSO_URL), ==, 1);

  {
    UserData user_data = {.prefix = "test-xmso2htm\u03b1", .use_executable = 0, .use_convoluted = 1};
	  g_test_add("/xmso2htm/api/convoluted",
			 SetupData,
			&user_data,
			setup_data,
			test_wrapper,
			teardown_data
			);
  }

  {
    UserData user_data = {.prefix = "test-xmso2htm\u03b2", .use_executable = 0, .use_convoluted = 0};
	  g_test_add("/xmso2htm/api/unconvoluted",
			 SetupData,
			&user_data,
			setup_data,
			test_wrapper,
			teardown_data
			);
  }

  {
    UserData user_data = {.prefix = "test-xmso2htm\u03b3", .use_executable = 1, .use_convoluted = 1};
	  g_test_add("/xmso2htm/executable/convoluted",
			 SetupData,
			&user_data,
			setup_data,
			test_wrapper,
			teardown_data
			);
  }

  {
    UserData user_data = {.prefix = "test-xmso2htm\u03b4", .use_executable = 1, .use_convoluted = 0};
	  g_test_add("/xmso2htm/executable/unconvoluted",
			 SetupData,
			&user_data,
			setup_data,
			test_wrapper,
			teardown_data
			);
  }

  return g_test_run();
}
