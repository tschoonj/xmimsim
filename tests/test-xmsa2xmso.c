#include <config.h>
#include <stdio.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <glib.h>
#include <gio/gio.h>
#include <math.h>
#include <glib/gstdio.h>

static xmi_archive *archive_1;
static xmi_archive *archive_2;

typedef struct xmsa{
  gchar *xmsa_file;
  gchar *xmso_file;
} SetupDataSingle;

typedef struct {
  gchar *xmsa_file;
  gchar *xmso_prefix;
} SetupDataMulti;

typedef struct {
  gchar *xmso_file_or_prefix;
  gboolean use_executable;
} UserData;

static int xmsa2xmso_execute_wrapper(gchar *xmsa_file, gchar *xmso_file_or_prefix, int index1, int index2, gboolean use_executable) {
  if (use_executable) {
    GPtrArray *argv = g_ptr_array_new_with_free_func(g_free);
    g_ptr_array_add(argv, g_strdup(g_getenv("XMSA2XMSO_EXEC")));
    if (index1 < 0 && index2 < 0) {
      g_ptr_array_add(argv, g_strdup("--all"));
    }
    else {
      g_ptr_array_add(argv, g_strdup_printf("--step1=%d", index1));
      g_ptr_array_add(argv, g_strdup_printf("--step2=%d", index2));
    }
    g_ptr_array_add(argv, g_strdup(xmsa_file));
    g_ptr_array_add(argv, g_strdup(xmso_file_or_prefix));
    g_ptr_array_add(argv, NULL);

    GError *error = NULL;
    GSubprocess *process = g_subprocess_newv((const gchar * const *) argv->pdata, G_SUBPROCESS_FLAGS_STDERR_MERGE | G_SUBPROCESS_FLAGS_STDOUT_PIPE, &error);
    if (error) {
      g_critical("g_subprocess_new error: %s", error->message);
      exit(1);
    }
    g_assert_nonnull(process);
    g_assert_true(g_subprocess_wait(process, NULL, NULL));
    int rv = g_subprocess_get_successful(process);
    g_object_unref(process);
    g_ptr_array_unref(argv);
    return rv;
  }
  else {
    return xmi_xmsa_to_xmso_xslt(xmsa_file, xmso_file_or_prefix, index1, index2);
  }
}

static void setup_data_single_1D(SetupDataSingle *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  data->xmsa_file = g_strdup_printf("%s.xmsa", user_data->xmso_file_or_prefix);
  data->xmso_file = g_strdup_printf("%s.xmso", user_data->xmso_file_or_prefix);

  GFile *xmsa_orig_gfile = g_file_new_for_path(TEST_XMSA_1);
  GFile *xmsa_copy_gfile = g_file_new_for_path(data->xmsa_file);

  g_assert_true(g_file_copy(xmsa_orig_gfile, xmsa_copy_gfile, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL));

  g_object_unref(xmsa_orig_gfile);
  g_object_unref(xmsa_copy_gfile);
}

static void teardown_data_single(SetupDataSingle *data, gconstpointer _user_data) {
  g_assert_cmpint(g_unlink(data->xmsa_file), ==, 0);
  g_assert_cmpint(g_unlink(data->xmso_file), !=, 0);
  g_free(data->xmso_file);
  g_free(data->xmsa_file);
}

static void test_single_1D_bad(SetupDataSingle *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  g_assert_cmpint(xmsa2xmso_execute_wrapper(data->xmsa_file, data->xmso_file,  0,  1, user_data->use_executable), ==, 0);
  g_assert_cmpint(xmsa2xmso_execute_wrapper(data->xmsa_file, data->xmso_file, 21, -1, user_data->use_executable), ==, 0);
}

static void test_single_1D_good(SetupDataSingle *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  int indices[3] = {0, 20, g_test_rand_int_range(1, 20)};

  for (int i = 0 ; i < G_N_ELEMENTS(indices) ; i++) {
    int _index = indices[i];
    g_assert_cmpint(xmsa2xmso_execute_wrapper(data->xmsa_file, data->xmso_file, _index, 0, user_data->use_executable), ==, 1);
    xmi_output *output = NULL;
    g_assert_nonnull(output = xmi_output_read_from_xml_file(data->xmso_file, NULL));
    g_assert_true(xmi_output_equals(output, g_ptr_array_index(archive_1->output, _index)));
    xmi_output_free(output);
    g_assert_cmpint(g_unlink(data->xmso_file), ==, 0);
  }
}

static void setup_data_multi_1D(SetupDataMulti *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  data->xmsa_file = g_strdup_printf("%s.xmsa", user_data->xmso_file_or_prefix);
  data->xmso_prefix = g_strdup(user_data->xmso_file_or_prefix);

  GFile *xmsa_orig_gfile = g_file_new_for_path(TEST_XMSA_1);
  GFile *xmsa_copy_gfile = g_file_new_for_path(data->xmsa_file);

  g_assert_true(g_file_copy(xmsa_orig_gfile, xmsa_copy_gfile, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL));

  g_object_unref(xmsa_orig_gfile);
  g_object_unref(xmsa_copy_gfile);
}

static void teardown_data_multi(SetupDataMulti *data, gconstpointer _user_data) {
  g_assert_cmpint(g_unlink(data->xmsa_file), ==, 0);
  g_free(data->xmso_prefix);
  g_free(data->xmsa_file);
}

static void test_multi_1D(SetupDataMulti *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  g_assert_cmpint(xmsa2xmso_execute_wrapper(data->xmsa_file, data->xmso_prefix, -1, -1, user_data->use_executable), ==, 1);

  for (int i = 0 ; i < 21 ; i++) {
    gchar *xmso_file = g_strdup_printf("%s_%d.xmso", data->xmso_prefix, i);
    xmi_output *output = NULL;
    g_assert_nonnull(output = xmi_output_read_from_xml_file(xmso_file, NULL));
    g_assert_true(xmi_output_equals(output, g_ptr_array_index(archive_1->output, i)));
    xmi_output_free(output);
    g_assert_cmpint(g_unlink(xmso_file), ==, 0);
    g_free(xmso_file);
  }
}

static void setup_data_single_2D(SetupDataSingle *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  data->xmsa_file = g_strdup_printf("%s.xmsa", user_data->xmso_file_or_prefix);
  data->xmso_file = g_strdup_printf("%s.xmso", user_data->xmso_file_or_prefix);

  GFile *xmsa_orig_gfile = g_file_new_for_path(TEST_XMSA_2);
  GFile *xmsa_copy_gfile = g_file_new_for_path(data->xmsa_file);

  g_assert_true(g_file_copy(xmsa_orig_gfile, xmsa_copy_gfile, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL));

  g_object_unref(xmsa_orig_gfile);
  g_object_unref(xmsa_copy_gfile);
}

static void test_single_2D_bad(SetupDataSingle *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  g_assert_cmpint(xmsa2xmso_execute_wrapper(data->xmsa_file, data->xmso_file, 0, -1,  user_data->use_executable), ==, 0);
  g_assert_cmpint(xmsa2xmso_execute_wrapper(data->xmsa_file, data->xmso_file, 11, 11, user_data->use_executable), ==, 0);
}

static void test_single_2D_good(SetupDataSingle *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  int indices[3][2] = {
    {0, 0},
    {10, 10},
    {g_test_rand_int_range(1, 10), g_test_rand_int_range(1, 10)}
  };

  for (int i = 0 ; i < G_N_ELEMENTS(indices) ; i++) {
    int *_index_pair = indices[i];
    g_debug("_index_pair: %d %d", _index_pair[0], _index_pair[1]);
    GArray *indices_arr = g_array_sized_new(FALSE, FALSE, sizeof(int), 2);
    g_array_append_vals(indices_arr, _index_pair, 2);
    g_assert_cmpint(xmsa2xmso_execute_wrapper(data->xmsa_file, data->xmso_file, _index_pair[0], _index_pair[1], user_data->use_executable), ==, 1);
    xmi_output *output = NULL;
    g_assert_nonnull(output = xmi_output_read_from_xml_file(data->xmso_file, NULL));
    g_assert_true(xmi_output_equals(output, g_ptr_array_index(archive_2->output, xmi_row_major_array_get_offset(archive_2->dims, indices_arr))));
    xmi_output_free(output);
    g_assert_cmpint(g_unlink(data->xmso_file), ==, 0);
    g_array_unref(indices_arr);
  }
}

static void setup_data_multi_2D(SetupDataMulti *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  data->xmsa_file = g_strdup_printf("%s.xmsa", user_data->xmso_file_or_prefix);
  data->xmso_prefix = g_strdup(user_data->xmso_file_or_prefix);

  GFile *xmsa_orig_gfile = g_file_new_for_path(TEST_XMSA_2);
  GFile *xmsa_copy_gfile = g_file_new_for_path(data->xmsa_file);

  g_assert_true(g_file_copy(xmsa_orig_gfile, xmsa_copy_gfile, G_FILE_COPY_OVERWRITE, NULL, NULL, NULL, NULL));

  g_object_unref(xmsa_orig_gfile);
  g_object_unref(xmsa_copy_gfile);
}

static void test_multi_2D(SetupDataMulti *data, gconstpointer _user_data) {
  const UserData *user_data = _user_data;
  g_assert_cmpint(xmsa2xmso_execute_wrapper(data->xmsa_file, data->xmso_prefix, -1, -1, user_data->use_executable), ==, 1);

  for (int i = 0 ; i < 11 ; i++) {
    for (int j = 0 ; j < 11 ; j++) {
      int indices[2] = {i, j};
      GArray *indices_arr = g_array_sized_new(FALSE, FALSE, sizeof(int), 2);
      g_array_append_vals(indices_arr, indices, 2);
      gchar *xmso_file = g_strdup_printf("%s_%d_%d.xmso", data->xmso_prefix, i, j);
      xmi_output *output = NULL;
      g_assert_nonnull(output = xmi_output_read_from_xml_file(xmso_file, NULL));
      g_assert_true(xmi_output_equals(output, g_ptr_array_index(archive_2->output, xmi_row_major_array_get_offset(archive_2->dims, indices_arr))));
      g_array_unref(indices_arr);
      xmi_output_free(output);
      g_assert_cmpint(g_unlink(xmso_file), ==, 0);
      g_free(xmso_file);
    }
  }
}

int main(int argc, char *argv[]) {
  //init test
  g_test_init(&argc, &argv, NULL);
  g_assert_cmpint(test_init(), ==, 1);

	//download files
  if (g_access(TEST_XMSA_1, R_OK) != 0)
	  g_assert_cmpint(test_download_file(TEST_XMSA_URL_1), ==, 1);

  g_assert_nonnull(archive_1 = xmi_archive_read_from_xml_file(TEST_XMSA_1, NULL));

  if (g_access(TEST_XMSA_2, R_OK) != 0)
	  g_assert_cmpint(test_download_file(TEST_XMSA_URL_2), ==, 1);

  g_assert_nonnull(archive_2 = xmi_archive_read_from_xml_file(TEST_XMSA_2, NULL));

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b1", .use_executable = FALSE};
	  g_test_add("/xmsa2xmso/single/1D/api/bad",
		  SetupDataSingle,
			&user_data,
			setup_data_single_1D,
			test_single_1D_bad,
			teardown_data_single
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b2", .use_executable = FALSE};
	  g_test_add("/xmsa2xmso/single/1D/api/good",
			SetupDataSingle,
      &user_data,
			setup_data_single_1D,
			test_single_1D_good,
			teardown_data_single
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b3", .use_executable = FALSE};
	  g_test_add("/xmsa2xmso/multi/1D/api",
			SetupDataMulti,
      &user_data,
			setup_data_multi_1D,
			test_multi_1D,
			teardown_data_multi
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b4", .use_executable = FALSE};
	  g_test_add("/xmsa2xmso/single/2D/api/bad",
			SetupDataSingle,
      &user_data,
			setup_data_single_2D,
			test_single_2D_bad,
			teardown_data_single
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b5", .use_executable = FALSE};
	  g_test_add("/xmsa2xmso/single/2D/api/good",
			SetupDataSingle,
      &user_data,
			setup_data_single_2D,
			test_single_2D_good,
			teardown_data_single
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b6", .use_executable = FALSE};
  	g_test_add("/xmsa2xmso/multi/2D/api",
			SetupDataMulti,
      &user_data,
			setup_data_multi_2D,
			test_multi_2D,
			teardown_data_multi
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b7", .use_executable = TRUE};
	  g_test_add("/xmsa2xmso/single/1D/executable/bad",
		  SetupDataSingle,
			&user_data,
			setup_data_single_1D,
			test_single_1D_bad,
			teardown_data_single
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b8", .use_executable = TRUE};
	  g_test_add("/xmsa2xmso/single/1D/executable/good",
			SetupDataSingle,
      &user_data,
			setup_data_single_1D,
			test_single_1D_good,
			teardown_data_single
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03b9", .use_executable = TRUE};
	  g_test_add("/xmsa2xmso/multi/1D/executable",
			SetupDataMulti,
      &user_data,
			setup_data_multi_1D,
			test_multi_1D,
			teardown_data_multi
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03c0", .use_executable = TRUE};
	  g_test_add("/xmsa2xmso/single/2D/executable/bad",
			SetupDataSingle,
      &user_data,
			setup_data_single_2D,
			test_single_2D_bad,
			teardown_data_single
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03c1", .use_executable = TRUE};
	  g_test_add("/xmsa2xmso/single/2D/executable/good",
			SetupDataSingle,
      &user_data,
			setup_data_single_2D,
			test_single_2D_good,
			teardown_data_single
			);
  }

  {
    UserData user_data = {.xmso_file_or_prefix = "test-xmsa2xmso\u03c2", .use_executable = TRUE};
  	g_test_add("/xmsa2xmso/multi/2D/executable",
			SetupDataMulti,
      &user_data,
			setup_data_multi_2D,
			test_multi_2D,
			teardown_data_multi
			);
  }
  
  int rv = g_test_run();

  xmi_archive_unref(archive_1);
  xmi_archive_unref(archive_2);

  return rv;
}
