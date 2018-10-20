#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <glib.h>
#include <gio/gio.h>
#include <math.h>
#include <unistd.h>

int main(int argc, char *argv[]) {

  //init test
  g_assert(test_init() == 1);

  //file should be present already
  g_assert(xmi_xmso_to_csv_xslt(TEST_XMSO, TEST_CSV, 1) == 1);

  //let's read both files in and compare...
  xmi_output *output = NULL;
  g_assert(xmi_read_output_xml(TEST_XMSO, &output, NULL) == 1);

  GFile *file = g_file_new_for_path(TEST_CSV);
  GFileInputStream *file_stream = g_file_read(file, NULL, NULL);
  g_object_unref(file);
  GDataInputStream *data_stream = g_data_input_stream_new(G_INPUT_STREAM(file_stream));
  g_data_input_stream_set_newline_type(data_stream, G_DATA_STREAM_NEWLINE_TYPE_ANY);

  char *line = (char *) 1;
  int nlines = 0;

  while (line) {
    gsize linelen;
    GError *tmp_error = NULL;
    line = g_data_input_stream_read_line(data_stream, &linelen, NULL, &tmp_error);
    g_assert_null(tmp_error);
    if (line == NULL)
	    break;
    if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
      continue;
    }
    gchar **splitted = g_strsplit(line, ",", 0);

    int i;
    for (i = 2 ; i < g_strv_length(splitted) ; i++) {
      double csv_value = g_ascii_strtod(splitted[i], NULL);
      g_assert_cmpfloat(output->channels_conv[i-1][nlines], ==, csv_value);
    }

    g_free(line);
    g_strfreev(splitted);
    nlines++;
  }
  g_object_unref(file_stream);
  g_object_unref(data_stream);

  //now let's try unconvoluted
  g_assert(xmi_xmso_to_csv_xslt(TEST_XMSO, TEST_CSV, 0) == 1);

  file = g_file_new_for_path(TEST_CSV);
  file_stream = g_file_read(file, NULL, NULL);
  g_object_unref(file);
  data_stream = g_data_input_stream_new(G_INPUT_STREAM(file_stream));
  g_data_input_stream_set_newline_type(data_stream, G_DATA_STREAM_NEWLINE_TYPE_ANY);

  line = (char *) 1;
  nlines = 0;

  while (line) {
    gsize linelen;
    GError *tmp_error = NULL;
    line = g_data_input_stream_read_line(data_stream, &linelen, NULL, &tmp_error);
    g_assert_null(tmp_error);
    if (line == NULL)
	    break;
    if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
      continue;
    }
    gchar **splitted = g_strsplit(line, ",", 0);

    int i;
    for (i = 2 ; i < g_strv_length(splitted) ; i++) {
      double csv_value = g_ascii_strtod(splitted[i], NULL);
      g_assert_cmpfloat(output->channels_unconv[i-1][nlines], ==, csv_value);
    }

    g_free(line);
    g_strfreev(splitted);
    nlines++;
  }
  g_object_unref(file_stream);
  g_object_unref(data_stream);

  unlink(TEST_CSV);
  xmi_output_free(output);

  return 0;
}
