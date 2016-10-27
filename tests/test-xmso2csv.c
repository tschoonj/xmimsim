#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>

int main(int argc, char *argv[]) {

	//init test
	g_assert(test_init() == 1);

  //file should be present already
  g_assert(xmi_xmso_to_csv_xslt(TEST_XMSO, TEST_CSV, 1) == 1);

  //let's read both files in and compare...
  struct xmi_output *output = NULL;
  g_assert(xmi_read_output_xml(TEST_XMSO, &output) == 1);

  FILE* filePtr = NULL;
  g_assert((filePtr = fopen(TEST_CSV, "r")) != NULL);
  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen;
  int nlines = 0;

  while ((linelen = getline(&line, &linecap, filePtr)) > 0) {
    gchar **splitted = g_strsplit(line, ",", 0);

    int i;
    for (i = 2 ; i < g_strv_length(splitted) ; i++) {
      double csv_value = g_ascii_strtod(splitted[i], NULL);
      g_assert_cmpfloat(output->channels_conv[i-1][nlines], ==, csv_value);
    }

    free(line);
    line = NULL;
    linecap = 0;
    g_strfreev(splitted);
    nlines++;
  }
  fclose(filePtr);

  //now let's try unconvoluted
  g_assert(xmi_xmso_to_csv_xslt(TEST_XMSO, TEST_CSV, 0) == 1);

  filePtr = NULL;
  g_assert((filePtr = fopen(TEST_CSV, "r")) != NULL);
  line = NULL;
  linecap = 0;
  nlines = 0;

  while ((linelen = getline(&line, &linecap, filePtr)) > 0) {
    gchar **splitted = g_strsplit(line, ",", 0);

    int i;
    for (i = 2 ; i < g_strv_length(splitted) ; i++) {
      double csv_value = g_ascii_strtod(splitted[i], NULL);
      g_assert_cmpfloat(output->channels_unconv[i-1][nlines], ==, csv_value);
    }

    free(line);
    line = NULL;
    linecap = 0;
    g_strfreev(splitted);
    nlines++;
  }
  fclose(filePtr);

  unlink(TEST_CSV);
  xmi_free_output(output);

  return 0;
}
