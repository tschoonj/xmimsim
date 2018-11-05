#include <config.h>
#include <stdio.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include "xmi_private.h"
#include <glib.h>
#include <gio/gio.h>
#include <math.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  int i, j;

  // init test
  g_assert(test_init() == 1);

  // file should be present already
  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_1, "temp.xmso", 0, -1) == 1);
  xmi_output *output = NULL;
  g_assert(xmi_read_output_xml("temp.xmso", &output, NULL) == 1);
  xmi_output_free(output);
  unlink("temp.xmso");

  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_1, "temp.xmso", 0, 1) == 0);
  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_1, "temp.xmso", 21, -1) == 0);
  //g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_1, "temp", -2, -2) == 0);

  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_1, "temp", -1, -1) == 1);

  xmi_archive *archive = NULL;

  g_assert(xmi_read_archive_xml(TEST_XMSA_1, &archive, NULL) == 1);

  for (i = 0 ; i < 21 ; i++) {
    gchar *filename = g_strdup_printf("temp_%d.xmso", i);
    if (!g_file_test(filename, G_FILE_TEST_EXISTS)) {
      fprintf(stderr, "%s does not exist!", filename);
      return 1;
    }
    g_assert(xmi_read_output_xml(filename, &output, NULL) == 1);
    g_assert_true(xmi_output_equals(output, archive->output[i][0]));
    xmi_output_free(output);
    unlink(filename);
    g_free(filename);
  }
  xmi_archive_free(archive);

  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_2, "temp.xmso", 0, 0) == 1);
  g_assert(xmi_read_output_xml("temp.xmso", &output, NULL) == 1);
  xmi_output_free(output);
  unlink("temp.xmso");
  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_2, "temp.xmso", 10, 10) == 1);
  unlink("temp.xmso");

  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_2, "temp.xmso", 0, -1) == 0);
  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_2, "temp.xmso", 11, 11) == 0);

  g_assert(xmi_xmsa_to_xmso_xslt(TEST_XMSA_2, "temp", -1, -1) == 1);
  g_assert(xmi_read_archive_xml(TEST_XMSA_2, &archive, NULL) == 1);

  for (i = 0 ; i < 11 ; i++) {
    for (j = 0 ; j < 11 ; j++) {
      gchar *filename = g_strdup_printf("temp_%d_%d.xmso", i, j);
      if (!g_file_test(filename, G_FILE_TEST_EXISTS)) {
        fprintf(stderr, "%s does not exist!", filename);
        return 1;
      }
      g_assert(xmi_read_output_xml(filename, &output, NULL) == 1);
      g_assert_true(xmi_output_equals(output, archive->output[i][j]));
      xmi_output_free(output);
      unlink(filename);
      g_free(filename);
    }
  }
  xmi_archive_free(archive);

  return 0;
}
