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

int main(int argc, char *argv[]) {

  //init test
  g_assert(test_init() == 1);

	//download file
  if (g_access(TEST_XMSO, R_OK) != 0)
	  g_assert_cmpint(test_download_file(TEST_XMSO_URL), ==, 1);

  g_assert_cmpint(xmi_xmso_to_csv_xslt(TEST_XMSO, TEST_CSV, 1), ==, 1);

  //let's read both files in and compare...
  xmi_output *output = NULL;
  g_assert_nonnull(output = xmi_output_read_from_xml_file(TEST_XMSO, NULL));

  test_compare_channels_and_csv(output->channels_conv, TEST_CSV);

  //now let's try unconvoluted
  g_assert_cmpint(xmi_xmso_to_csv_xslt(TEST_XMSO, TEST_CSV, 0), ==, 1);

  test_compare_channels_and_csv(output->channels_unconv, TEST_CSV);

  g_unlink(TEST_CSV);
  xmi_output_free(output);

  return 0;
}
