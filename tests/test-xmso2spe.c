#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>
#include <glib/gstdio.h>

int main(int argc, char *argv[]) {
  xmi_output *output = NULL;
  int i, j;
  struct spe_data *sd = NULL;

  //init test
  g_assert_cmpint(test_init(), ==, 1);

	//download file
  if (g_access(TEST_XMSO, R_OK) != 0)
	  g_assert_cmpint(test_download_file(TEST_XMSO_URL), ==, 1);

  //read in the xmso file
  g_assert_nonnull(output = xmi_output_read_from_xml_file(TEST_XMSO, NULL));

  for (i = output->use_zero_interactions ? 0 : 1 ; i <= output->ninteractions ; i++) {
    //convoluted
    g_assert_cmpint(xmi_xmso_to_spe_xslt(TEST_XMSO, TEST_SPE, 1, i), ==, 1);
    sd = read_spe(TEST_SPE);
    g_assert_nonnull(sd);
    g_assert_cmpint(output->input->detector->nchannels, ==, sd->nchannels);
    //compare channel contents
    for (j = 0 ; j < sd->nchannels ; j++) {
      g_assert_cmpfloat(output->channels_conv[i][j], ==, sd->data[j]);
    }

    free_spe_data(sd);
    g_unlink(TEST_SPE);

    //unconvoluted
    g_assert_cmpint(xmi_xmso_to_spe_xslt(TEST_XMSO, TEST_SPE, 0, i), ==, 1);
    sd = read_spe(TEST_SPE);
    g_assert_nonnull(sd);
    g_assert_cmpint(output->input->detector->nchannels, ==, sd->nchannels);
    //compare channel contents
    for (j = 0 ; j < sd->nchannels ; j++) {
      g_assert_cmpfloat(output->channels_unconv[i][j], ==, sd->data[j]);
    }

    free_spe_data(sd);
    g_unlink(TEST_SPE);
  }
  return 0;
}
