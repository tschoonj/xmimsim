#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>
#include <assert.h>

int main(int argc, char *argv[]) {
  struct xmi_output *output = NULL;
  int i, j;
  struct spe_data *sd = NULL;

  //init test
  assert(test_init() == 1);

  //read in the xmso file
  assert(xmi_read_output_xml(TEST_XMSO, &output) == 1);

  for (i = output->use_zero_interactions ? 0 : 1 ; i <= output->ninteractions ; i++) {
    //convoluted
    assert(xmi_xmso_to_spe_xslt(TEST_XMSO, TEST_SPE, 1, i) == 1);
    sd = read_spe(TEST_SPE);
    assert(sd != NULL);
    assert(output->input->detector->nchannels == sd->nchannels);
    //compare channel contents
    for (j = 0 ; j < sd->nchannels ; j++) {
      assert(output->channels_conv[i][j] == sd->data[j]);
    }

    free_spe_data(sd);
    unlink(TEST_SPE);

    //unconvoluted
    assert(xmi_xmso_to_spe_xslt(TEST_XMSO, TEST_SPE, 0, i) == 1);
    sd = read_spe(TEST_SPE);
    assert(sd != NULL);
    assert(output->input->detector->nchannels == sd->nchannels);
    //compare channel contents
    for (j = 0 ; j < sd->nchannels ; j++) {
      assert(output->channels_unconv[i][j] == sd->data[j]);
    }

    free_spe_data(sd);
    unlink(TEST_SPE);
  }
  return 0;
}
