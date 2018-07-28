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

  // init test
  g_assert(test_init() == 1);

  // file should be present already
  // 1. keep outputfile as is
  g_assert(xmi_xmso_to_xmsi_xslt(TEST_XMSO, "temp.xmsi", NULL) == 1);

  struct xmi_output *output = NULL;
  g_assert(xmi_read_output_xml(TEST_XMSO, &output, NULL) == 1);

  struct xmi_input *input = NULL;
  g_assert(xmi_read_input_xml("temp.xmsi", &input, NULL) == 1);

  g_assert(xmi_compare_input(input, output->input) == 0);

  xmi_free_input(input);

  // 2. change outputfile
  g_assert(xmi_xmso_to_xmsi_xslt(TEST_XMSO, "temp.xmsi", "new-outputfile.xmso") == 1);
  
  g_assert(xmi_read_input_xml("temp.xmsi", &input, NULL) == 1);

  g_assert(xmi_compare_input(input, output->input) == XMI_CONFLICT_GENERAL);
 
  g_assert_cmpstr(input->general->outputfile, ==, "new-outputfile.xmso");

  xmi_free_input(input);
  xmi_free_output(output);
  unlink("temp.xmsi");

  return 0;
}
