#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>
#include <libxml/HTMLparser.h>

int main(int argc, char *argv[]) {

  //init test
  g_assert(test_init() == 1);

  //file should be present already
  g_assert(xmi_xmso_to_htm_xslt(TEST_XMSO, TEST_HTM, 1) == 1);

  //not much we can do in this test, except read in the html file and validate it
  htmlDocPtr ptr = htmlReadFile(TEST_HTM, NULL, HTML_PARSE_PEDANTIC);
  g_assert(ptr != NULL);
  xmlFreeDoc(ptr);

  g_assert(xmi_xmso_to_htm_xslt(TEST_XMSO, TEST_HTM, 0) == 1);
  ptr = htmlReadFile(TEST_HTM, NULL, HTML_PARSE_PEDANTIC);
  g_assert(ptr != NULL);
  xmlFreeDoc(ptr);

  unlink(TEST_HTM);

  return 0;
}
