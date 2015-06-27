#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include "xmi_aux.h"
#include <glib.h>
#include <math.h>
#include <unistd.h>

//this test should only be run if the GUI is going to get built!
#include <gdk-pixbuf/gdk-pixbuf.h>

int main(int argc, char *argv[]) {

	//init test
	g_assert(test_init() == 1);

	//check if gdk-pixbuf has svg support
  GSList *list = gdk_pixbuf_get_formats();
	GSList *current = list;
	gboolean svg_found = FALSE;
  do {
  	GdkPixbufFormat *format = (GdkPixbufFormat *) current->data;
		//fprintf(stdout, "format: %s\n", gdk_pixbuf_format_get_name(format));
		if (strcmp("svg", gdk_pixbuf_format_get_name(format)) == 0) {
			svg_found = TRUE;
			break;
		}
  } while((current = g_slist_next(current)) != NULL);
	g_slist_free(list);

  if (!svg_found)
		return 77; //skip test

	//we found it
  //file should be present already
  g_assert(xmi_xmso_to_svg_xslt(TEST_XMSO, TEST_SVG, 1) == 1);

  //try reading in the svg file
	GdkPixbuf *pixbuf = gdk_pixbuf_new_from_file(TEST_SVG, NULL);
	if (!pixbuf)
		return 1;
  g_object_unref(pixbuf);
	unlink(TEST_SVG);

  //now unconvoluted
  g_assert(xmi_xmso_to_svg_xslt(TEST_XMSO, TEST_SVG, 0) == 1);

  //try reading in the svg file
	pixbuf = gdk_pixbuf_new_from_file(TEST_SVG, NULL);
	if (!pixbuf)
		return 1;
  g_object_unref(pixbuf);
	unlink(TEST_SVG);

  return 0;
}
