#include "xmimsim-gui-source-abstract.h"
#include "xmimsim-gui-source-module.h"
#include <gmodule.h>
#include "xmi_xml.h"

#include <stdio.h>
#include <string.h>

typedef GtkWidget* (*XmiMsimGuiSourceConstructor) (struct xmi_input *current);

static gboolean delete_event( GtkWidget *widget,
                              GdkEvent  *event,
                              gpointer   data ) {

	return FALSE;
}

static void destroy( GtkWidget *widget,
                     gpointer   data )
{
    gtk_main_quit ();
}

int main(int argc, char *argv[]) {

	gtk_init(&argc, &argv);

	if (argc != 3) {
		fprintf(stderr, "No filename provided!\n");
		return 1;
	}

	if (g_module_supported() == FALSE) {
		fprintf(stderr, "GModule is not supported on this platform. Test cannot continue\n");
		return 1;
	}

	XmiMsimGuiSourceModule *module = xmi_msim_gui_source_module_new(argv[1]);
	
	if (module == NULL || g_type_module_use(G_TYPE_MODULE(module)) == FALSE) {
		return 1;
	}

	// get kids
	guint ntypes;
	GType *source_types = g_type_children(XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT, &ntypes);

	if (ntypes == 0) {
		fprintf(stderr, "No sources were loaded!!!\n");
		return 1;
	}
	else {
		fprintf(stdout, "%u types found!\n", ntypes);
	}

	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}

	struct xmi_input *current = NULL;
	if (xmi_read_input_xml(argv[2], &current) == 0) {
		fprintf(stderr, "Could not read in XMSI file %s\n", argv[2]);
		return 1;
	}

	GtkWidget *source = g_object_new(source_types[0], 
		"xmi-input-current",
		current,
		NULL);

	if (XMI_MSIM_GUI_IS_SOURCE_ABSTRACT(source) == FALSE) {
		fprintf(stderr, "source is not instance of XmiMsimGuiAbstractSource!!!\n");
		return 1;
	}

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), xmi_msim_gui_source_abstract_get_name(XMI_MSIM_GUI_SOURCE_ABSTRACT(source)));
	gtk_container_set_border_width(GTK_CONTAINER(window), 10);
	gtk_container_add(GTK_CONTAINER(window), source);
	g_signal_connect(window, "delete-event", G_CALLBACK(delete_event), NULL);
	g_signal_connect (window, "destroy", G_CALLBACK(destroy), NULL);
	gtk_widget_show(source);
	gtk_widget_show(window);
	
	gtk_main();
	

	return 0;	
}
