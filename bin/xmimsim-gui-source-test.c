#include "xmimsim-gui-source-abstract.h"
#include <gmodule.h>

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

	if (argc != 2) {
		fprintf(stderr, "No filename provided!\n");
		return 1;
	}

	if (g_module_supported() == FALSE) {
		fprintf(stderr, "GModule is not supported on this platform. Test cannot continue\n");
		return 1;
	}

	char *class_name_short;

	GError *error = NULL;
	GRegex *regex = g_regex_new("xmimsim-gui-source-(.+)." G_MODULE_SUFFIX, 0, 0, &error);
	if (regex == NULL) {
		fprintf(stderr, "regex compile error: %s\n", error->message);
		return 1;
	}
	GMatchInfo *match_info;
	if (g_regex_match(regex, argv[1], 0, &match_info) == FALSE) {
		fprintf(stderr, "regex: no match\n");
		return 1;
	}
	
//	while (g_match_info_matches(match_info)) {
		class_name_short = g_match_info_fetch (match_info, 1);
		g_print ("Found: %s\n", class_name_short);
//		g_free (word);
//		g_match_info_next (match_info, &error);
//	}
	GModule *module = g_module_open(argv[1], G_MODULE_BIND_LOCAL);

	// replace dashes with underscores
	int i;
	for (i = 0 ; i < strlen(class_name_short) ; i++) {
		if (class_name_short[i] == '-')
			class_name_short[i] = '_';
	}
	
	gchar *constructor_symbol_name = g_strdup_printf("xmi_msim_gui_source_%s_new", class_name_short);

	XmiMsimGuiSourceConstructor constructor;

	if (g_module_symbol(module, constructor_symbol_name, (gpointer *) &constructor) == FALSE) {
		fprintf(stderr, "Could not get symbol %s in %s!\n", constructor_symbol_name, argv[1]);
		return 1;
	}

	GtkWidget *source = constructor(NULL);

	if (XMI_MSIM_GUI_IS_SOURCE_ABSTRACT(source) == FALSE) {
		fprintf(stderr, "source is not instance of XmiMsimGuiAbstractSource!!!\n");
		return 1;
	}

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_container_set_border_width(GTK_CONTAINER(window), 10);
	gtk_container_add(GTK_CONTAINER(window), source);
	g_signal_connect(window, "delete-event", G_CALLBACK(delete_event), NULL);
	g_signal_connect (window, "destroy", G_CALLBACK(destroy), NULL);
	gtk_widget_show(source);
	gtk_widget_show(window);
	
	gtk_main();
	

	return 0;	
}
