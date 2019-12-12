/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "xmimsim-gui.h"

static gboolean delete_event( GtkWidget *widget,
                              GdkEvent  *event,
                              gpointer   data ) {
	return FALSE;
}

static void destroy( GtkWidget *widget,
                     gpointer   data ) {
    gtk_main_quit ();
}

static void print_xdata(XmiMsimGuiXmsiSelectionXPathData *data, gpointer unused) {
	fprintf(stdout, "xpath: %s -> %u\n", xmi_msim_gui_xmsi_selection_xpath_data_get_string(data), xmi_msim_gui_xmsi_selection_xpath_data_get_flags(data));
}

static void xpath_changed_cb(XmiMsimGuiXmsiSelectionScrolledWindow *sw, GParamSpec *pspec, gpointer user_data) {
	fprintf(stdout, "Calling xpath_changed_cb\n");
	
	GPtrArray *arr = xmi_msim_gui_xmsi_selection_scrolled_window_get_xpath_expressions(sw);
	if (arr) {
		g_ptr_array_ref(arr);
		g_ptr_array_foreach(arr, (GFunc) print_xdata, NULL);
	}
	else {
		fprintf(stdout, "No XPATH data found!\n");
	}
}

int main(int argc, char *argv[]) {

	gtk_init(&argc, &argv);
	xmi_msim_gui_utils_init_colors();
	xmi_xmlLoadCatalog(NULL);

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window), 1200, 1200);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect (window, "delete-event",
		      G_CALLBACK (delete_event), NULL);
	g_signal_connect (window, "destroy",
		      G_CALLBACK (destroy), NULL);
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);

	// load example file
	g_assert(argc == 2);
	GError *error = NULL;
	xmi_input *input = xmi_input_read_from_xml_file(argv[1], &error);
	if (!input) {
		fprintf(stderr, "error message: %s\n", error->message);
		return 1;
	}
	GtkWidget *sw = xmi_msim_gui_xmsi_selection_scrolled_window_new(input, FALSE);
	g_signal_connect(sw, "notify::xpath-expressions", G_CALLBACK(xpath_changed_cb), NULL);
	gtk_widget_show(sw);
	gtk_container_add(GTK_CONTAINER(window), sw);
	gtk_widget_show(window);

	gtk_main();

	xmi_input_free(input);

	return 0;
}
